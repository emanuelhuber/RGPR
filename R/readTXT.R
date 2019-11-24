# require rmNaCol

.gprTXT <- function(A, fName = character(0), desc = character(0),
                    fPath = character(0), Vmax = NULL){  
  
  if(!is.null(A$depth) && !is.null(A$pos)){
    x <- list(data = bits2volt(Vmax = Vmax)*A$data,
              pos = A$pos,
              depth = A$depth,
              name = fName,
              filepath = fPath)
  }else{
    x <- list(data = bits2volt(Vmax = Vmax)*A$data)
  }
  y <- as(x, "GPR") 
  if(desc != "") description(y) <- desc
  return(y)
}



readTXT <- function(dsn){
  # fName <- getFName(dsn, ext = c(".txt"))
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  
  # detect header, column separator, skip lines, number of columns.
  pp <- detectASCIIProp(dsn)
  
  #------------------------------ Matrix file ----------------------------------#  
  if(length(pp$nCols) > 1){
    # only first row has one element less -> first row = trace position
    #                                     -> first col = trace depth
    if( length(unique(pp$nCols[-1])) == 1 && pp$nCols[1] == (pp$nCols[2] - 1) ){
      invisible(seek(dsn, where = 0, origin = "start"))
      content <- verboseF(readLines(dsn), verbose = FALSE)
      if(length(content) == 0){
        .closeFileIfNot(dsn)
        return(NULL)
      }
      # A <- read.table(file   = dsn, 
      A <- read.table(textConnection(gsub(pp$sep, "\t", content)), 
                      header = pp$header, 
                      skip   = pp$skip + 1) #, 
                      # sep    = pp$sep)

      if(pp$header == TRUE){
        pp$skip <- pp$skip + 1
      }
      invisible(seek(dsn, where = 0, origin = "start"))
      Apos <- scan(file   = dsn, 
                   sep    = pp$sep, 
                   skip   = pp$skip, 
                   nlines = 1,
                   quiet = TRUE)
      .closeFileIfNot(dsn)
      return(list(data  = as.matrix(A[,-1]), 
                  pos   = Apos, 
                  depth = A[,1]))
    }else{
      stop("Error, not same number of elements per line.")
    }
  }else{
    #message(nCols, " columns")
  }
  
  #---------------------------- 3 (or 4) column file --------------------------#
  invisible(seek(dsn, where = 0, origin = "start"))
  content <- verboseF(readLines(dsn), verbose = FALSE)
  if(length(content) == 0){
    .closeFileIfNot(dsn)
    return(NULL)
  }
  # A <- read.table(file   = dsn, 
  X <- read.table(textConnection(gsub(pp$sep, "\t", content)), 
                  header = pp$header, 
                  skip   = pp$skip) #, 
  # sep    = pp$sep)
  # X <- read.table(file   = dsn, 
  #                 header = pp$header,
  #                 skip   = pp$skip, 
  #                 sep    = pp$sep)
  
  # remove NA columns
  X <- rmNaCol(X)
  
  if(ncol(X) < 3){
    stop("The data are not correctly formated.")
  }else if(ncol(X) == 3){
    Xn <- list()
    Xn[[1]] <- unique(rle(X[,1])$lengths)
    Xn[[2]] <- unique(rle(X[,2])$lengths)
    Xn[[3]] <- unique(rle(X[,3])$lengths)
    pos <- 1:3
    
    Xamp <- which(lapply(Xn, length) > 2)
    if(length(Xamp) > 1){
      cat("Error")
    }
    
    XnTemp <- Xn
    XnTemp[[Xamp]] <- NULL
    pos <- pos[-Xamp]
    Xpos <- pos[which.max((XnTemp))]
    Xt <- pos[-Xpos]
    
    nr <- Xn[[Xpos]]
    nc <- length(unique(X[, Xpos]))
    if( nc != nrow(X)/nr){
      cat("Error")
    }
    
    A <- matrix(data  = X[, Xamp][seq_len(nc * nr)], 
                nrow  = nr, 
                ncol  = nc, 
                byrow = FALSE)
    .closeFileIfNot(dsn)
    return(list(data  = A, 
                pos   = unique(X[, Xpos]), 
                depth = unique(X[, Xt])))
    
    # }else if(ncol(X) == 4){
    #   # case xyza!!!
    #   Xn <- list()
    #   Xn[[1]] <- unique(rle(X[,1])$lengths)
    #   Xn[[2]] <- unique(rle(X[,2])$lengths)
    #   Xn[[3]] <- unique(rle(X[,3])$lengths)
    #   Xn[[4]] <- unique(rle(X[,4])$lengths)
    #   pos <- 1:x <- list(data = A$data)
    #   
    #   Xamp <- which(lapply(Xn, length) > 2)
    #   if(length(XampPos) > 1){
    #     cat("Error")
    #   }
    #   XnTemp <- Xn
    #   XnTemp[[Xamp]] <- NULL
    #   pos <- pos[-Xamp]
    #   Xpos <- pos[which.max((XnTemp))]
    #   Xt <- pos[-Xpos]
    #   
    #   nr <- Xn[[Xpos]]
    #   nc <- length(unique(X[, Xpos]))
    #   if( nc != nrow(X)/nr){
    #     cat("Error")
    #   }
    #   
    #   A <- matrix(X[, Xamp][seq_len(nc * nr)], nrow = nr, ncol = nc, byrow = FALSE)
    #   return(list(data = A, pos = unique(X[, Xpos]), depth = unique(X[, Xt])))
  }else{
    .closeFileIfNot(dsn)
    return(list(data = X))
  }
}  


#' Get properties of ASCII file to read it with \code{read.table}
#' 
#' To get header, separator, column with na values, etc.
#' 
#' don't forget to skip blank line when reading dsn
#' @param dsn   (character) File path or connection
#' @param lns     (numeric) Number of lines to read to get the properties of the ASCII file
#' @param verbose (boolean) If \code{TRUE} print messages allowed.
#' @return 1) header, 2) skip, 3) 
#' @export
detectASCIIProp <- function(dsn, lns = 20, verbose = TRUE){
  
  # if(!inherits(dsn, "connection")){
  #   dsn <- file(dsn, 'rb')
  # }
  #---------------------- read first 'lns' lines ------------------------------#
  # con <- file(dsn , "rt")
  x <- readLines(dsn, n = lns, skipNul = TRUE)
  # close(dsn)
  x <- x[ x!= ""]
  
  #------------------------------ detect header -------------------------------#
  # y <- strsplit(x, split = "[^[:alnum:]]+")
  # split at all punctuations signs except '-' and '-'
  y <- strsplit(x, split = "[^[:alnum:]\\.\\-]+") 
  test0 <- suppressWarnings(lapply(y, as.numeric))
  test <- sapply(test0, function(x) sum(is.na(x)))
  if( all(test[-1] > 0) ){
    if(length(unique(test)) == 1){
      nHeader <- 0
    }else{
      nHeader <- 1
      if(verbose){
        warning("Cannot detect header with certitude. ",
                "I assume that the first non-empty line it the header.")
      }
    }
  }else{
    nHeader <- which(test > 0)
    #message("there is ", length(nHeader), " header lines!")
  }
  
  if(length(nHeader) > 0){
    x0 <- x[-nHeader]
    header <- TRUE
    skip <- max(nHeader) - 1
  }else{
    x0 <- x
    header <- FALSE
    skip <- 0
  }
  
  #--------------------------- detect column separator ------------------------#
  sep <- unique(unlist(lapply(x0, detectSep)))
  sepName <- sep
  if(sep == "\t"){
    sepName <- "\\t"
  }
  if(length(sep) > 1){
    stop("seems that you have different column delimiters: ", sepName, "\n")
  }else{
    #message("Column delimiter is '", sepName, "'")
  }
  
  #--------------------------- number of columns ------------------------------#
  # z <- strsplit(x0, split = "[^[:alnum:]\\.\\-]+")
  z <- strsplit(x0, split = sep)
  nCols <- unique(sapply(z, length))
  
  return(list(header = header, skip = skip, sep = sep, nCols = nCols))
}

detectSep <- function(x){ 
  # i <- gregexpr("[^[:alnum:]]+", x, perl = TRUE)
  i <- gregexpr("[^[:alnum:]\\.\\-]+", x, perl = TRUE)
  sep <- unique(substring(x, i[[1]], i[[1]]))
}
