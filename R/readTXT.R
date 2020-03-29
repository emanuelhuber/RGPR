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
  if(desc != "") y@desc <- desc
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
      # A <- read.table(textConnection(gsub(pp$sep, ";", content)), 
      #                 header = pp$header, 
      #                 skip   = pp$skip + 1,
      #                 sep = ";") #, 
      #                 # sep    = pp$sep)
      A <- read.table(textConnection(content),
                      header = pp$header, 
                      skip   = pp$skip + 1, 
                      sep    = pp$sep)

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
  X <- read.table(textConnection(content), 
                  header = pp$header, 
                  skip   = pp$skip,
                  sep    = pp$sep) #, 
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


