
# RULE
#  - warnings & messages suppressed in functions other than 'GPRsurvey()'



#------------------------------------------#
#----------- CLASS DEFINITION -------------#
setClass(
  Class = "GPRsurvey",  
  slots = c(
    version       = "character",     # version of the class
    filepaths     = "character",     # filepath of the GPR data
    names         = "character",      # names of the GPR profiles
    descriptions  = "character",  # descriptions of the GPR profiles
    freqs         = "numeric",       # frequencies of the GPR profiles
    lengths       = "numeric",      # length in metres of the GPR profiles = [n]
    surveymodes   = "character",  # survey mode (reflection/CMP)
    dates         = "character",      # dates  of the GPR profiles
    antseps       = "numeric",      # antenna separation of the GPR profiles
    posunits     = "character",     # position units  !!!length = 1!!!
    crs           = "character",      # coordinates reference system
    coordref      = "numeric",    # reference position
    coords        = "list",      # (x,y,z) coordinates for each profiles
    intersections = "list",      # (x,y) position of the profile intersections
    fids          = "list",      # fiducials of the GPR profiles
    ntraces       = "integer",   # to control if nrow(@coord) == ncol(x[[i]])
    nz            = "integer",
    dz            = "numeric",     # depth/time window (vertical)
    zunits        = "character"   # time/depth unit  !!!length = 1!!!
  )
)

#------------------------------------------#
#-------------- CONSTRUCTOR ---------------#
#' Create an object of the class GPRsurvey
#'
#' Create an object of the class GPRsurvey using a vector of GPR data filepath
#' @name GPRsurvey
#' @export
# LINES = list of datapath
GPRsurvey <- function(LINES, verbose = TRUE){
  n <- length(LINES)
  line_names <- character(n)
  line_descriptions <- character(n)
  line_surveymodes <- character(n)
  line_dates <- character(n)
  line_freq <- numeric(n)
  line_antsep <- numeric(n)
  line_lengths <- numeric(n)
  line_posunits <- character(n)
  line_crs <- character(n)
  xyzCoords <- list()
  line_fids <- list()
  line_traces <- integer(n)
  line_nz <- integer(n)
  line_dz <- integer(n)
  line_depthunits <- character(n)
  for(i in seq_along(LINES)){
    gpr <- readGPR(LINES[[i]], verbose = verbose)
    # FIX ME!
    #  > check if name(gpr) is unique
    line_traces[i]       <- ncol(gpr)
    line_nz[i]           <- nrow(gpr)
    line_dz[i]           <- mean(diff(gpr@depth))
    line_names[i]        <- name(gpr)[1]
    if(line_names[i] == ""){
      line_names[i] <- "default_name"
    }
    if(i > 1){
      line_names[i] <- safeName(x = line_names[i], 
                                y = line_names[1:(i - 1)])
    }
    line_descriptions[i] <- description(gpr)
    line_surveymodes[i]  <- gpr@surveymode
    if(length(gpr@date) == 0){
      line_dates[i]        <- NA
    }else{
      line_dates[i]        <- gpr@date
    }
    if(length(gpr@freq) == 0){
      line_freq[i]        <- NA
    }else{
      line_freq[i]        <- gpr@freq
    }
    if(length(gpr@antsep) == 0){
      line_antsep[i]        <- NA
    }else{
      line_antsep[i]        <- gpr@antsep
    }
    line_posunits[i]        <- gpr@posunit
    line_depthunits[i]      <- gpr@depthunit  
    line_crs[i] <- ifelse(length(gpr@crs) > 0, gpr@crs[1], character(1))
    if(length(gpr@coord) > 0){
      # if(is.null(colnames(gpr@coord))){
      #   xyzCoords[[line_names[i] ]] <- gpr@coord
      # }else if(all(toupper(colnames(gpr@coord)) %in% c("E","N","Z"))){
      #   xyzCoords[[line_names[i] ]] <- gpr@coord[,c("E","N","Z")]
      # }else if(all(toupper(colnames(gpr@coord)) %in% c("X","Y","Z"))){
      #   xyzCoords[[line_names[i] ]] <- gpr@coord[,c("X","Y","Z")]
      # }else{
      #   xyzCoords[[line_names[i] ]] <- gpr@coord
      # }
      # xyzCoords[[line_names[i]]] <- as.matrix(value[[i]])
      xyzCoords[[line_names[i]]] <- gpr@coord
      colnames(xyzCoords[[line_names[i]]]) <- c("x", "y", "z")
      line_lengths[i] <- posLine(gpr@coord[, 1:2], last = TRUE)
    }else{
      line_lengths[i] <- gpr@dx * ncol(gpr@data)
    }
    line_fids[[line_names[i] ]] <- trimStr(gpr@fid)
  }
  if( length(unique(line_posunits)) > 1 ){
    warning("Position units are not identical: \n",
            paste0(unique(line_posunits), collaspe = ", "), "!")
  }
  if(length(unique(line_depthunits)) > 1){
    warning("Depth units are not identical: \n",
            paste0(unique(line_depthunits), collaspe = ", "), "!\n")
  }
  if(length(unique(line_crs)) > 1){
    warning("Not all the coordinate reference systems are identica: \n",
            paste0(unique(line_crs), collaspe = ", "), "!\n")
  }
  x <- new("GPRsurvey",
        version       = "0.1",
        filepaths     = LINES,       # vector of [n] file names
        names         = line_names,      # length = [n]
        descriptions  = line_descriptions,  # length = [n]
        freqs         = line_freq,       # length = [n]
        lengths       = line_lengths,       # length = [n]
        surveymodes   = line_surveymodes,    # length = [n]
        dates         = line_dates,      # length = [n]
        antseps       = line_antsep,      # length = [n]
        posunits      = line_posunits,    # length = 1
        crs           = line_crs,      # length = 1
        coords        = xyzCoords,    # header
        fids          = line_fids,
        intersections = list(),
        ntraces       = line_traces,   # to control if nrow(@coord) == ncol(x[[i]])
        nz            = line_nz,
        dz            = line_dz,     # depth/time window (vertical)
        zunits        = line_depthunits   # time/depth unit
  )
  x <- coordref(x)
  return(x)
}


#' Create empty GPR object
#' 
#' Create empty GPR object of length n
#' 
#' @export
GPRsurveyEmpty <- function(n = 1){
  n <- as.integer(round(n))
  if(n < 1) stop("'n' must be strictly positiv!")
  new("GPRsurvey",
      version       = "0.1",
      filepaths     = character(n),       # vector of [n] file names
      names         = character(n),      # length = [n]
      descriptions  = character(n),  # length = [n]
      freqs         = numeric(n),       # length = [n]
      lengths       = numeric(n),       # length = [n]
      surveymodes   = character(n),    # length = [n]
      dates         = character(n),      # length = [n]
      antseps       = numeric(n),      # length = [n]
      posunits      = character(n),    # length = 1
      crs           = character(n),      # length = 1
      coordref      = numeric(1),
      coords        = list(),   # vector(mode = "list", length = n),    # header
      fids          = list(),   # vector(mode = "list", length = n),
      intersections = list(),   # vector(mode = "list", length = n),
      ntraces       = integer(n),   # to control if nrow(@coord) == ncol(x[[i]])
      nz            = integer(n),
      dz            = numeric(n),     # depth/time window (vertical)
      zunits        = character(n)   # time/depth unit
  )
}

  
#' @export
setAs(from = "GPRsurvey", to = "SpatialLines",
      def = function (from) as.SpatialLines(from))
      
#' @export
setAs(from = "GPRsurvey", to = "SpatialPoints",
      def = function (from) as.SpatialPoints(from))    




.getCheckedCRS <- function(x){
  if(length(x@crs) == 0 || all(x@crs == "")){
    warning("no CRS defined!\n")
  }else{
    if(length(unique(x@crs)) > 1){
      warning( "Not all the coordinate reference systems are identical: \n",
            paste0(unique(x@crs), collaspe = ", "), "!\n") 
    } 
  }
  return( sp::CRS(x@crs[1]) )
}

#' Coerce to SpatialLines
#'
#' @name GPRsurvey.as.SpatialLines
#' @rdname GPRsurveycoercion
#' @export
setMethod("as.SpatialLines", signature(x = "GPRsurvey"), function(x){
  # remove NULL from list
  # FIXME
  # Filter(Negate(is.null), x) = alternative
  isNotNull <- !sapply(x@coords, is.null)
  if(any(isNotNull)){
    xyz <- x@coords[isNotNull]
    lineList <- lapply(unname(xyz), xyToLine)
    linesList <- lapply(seq_along(lineList), LineToLines, lineList, 
                        names(xyz))
    mySpatLines <- sp::SpatialLines(linesList)
    
    sp::proj4string(mySpatLines) <- .getCheckedCRS(x)
    
    return(mySpatLines)
  }else{
    warning("no coordinates!")
    return(NULL)   
  }
})



#' Coerce to SpatialPoints
#'
#' @name GPRsurvey.as.SpatialPoints
#' @rdname GPRsurveycoercion
#' @export
setMethod("as.SpatialPoints", signature(x = "GPRsurvey"), function(x){
  allTopo <- do.call(rbind, x@coords)  #  N, E, Z
  allTopo2 <- as.data.frame(allTopo)
  names(allTopo2) <- c("x", "y", "z")
  sp::coordinates(allTopo2) <- ~ x + y
  
  sp::proj4string(allTopo2) <- .getCheckedCRS(x)
  
  return(allTopo2)
})

#' Define a local reference coordinate
#' 
#' @rdname coordref-methods
#' @aliases coordref,GPRsurvey-method
setMethod("coordref", "GPRsurvey", function(x){
  if(length(x@coords) > 0 && all(sapply(x@coords, length) > 0 )){
    xcoords <- Filter(Negate(is.null), x@coords)
      A <- do.call("rbind", x@coords)
      A <- apply(round(A),2,range)
      Evalue <- .minCommon10(A[1,1],A[2,1])
      Nvalue <- .minCommon10(A[1,2],A[2,2])
      Zvalue <- 0
      x@coordref <- c(Evalue, Nvalue,Zvalue)
      cat("Coordinates of the local system:", x@coordref,"\n")
      x <- surveyIntersect(x)
    }
    return(x)
  }
)

setReplaceMethod(
  f="coordref",
  signature="GPRsurvey",
  definition=function(x,value){
    x@coordref <- value
    return(x)
  }
)

#' @name crs
#' @rdname crs
#' @export
setMethod("crs", "GPRsurvey", function(x){
    return(.getCheckedCRS(x))
  } 
)


#' @name crs<-
#' @rdname crs
#' @export
setReplaceMethod(
  f="crs",
  signature="GPRsurvey",
  definition=function(x,value){
    value <- as.character(value)
    # return a warning if number of items to replace is not a multiple of 
    # replacement length
    x@crs[] <- value
    return(x)
  }
)






#------------------------------
# "["
#' extract parts of GPRsurvey
#'
#' Return an object of class GPRsurvey
#' @name GPRsurvey-subset
#' @docType methods
#' @rdname GPRsurvey-subset
setMethod(
  f= "[",
  signature="GPRsurvey",
  definition=function(x,i,j,drop){
    if(missing(i)) i <- j
    # cat(typeof(i),"\n")
    # cat(j,"\n")
    # i <- as.numeric(i)
    y <- x
    y@filepaths      <- x@filepaths[i]
    y@names          <- x@names[i]
    y@descriptions   <- x@descriptions[i]
    y@freqs          <- x@freqs[i]
    y@lengths        <- x@lengths[i]
    y@surveymodes    <- x@surveymodes[i]
    y@dates          <- x@dates[i]
    y@antseps        <- x@antseps[i]
    y@coords         <- x@coords[x@names[i]]
    y@fids           <- x@fids[x@names[i]]
    y@intersections  <- x@intersections[x@names[i]]
    y@ntraces        <- x@ntraces[i]
    y@nz             <- x@nz[i]
    y@dz             <- x@dz[i]
    y@zunits         <- x@zunits[i]
    y@posunits       <- x@posunits[i]
    # FIXME
    if(length(x@crs) == 1){
      y@crs <- x@crs
    }else{
      y@crs <- x@crs[i]
    }
    return(y)
  }
)

#------------------------------

# "[["
# return an instance of the class GPR!
# identical to getGPR
# i can be either the gpr data number or the gpr data name

#' extract a GPR object from a GPRsurvey object
#'
#' Return an object of class GPR
#' @name [[
#' @docType methods
#' @rdname GPRsurvey-subsubset
setMethod(
  f= "[[",
  signature="GPRsurvey",
  definition=function (x, i, j, ...){
    if(missing(i)) i <- j
    return(getGPR(x, id = i))
  }
)
    
#-------------------------------
# CHECKME: check posunit!!
# CHECKME: i = new index!!
#' @rdname GPRsurvey-subsubset
setReplaceMethod(
  f = "[[",
  signature = "GPRsurvey",
  definition = function(x, i, value){
    if(class(value) != "GPR"){
      stop("'value' must be of class 'GPR'!")
    }
    if(missing(i)){
      stop("missing index")  
    }
    i <- as.integer(i[1])
    oldName <- x@names[i]
    if(oldName == ""){
      oldName <- i
    }
    newName <- value@name
    if(newName == ""){
      newName <- "default_name"
    }
    newName <- safeName(x = newName, y = x@names[-i])
    # ng <- x@names[-i]
    # it <- 1
    # while(newName %in% ng){
    #   newName <- paste0(value@name, "_", it)
    #   it <- it + 1
    # }
    #tmpf <- tempfile(newName)
    value@name <- newName
    #writeGPR(value, type = "rds", overwrite = FALSE,
    #       fPath = tmpf)
    x@names[i] <- newName
    # x@filepaths[[i]] <- paste0(tmpf, ".rds")
    x@filepaths[[i]] <- .saveTempFile(value)
    x@descriptions[i] <- value@description
    x@freqs[i] <- value@freq
    if(length(value@coord) > 0){
      x@lengths[i] <- posLine(value@coord[,1:2], last = TRUE)
    }else{
      x@lengths[i] <- abs(value@pos[length(value@pos)] - value@pos[1])
    }
    x@surveymodes[i] <- value@surveymode
    x@dates[i] <-  value@date
    x@antseps[i] <- value@antsep
    if(length(value@crs) > 0 && value@crs != ""){
      x@crs[i] <- value@crs
    }
    # if value has coordinates, update x
    if(length(value@coord) > 0){
      if(nrow(value@coord) != ncol(value) && ncol(value@coord) != 3){
        stop('coordinates not correct...')
      }
      if(length(x@coords) > 0){
        x@coords[[oldName]] <- value@coord
        names(x@coords)[i] <- newName
      }else{
        x@coords <- vector(mode = "list", length = length(x))
        x@coords[[i]] <- value@coord
        names(x@coords) <- x@names
      }
    }else{
      if(all(sapply(x@coords, length) == 0)){
        x@coords <- list()
      }
    }
    # if(length(x@coords) > 0){
    #   x@coords[[oldName]] <- value@coord
    #   names(x@coords)[i] <- newName
    # }else if(length(value@coord) > 0){
    #   x@coords <- vector(mode = "list", length = length(x))
    #   x@coords[[i]] <- value@coord
    #   names(x@coords) <- x@names
    # }
    if(length(x@fids) > 0){
      x@fids[[oldName]] <- value@fid
      names(x@fids)[i] <- newName
    }else if(length(value@fid) > 0){
      x@fids <- vector(mode = "list", length = length(x))
      x@fids[[i]] <- value@fid
      names(x@fids) <- x@names
    }
    x@intersections <- list()
    x@ntraces[i]        <- ncol(value)
    x@nz[i]             <- nrow(value)
    x@dz[i]             <- mean(diff(value@depth))
    x@zunits[i]         <- value@depthunit
    x@posunits[i]       <- value@posunit
    x <- coordref(x)
    return (x)
  }
)

#' Extract GPR object from GPRsurvey object
#' 
#' Extract GPR object from GPRsurvey object
#' @rdname getGPR
#' @export
setMethod("getGPR", "GPRsurvey", function(x,id){
    if(length(id)>1){
      warning("Length of id > 1, I take only the first element!\n")
      id <- id[1]
    }
    if(is.numeric(id)){
      no <- id
      gpr <- readGPR(x@filepaths[[id]])
    }else if(is.character(id)){
      no <- which(x@names == trimStr(id))
      if(length(no > 0)){
        gpr <- readGPR(x@filepaths[[no]])
      }else{
        stop("There is no GPR data with the name '", trimStr(id),"'\n")
      }
    }
    if(length(x@coords[[gpr@name]])>0){
      gpr@coord <- x@coords[[gpr@name]]
    }
    if(length(x@intersections[[gpr@name]])>0){
      #ann(gpr) <- x@intersections[[gpr@name]][,3:4,drop=FALSE]
      ann(gpr) <- cbind(x@intersections[[gpr@name]]$trace,
                        x@intersections[[gpr@name]]$name)
    }
    # FIXME
    if(length(x@crs) == 1){
      gpr@crs <- x@crs
    }else{
      gpr@crs <- x@crs[no]
    }
    if(length(x@coordref)>0){
      gpr@coordref <- x@coordref
    }
    return(gpr)
  }
)



#-------------------------------------------#
#---------------- SETMETHOD ----------------#
#' Print GPR survey
#'
#' @method print GPRsurvey
#' @name print
#' @rdname show
# > 2. S3 function:
# setMethod("print", "GPR", function(x) print.GPR(x))   
# > 2. S3 function:
print.GPRsurvey <- function(x, ...){
  cat("*** Class GPRsurvey ***\n")
  n <- length(x)
  dirNames <- dirname(x@filepaths)
  if(length(unique(dirNames))==1){
    cat("Unique directory:", dirNames[1],"\n")
  }else{
    cat("One directory among others:", dirNames[1],"\n")
  }
  testCoords <- rep(0, n)
  names(testCoords) <- x@names
  if(length(x@coords) > 0){
    testLength <- sapply(x@coords, length)
    testCoords[names(testLength)] <- testLength
  }
  testCoords <- as.numeric(testCoords > 0) + 1
  testIntersecs <- rep(0,n)
  names(testIntersecs) <- x@names
  if(length(x@intersections)>0){
    testLength <- sapply(x@intersections,length)
    testIntersecs[names(testLength)] <- testLength
  }
  testIntersecs <- as.numeric(testIntersecs > 0)+1
  
  is_test <- c("NO","YES")
  cat("- - - - - - - - - - - - - - -\n")
  #overview <- data.frame("name" = .fNameWExt(x@filepaths),
  overview <- data.frame("name"    = x@names,
                        # "length"   = round(x@lengths,2),
                        "length"   = formatC(signif(x@lengths, digits = 4), digits = 2, format = "fg", flag = "#"),
                        "units"    = x@posunits,
                        "date"     = x@dates,
                        "freq"     = x@freqs,
                        "coord"    = is_test[testCoords],
                        "int"      = is_test[testIntersecs],
                        "filename" = basename(x@filepaths))
  print(overview)
  if(length(x@coords)>0 ){
    cat("- - - - - - - - - - - - - - -\n")
    if(length(x@crs) > 0 ){
      if(length(unique(x@crs)) == 1){
        cat("Coordinate system:", x@crs,"\n")
      }else{
        cat("Coordinate systems:\n", paste0(x@crs, collapse = "\n"))
      }
    }else{
      cat("Coordinate system: undefined\n")
    }
    cat
  }
  cat("****************\n")
  return(invisible(overview))
}

#' Show some information on the GPR object
#'
#' Identical to print().
#' @name show
#' @rdname show
# > 3. And finally a call to setMethod():
setMethod("show", "GPRsurvey", function(object){print.GPRsurvey(object)}) 


# setMethod("length", "GPRsurvey", function(x) ncol(x@data))

#' @export
setMethod(f="length", signature="GPRsurvey", definition=function(x){
    length(x@filepaths)
  }
)



# intersection
# list
#     $GPR_NAME
#         $ coords (x,y)
#         $ trace
#         $ name

#' Compute the survey intersections
#' 
#' Compute the survey intersections
#' @rdname surveyIntersect
#' @export
setMethod("surveyIntersect", "GPRsurvey", function(x){
  # intersections <- list()
  for(i in seq_along(x@coords)){
    if(!is.null(x@coords[[i]])){
      top0 <- x@coords[[i]]
      Sa <- verboseF(as.SpatialLines(x[i]), verbose = FALSE)
      v <- seq_along(x@coords)[-i]
      int_coords <- c()
      int_traces <- c()
      int_names <- c()
      for(j in seq_along(v)){
        if(!is.null(x@coords[[v[j]]])){
          top1 <- x@coords[[v[j]]]
          Sb <- verboseF(as.SpatialLines(x[v[j]]), verbose = FALSE)
          pt_int <- rgeos::gIntersection(Sa,Sb)
          if(!is.null(pt_int) && class(pt_int) == "SpatialPoints"){
            # for each intersection points
            for(k in seq_along(pt_int)){
              d <- sqrt(rowSums((top0[,1:2] - 
                              matrix(sp::coordinates(pt_int)[k,],
                              nrow = nrow(top0), ncol = 2, byrow = TRUE))^2))
              int_coords <- rbind(int_coords, sp::coordinates(pt_int)[k,])
              int_traces <- c(int_traces, which.min(d)[1])
              int_names  <- c(int_names, x@names[v[j]])
            }
          }
        }
      }
      if(length(int_names) > 0){
        x@intersections[[x@names[i]]] <- list(coord = int_coords,
                                              trace = int_traces,
                                              name  = int_names)
      }else{
        x@intersections[[x@names[i]]] <- NULL
      }
    }
  }
  return(x)
})

#' Return intersection from GPRsurvey
#'
#' @rdname intersections-methods
#' @aliases intersections,GPRsurvey-method
setMethod("intersections", "GPRsurvey", function(x){
    return(x@intersections)
  }
)

           
#' @export
setMethod("trRmDuplicates", "GPRsurvey", function(x, tol = NULL){
  nrm <- integer(length(x))
  for(i in seq_along(x)){
    y <- verboseF(x[[i]], verbose = FALSE)
    n0 <- ncol(y)
    y <- verboseF(trRmDuplicates(y), verbose = FALSE)
    if( (n0 - ncol(y)) > 0){
      message(n0 - ncol(y), " duplicated trace(s) removed from '", name(y), "'!")
      x@filepaths[[i]]     <- .saveTempFile(y)
      x@coords[[y@name]]   <- y@coord
      x@fids[[y@name]]     <- y@fid
    }
  }
  x@intersections <- list()
  x <- coordref(x)
  return(x) 
})

#' @export
setMethod("interpPos", "GPRsurvey",
          function(x, topo, plot = FALSE, r = NULL, tol = NULL, 
                   method = c("linear", "linear", "linear"), crs = NULL,
                   ...){
    for(i in seq_along(x)){
      gpr <- readGPR(x@filepaths[[i]])
      # topoLine <- topo[[i]]
      # gpr <- interpPos(gpr,topoLine, ...)
      gpr <- interpPos(gpr, topo[[i]], plot = plot, r = r, tol = tol, 
                       method = method, ...)
      x@coords[[gpr@name]] <- gpr@coord
      x@lengths[i] <- posLine(gpr@coord[ ,1:2], last = TRUE)
    }
    x@intersections <- list()
    x <- coordref(x)
    return(x)
  }
)


#' Reverse the trace position.
#'
#' @name reverse
#' @rdname reverse
#' @export
setMethod("reverse", "GPRsurvey", function(x, id = NULL, tol = 0.3){
  if(is.null(id) && length(x@coords) > 0){
    # reverse radargram based on their name 
    # (all the XLINE have the same orientation, 
    # all the YLINE have the same orientation)
    lnTypes <- gsub("[0-9]*$", "", basename(x@names))
    lnTypeUniq <- unique(lnTypes)
    angRef <- rep(NA, length = length(lnTypeUniq))
    # revTRUE <- rep(FALSE, length = length(x))
    for(i in seq_along(x)){
      y <- verboseF( x[[i]], verbose = FALSE )
      typeNo <- which(lnTypeUniq %in% lnTypes[[i]] )
      if(is.na(angRef[typeNo])){
        angRef[typeNo] <- gprAngle(y)
      }else{
        angi <- gprAngle(y) 
        if(!isTRUE(inBetAngle( angRef[typeNo], angi, atol = tol))){
          y <- reverse(y)
          # revTRUE[i] <- TRUE
          message(y@name, " > reverse!")
          # tmpf <- tempfile(y@name)
          # writeGPR(y, type = "rds", overwrite = FALSE, fPath = tmpf)
          # x@filepaths[[i]]     <- paste0(tmpf, ".rds")
          x@filepaths[[i]]     <- .saveTempFile(y)
          x@coords[[y@name]]   <- y@coord
          x@fids[[y@name]]      <- y@fid
        }
      }
    }
    x@intersections <- list()
    x <- coordref(x)
    return(x)
  }
  if (is.null(id) || (is.character(id) && id == "zigzag")){
    if(length(x) > 1){
      id <- seq(from = 2L, by = 2L, to = length(x))
    }
  } 
  if(is.numeric(id)){
    id <- as.integer(id)
    if(max(id) <= length(x) && min(id) >= 1){
      for(i in seq_along(id)){
        y <- verboseF(getGPR(x, id = id[i]), verbose = FALSE)
        y <- reverse(y)
        x@filepaths[[id[i]]]     <- .saveTempFile(y)
        if(length(y@coord) > 0){
          # x@coords[[y@name]]   <- y@coord
          x@coords[[id[i]]]   <- y@coord
        }
        # x@fids[[y@name]]      <- y@fid
        x@fids[[id[i]]]      <- y@fid
      }
      x@intersections <- list()
      x <- coordref(x)
      return(x) 
    }else{
      stop("id must be between 1 and ", length(x),"!")
    }
  }
  # if is.character(id) (<- name of data)
})


# value = x, y, dx, dy

#' Set grid coordinates the trace position.
#'
#' Set grid coordinates to a survey
#' @param x An object of the class GPRsurvey
#' @param value A list with following elements: \code{xlines} (number or id of 
#'              the GPR data along the x-coordinates), \code{ylines} (number or 
#'              id of the GPR data along the y-coordinates), \code{xpos} 
#'              (position of the x-GPR data on the x-axis),
#'              \code{xpos} (position of the y-GPR data on the y-axis)
#' @rdname setGridCoord-methods
#' @export
setReplaceMethod(
  f = "setGridCoord",
  signature = "GPRsurvey",
  definition = function(x, value){
    value$xlines <- unique(value$xlines)
    value$ylines <- unique(value$ylines)
    if( any(value$xlines %in% value$ylines) ){
      stop("No duplicates between 'x' and 'y' allowed!")
    }
    
    if(length(value$xlines) != length(value$xpos)){
      stop("length(x) must be equal to length(dx)")
    }
    if(length(value$ylines) != length(value$ypos)){
      stop("length(y) must be equal to length(dy)")
    }
    if(!is.null(value$xlines)){
      if(is.numeric(value$xlines)){
        if(max(value$xlines) > length(x) || 
           min(value$xlines) < 1){
          stop("Length of 'xlines' must be between 1 and ", length(x))
        }
        xNames <- x@names[value$xlines]
      }else if(is.character(value$xlines)){
        if(!all(value$xlines %in% x@names) ){
          stop("These names do not exist in the GPRsurvey object:\n",
               value$xlines[! (value$xlines %in% x@names) ])
        }
        xNames <- value$xlines
      }
      for(i in seq_along(xNames)){
        y <- verboseF( getGPR(x, xNames[i]), verbose = FALSE )
        ntr <- ncol(y)
        x@coords[[xNames[i]]] <- matrix(0, nrow = ntr, ncol = 3)
        x@coords[[xNames[i]]][,1] <- value$xpos[i]
        x@coords[[xNames[i]]][,2] <- y@pos
      }
    }
    if(!is.null(value$ylines)){
      if(is.numeric(value$ylines)){
        if(max(value$ylines) > length(x) || 
           min(value$ylines) < 1){
          stop("Length of 'ylines' must be between 1 and ", length(x))
        }
        yNames <- x@names[value$ylines]
      }else if(is.character(value$ylines)){
        if(!all(value$ylines %in% x@names) ){
          stop("These names do not exist in the GPRsurvey object:\n",
               value$ylines[! (value$ylines %in% x@names) ])
        }
        xyNames <- value$ylines
      }
      for(i in seq_along(yNames)){
        y <- verboseF( getGPR(x, xNames[i]), verbose = FALSE)
        ntr <- ncol(y)
        x@coords[[yNames[i]]] <- matrix(0, nrow = ntr, ncol = 3)
        x@coords[[yNames[i]]][,1] <- y@pos
        x@coords[[yNames[i]]][,2] <- value$ypos[i]
      }
    }
    return(x)
  }
)


#' Return coordinates
#'
#' Return coordinates
#' @rdname coords-methods
#' @aliases coords,GPRsurvey-method
setMethod(
  f="coords",
  signature="GPRsurvey",
  definition=function(x, i){
    if(length(x@coords) == 0){
      return(x@coords)
    }
    if(missing(i)){
      return(x@coords)
    }else{
      return(x@coords[[i]])
    }
  }
)

#' Set coordinates
#'
#' @rdname coords-methods
#' @aliases coords<-,GPRsurvey-method
setReplaceMethod(
  f = "coords",
  signature = "GPRsurvey",
  definition = function(x, value){
    if(!is.list(value)){
      stop("value should be a list!!\n")
    }
    if(length(value) != length(x)){
      stop("number of elements not equal to the number of gpr files!!\n")
    }
    for(i in seq_along(x)){
      if( nrow(value[[i]]) != x@ntraces[i] ){
        stop("error with the ", i, "th element of 'value':",
             " number of coordinates is different from number of traces")
      }
      if(ncol(value[[i]]) < 3){
        stop("error with the ", i, "th element of 'value':",
             " number of column is smaller than 3!")
      }else if(ncol(value[[i]]) > 3){
        warning("the ", i, "th element of 'value' has more than 3 columns:",
             " I take only the first 3 columns!")
      }
      
      x@coords[[x@names[i]]] <- as.matrix(value[[i]])
      colnames(x@coords[[x@names[i]]]) <- c("x", "y", "z")

      x@lengths[i] <- posLine(value[[i]][,1:2],last=TRUE)
    }
    # in coordref, the intersection is computed by 
    #    "x <- surveyIntersect(x)"
    # remove duplicates
    x@intersections <- list()
    x <- trRmDuplicates(x, tol = NULL)
    #x <- coordref(x)
    return(x)
  }
)
    
# Rotate coordinates of the GPR traces
#
#' @export
#' @name georef
#' @rdname georef
setMethod("georef", "GPRsurvey", 
          function(x, alpha = NULL, cloc = NULL, creg = NULL,
                   ploc = NULL, preg = NULL, FUN = mean){
            if(is.null(cloc)){
              cloc <- .centroid(x)[1:2]
            }
            if(is.null(alpha) && is.null(ploc) && is.null(preg)){
              alpha <- svAngle(x)
            }
            # here I cannot write FUN = FUN because FUN is an argument of
            # lapply...
            xyz  <- lapply(x@coords, georef, alpha = alpha, cloc = cloc,
                           creg = creg, ploc = ploc, preg = preg, FUN)
            x@coords <- xyz
            x@intersections <- list()
            x <- coordref(x)
            return(x)
          })

.centroid <- function(x){
  pos <- do.call(rbind, x@coords)
  return(colMeans(pos))
}


#' @export
setMethod("trProject", "GPRsurvey", function(x, CRSobj){
  xshp <- as(x, "SpatialLines")
  xshpc <- sp::spTransform(xshp, CRSobj)
  xshpc_coords <- sp::coordinates(xshpc)
  FUN <- function(x, y){
    x[, 1:2] <- y[[1]]
    return(x)
  }
  x@crs <- as.character(CRSobj)
  coords(x) <- mapply(FUN, coords(x), xshpc_coords, 
                      USE.NAMES = FALSE, SIMPLIFY = FALSE)
  return(x)
})

#' Oriented bounding box (2D)
#' 
#' Returns the oriented bounding box of the trace position of the survey.
#' 
#' The algorithm you are looking for is known in polygon generalisation as 
#' "smallest surrounding rectangle".
#' Compute the convex hull of the cloud.
#' For each edge of the convex hull:
#' compute the edge orientation (with arctan),
#' rotate the convex hull using this orientation in order to compute easily 
#' the bounding rectangle area with min/max of x/y of the rotated convex hull,
#' Store the orientation corresponding to the minimum area found,
#' Return the rectangle corresponding to the minimum area found.
#' In 3D, the same applies, except:
#'   The convex hull will be a volume,
#'   The orientations tested will be the orientations (in 3D) of the convex hull faces.
#' @source  source "whuber" from stackexchange.com, 
#' https://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points/181883#181883
#' @name tpOBB2D
#' @rdname tpOBB2D
#' @export
setMethod("tpOBB2D", "GPRsurvey", function(x){
  if(length(x@coords) > 0){
    xyz <- coords(x)
    xyz <- Filter(Negate(is.null), xyz)
    p <- do.call(rbind, xyz)
    return(OBB(p[,1:2]))
  }else{
    stop("x has no coordinates.")
  }
})


#' Angle of the GPR survey 
#' 
#' @name svAngle
#' @rdname svAngle
#' @export
setMethod("svAngle", "GPRsurvey", function(x){
  if(length(x@coords) > 0){
    orb <- tpOBB2D(x)
    dEN <- orb[1,] - orb[2,]
    i <- which.max(diff(posLine(orb)))[1]
    dOBB <- orb[i + 1,] - orb[i,]
    angl_OBB <- atan2(dOBB[2], dOBB[1])
    # # angl_OBB/pi * 180
    # # abs(angl_EN - angl_OBB) / pi * 180
    # if(pi * 6/5 > abs(angl_EN - angl_OBB) && abs(angl_EN - angl_OBB)  > pi* 4 /5){
    #   angl_OBB <- angl_OBB + pi
    #   if(angl_OBB > pi) angl_OBB <- angl_OBB - 2*pi
    # }
    return(angl_OBB)
  }else{
    stop("x has no coordinates.")
  }
})


#' @export
setMethod("shiftEst", "GPRsurvey", function(x, y = NULL, 
          method=c("phase", "WSSD"), dxy = NULL, ...){
  if(!is.null(dxy) && length(dxy) != 2){
    stop("dxy is either NULL or a length-two vector")
  }
  Dshift <- matrix(ncol = 2, nrow = length(x) - 1)
  y <- verboseF( x[[1]], verbose = FALSE)
  ny <- nrow(y)
  my <- ncol(y)
  i0 <- NULL
  j0 <- NULL
  if( length(list(...)) ){
    dots <- list(...)
    if( !is.null(dots$i)){
      i0 <- dots$i
    }
    if( !is.null(dots$j)){
      j0 <- dots$j
    }
  }
  for(k in seq_len(length(x)-1)){
    z <- verboseF( x[[k + 1]], verbose = FALSE)
    nz <- nrow(z)
    mz <- ncol(z)
    if(is.null(i0)){
      i <- seq_len(min(nz, ny))
    }else{
      i <- i0
    }
    if(is.null(j0)){
      j <- seq_len(min(mz, my))
    }else{
      j <- j0
    }
    Dshift[k,] <- displacement(y@data[i, j], z@data[i,j], 
                          method = "phase", dxy = dxy)
    y <- z
    ny <- nz
    my <- mz
  }

  return( Dshift )
})    


#' Shift trace positions of one GPR data
#'
#' Shift trace positions of GPR data \code{i} by \code{dx} along x-axis and
#' by \code{dy} along y-axis.
#' @name tpShift
#' @rdname tpShift
#' @export
setMethod("tpShift", "GPRsurvey", function(x, i, dx = 0, dy = 0){
  # if you want to shift the coordinates by 1 m along x-direction, 
  # 0.5 m along the y-direction
  # for your 3rd GPR data line, do that
  coords(x)[[names(x)[i]]] <- t(t(coords(x)[[names(x)[i]]]) + c(dx, dy , 0))
  return(x)
})
                                            
   




#----------------------- EXPORT/SAVE -----------------#
#' Write GPRsurvey object
#' 
#' Write GPRsurvey object
#' @name writeSurvey
#' @rdname writeSurvey
#' @export
setMethod("writeSurvey", "GPRsurvey", function(x, fPath, overwrite=FALSE){
  if(isTRUE(overwrite)){
    cat("file may be overwritten\n")
  }else{
    fPath <- safeFPath(fPath)
  }
  x@filepath <- as.character(fPath)
  namesSlot <- slotNames(x)
  xList <- list()
#   xList[["version"]] <- "0.1"
  for(i in seq_along(namesSlot)){
    xList[[namesSlot[i]]] <- slot(x, namesSlot[i])
  }
  saveRDS(xList, fPath)
#   saveRDS(x, fPath)
})









#----------------- 1D-SCALING (GAIN)
#' Gain compensation
#' 
#' @name trAmplCor
#' @rdname trAmplCor
#' @export
setMethod("trAmplCor", "GPRsurvey", function(x, 
          type = c("power", "exp", "agc"),...){
  type <- match.arg(type, c("power", "exp", "agc"))
  for(i in seq_along(x)){
    y <- verboseF( x[[i]], verbose = FALSE )
    y@data[is.na(y@data)] <-0
    if(type=="power"){
      y@data <- .gainPower(y@data, dts = y@dz, ...)
    }else if(type=="exp"){
      y@data <- .gainExp(y@data, dts = y@dz, ...)
    }else if(type=="agc"){
      y@data <- .gainAgc(y@data, dts = y@dz, ...)
    }
    proc(y) <- getArgs()
    x@filepaths[[i]] <- .saveTempFile(y)
  #   x@proc <- c(x@proc, proc)
  }
  return(x)
  } 
)


#' Apply processing to GPRsurvey object
#' 
#' @name papply
#' @rdname papply
#' @export
setMethod("papply", "GPRsurvey", function(x, prc = NULL){
  if(typeof(prc) != "list") stop("'prc' must be a list")
  for(i in seq_along(x)){
    y <- verboseF( x[[i]], verbose = FALSE )
    message('Processing ', y@name, '...', appendLF = FALSE)
    for(k in seq_along(prc)){
      y <- do.call(names(prc[k]), c(x = y,  prc[[k]]))
    }
    x@names[[i]]        <- y@name
    x@descriptions[[i]] <- y@description
    x@lengths[[i]]      <- y@dx * ncol(y@data)
    x@surveymodes[[i]]  <- y@surveymode
    x@posunits[[i]]     <- y@posunit
    if(length(x@crs) == 1){
      x@crs <- y@crs
    }else{
      x@crs[[i]]          <- y@crs
    }
    x@fids[[i]]         <- y@fid
    x@ntraces[[i]]      <- ncol(y)
    x@nz[[i]]           <- nrow(y)
    x@dz[[i]]           <- mean(diff(y@depth))
    x@zunits[[i]]       <- y@depthunit
    if(length(y@date) == 0){
      x@dates[[i]]        <- NA
    }else{
      x@dates[[i]]        <- y@date
    }
    if(length(y@freq) == 0){
      x@freqs[[i]]        <- NA
    }else{
      x@freqs[[i]]        <- y@freq
    }
    if(length(y@antsep) == 0){
      x@antseps[[i]]        <- NA
    }else{
      x@antseps[[i]]      <- y@antsep
    }
    if(length(y@coord) > 0){
      x@coords[[y@name]] <- y@coord
      x@lengths[[i]]      <- posLine(y[,1:2],last=TRUE)
    } 
    x@filepaths[[i]] <- .saveTempFile(y)
    message(' done!', appendLF = TRUE)
  }
  x@intersections <- list()
  x <- coordref(x)
  return(x)
  } 
)


#' Return TRUE is the data are a function of time
#' 
#' @name isTimeUnit
#' @rdname isTimeUnit
#' @export
setMethod("isTimeUnit", "GPRsurvey", function(x){
  grepl("[s]$", x@zunits)
} 
)

#' Return TRUE is the data are a function of length
#' 
#' @name isLengthUnit
#' @rdname isLengthUnit
#' @export
setMethod("isLengthUnit", "GPRsurvey", function(x){
  !isTimeUnit(x)
} 
)

