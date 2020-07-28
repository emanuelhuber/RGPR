
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Don't hesitate to contact me if you ",
                               "have any question:\n",
                               "emanuel.huber@alumni.ethz.ch"))
}

# check lockBinding  (bibliotheque/documents/R/manuel-S4)



#--------------------------------------------#
#---------------- SETGENERIC ----------------#
# setGenericVerif <- function(x,y){
#   if(!isGeneric(x)){
#     setGeneric(x,y)
#   }else{
#     cat("setGeneric", x,"already exists!\n")
#   }
# }
setGenericVerif <- function(x,y){setGeneric(x,y)}


#------------------------------

setGenericVerif("as.SpatialPoints", function(x) 
  standardGeneric("as.SpatialPoints"))

setGenericVerif("as.SpatialLines", function(x) 
  standardGeneric("as.SpatialLines"))


#' @importFrom magrittr %>%
#' @export
magrittr::'%>%'

#' @importFrom magrittr %T>%
#' @export
magrittr::'%T>%'

#' @name trPlot
#' @rdname trPlot
#' @export
setGeneric("trPlot", function(x, ...)
  standardGeneric("trPlot"))


#------------------------------
#' @name coordref
#' @rdname coordref-methods
#' @exportMethod coordref
setGenericVerif("coordref", function(x) standardGeneric("coordref"))


#' @name coordref<-
#' @rdname coordref-methods
#' @exportMethod coordref
setGenericVerif("coordref<-", function(x, value) standardGeneric("coordref<-"))

#' @name intersections
#' @rdname intersections-methods
#' @exportMethod intersections
setGenericVerif("intersections", function(x) standardGeneric("intersections"))

#' @name filepath
#' @rdname filepath-methods
#' @exportMethod filepath
setGenericVerif("filepath", function(x) standardGeneric("filepath"))

#' @name filepath<-
#' @rdname filepath-methods
#' @exportMethod filepath<-
setGenericVerif("filepath<-", function(x, value) standardGeneric("filepath<-"))

#' @name coords
#' @rdname coords-methods
#' @exportMethod coords
setGenericVerif("coords", function(x,i) standardGeneric("coords"))

#' @name coords<-
#' @rdname coords-methods
#' @exportMethod coords<-
setGenericVerif("coords<-",function(x,value){standardGeneric("coords<-")})

#' @name coord
#' @rdname coord-methods
#' @exportMethod coord
setGenericVerif("coord", function(x, i, ...) standardGeneric("coord"))

#' @name coord<-
#' @rdname coord-methods
#' @exportMethod coord<-
setGenericVerif("coord<-",function(x,value){standardGeneric("coord<-")})


#' @name svDate
#' @rdname svDate
#' @export
setGenericVerif("svDate", function(x, i, ...) standardGeneric("svDate"))

#' @name svDate<-
#' @rdname svDate
#' @export
setGenericVerif("svDate<-",function(x,value){standardGeneric("svDate<-")})



#' @name ann
#' @rdname ann
#' @export
setGenericVerif("ann", function(x) standardGeneric("ann"))

#' @name ann<-
#' @rdname ann
#' @export
setGenericVerif("ann<-",function(x,value){standardGeneric("ann<-")})



#' @name depthunit
#' @rdname depthunit
#' @export
setGenericVerif("depthunit", function(x) standardGeneric("depthunit"))

#' @name depthunit<-
#' @rdname depthunit
#' @export
setGenericVerif("depthunit<-",function(x,value){standardGeneric("depthunit<-")})

#' @name posunit
#' @rdname posunit
#' @export
setGenericVerif("posunit", function(x) standardGeneric("posunit"))

#' @name posunit<-
#' @rdname posunit
#' @export
setGenericVerif("posunit<-",function(x,value){standardGeneric("posunit<-")})

#' @name crs
#' @rdname crs
#' @export
setGenericVerif("crs", function(x) standardGeneric("crs"))

#' @name crs<-
#' @rdname crs
#' @export
setGenericVerif("crs<-",function(x,value){standardGeneric("crs<-")})

#' @name depth
#' @rdname depth
#' @export
setGenericVerif("depth", function(x) standardGeneric("depth"))

#' @name depth<-
#' @rdname depth
#' @export
setGenericVerif("depth<-", function(x,value) standardGeneric("depth<-"))

#' @name pos
#' @rdname pos
#' @export
setGenericVerif("pos", function(x) standardGeneric("pos"))

#' @name pos<-
#' @rdname pos
#' @export
setGenericVerif("pos<-", function(x, value) standardGeneric("pos<-"))

#' @name time0
#' @rdname time0
#' @export
setGenericVerif("time0", function(x) standardGeneric("time0"))

#' @name time0<-
#' @rdname time0
#' @export
setGenericVerif("time0<-",function(x, value){standardGeneric("time0<-")})

#' @name setTime0
#' @rdname time0
#' @export
setGenericVerif("setTime0", function(x, t0, track = TRUE) standardGeneric("setTime0"))



#' Time of data collection for each trace
#'
#' @name trTime
#' @rdname trTime
#' @export
setGenericVerif("trTime", function(x) standardGeneric("trTime"))

#' @name fid
#' @rdname fid
#' @export
setGenericVerif("fid", function(x) standardGeneric("fid"))

#' @name fid<-
#' @rdname fid
#' @export
setGenericVerif("fid<-",function(x,value){standardGeneric("fid<-")})

#' @name values
#' @rdname values
#' @export
setGenericVerif("values", function(x) standardGeneric("values"))

#' @name values<-
#' @rdname values
#' @export
setGenericVerif("values<-", function(x,value) standardGeneric("values<-"))

#' @name processing
#' @rdname processing
#' @export
setGenericVerif("processing", function(x) standardGeneric("processing"))

#' @name proc
#' @rdname proc
#' @export
setGenericVerif("proc", function(x) standardGeneric("proc"))


#' @name proc<-
#' @rdname proc
#' @export
setGenericVerif("proc<-",function(x,value){standardGeneric("proc<-")})

#' @name antsep
#' @rdname antsep
#' @export
setGenericVerif("antsep", function(x) standardGeneric("antsep"))

#' @name antsep<-
#' @rdname antsep
#' @export
setGenericVerif("antsep<-",function(x,value){standardGeneric("antsep<-")})                


#' @name antfreq
#' @rdname antfreq
#' @export
setGenericVerif("antfreq", function(x) standardGeneric("antfreq"))

#' @name antfreq<-
#' @rdname antfreq
#' @export
setGenericVerif("antfreq<-",function(x,value){standardGeneric("antfreq<-")})                



#' @name surveymode
#' @rdname surveymode
#' @export
setGenericVerif("surveymode", function(x) standardGeneric("surveymode"))

#' @name surveymode<-
#' @rdname surveymode
#' @export
setGenericVerif("surveymode<-",
                function(x, value){standardGeneric("surveymode<-")})    

#' @name isCMP
#' @rdname isCMP
#' @export
setGenericVerif("isCMP", function(x) standardGeneric("isCMP"))

#' @name isTimeUnit
#' @rdname isTimeUnit
#' @export
setGenericVerif("isTimeUnit", function(x) standardGeneric("isTimeUnit"))

#' @name isLengthUnit
#' @rdname isLengthUnit
#' @export
setGenericVerif("isLengthUnit", function(x) standardGeneric("isLengthUnit"))


#' @name description
#' @rdname description
#' @export
setGenericVerif("description", function(x) standardGeneric("description"))

#' @name description<-
#' @rdname description
#' @export
setGenericVerif("description<-", function(x, value) 
  standardGeneric("description<-"))


#####
setGenericVerif("papply", function(x, prc = NULL) standardGeneric("papply"))
##########


#' @name trProject
#' @rdname trProject
#' @export
setGenericVerif("trProject", function(x, CRSobj) standardGeneric("trProject"))

#' @name tpOBB2D
#' @rdname tpOBB2D
#' @export
setGenericVerif("tpOBB2D", function(x) standardGeneric("tpOBB2D"))

#' @name svAngle
#' @rdname svAngle
#' @export
setGenericVerif("svAngle", function(x) standardGeneric("svAngle"))


#------------------------------GPR
#' @name gethd
#' @rdname gethd
#' @export
setGenericVerif("gethd", function(x,hd=NULL) standardGeneric("gethd"))


setGenericVerif("plotAmpl", function(x, npad = 100, FUN = mean, add = FALSE, 
                                     all = FALSE,...) standardGeneric("plotAmpl"))





setGenericVerif("plotEnvelope", function(x, npad = 100, FUN = mean, add = FALSE, 
                                         all = FALSE,...) standardGeneric("plotEnvelope"))




#' @name trRmDuplicates
#' @rdname trRmDuplicates
#' @export
setGenericVerif("trRmDuplicates", function(x, tol = NULL, verbose = TRUE) 
  standardGeneric("trRmDuplicates"))

#' @name interpPos
#' @rdname interpPos
#' @export
setGenericVerif("interpPos", function(x, topo, plot = FALSE, r = NULL, tol = NULL, 
                                      method = c("linear", "linear", "linear"), crs = NULL, ...) 
  standardGeneric("interpPos"))

setGeneric("interpPosArray", function(x, d, GPGGA = NULL, geojson = NULL, 
                   tol = NULL, backproject = FALSE)
  standardGeneric("interpPosArray"))

# #' @name interpPosFromGPGGA
# #' @rdname interpPosFromGPGGA
# #' @export
# setGenericVerif("interpPosFromGPGGA", 
#           function(x, GPGGA, tol = NULL, backproject = TRUE)
#             standardGeneric("interpPosFromGPGGA"))

#' @name regInterpPos
#' @rdname regInterpPos
#' @export
setGenericVerif("regInterpPos", function(x, type = c("linear", "cosine"), 
                                         dx = NULL)  standardGeneric("regInterpPos"))

#' @name relTrPos
#' @rdname relTrPos
#' @export
setGenericVerif("relTrPos", function(x, last = FALSE) 
  standardGeneric("relTrPos"))

#' @name relTrPos3D
#' @rdname relTrPos
#' @export
setGenericVerif("relTrPos3D", function(x, last = FALSE) 
  standardGeneric("relTrPos3D"))



# #' @name readGPR
# #' @rdname readGPR
# #' @export
# setGenericVerif("readGPR", function(fPath, desc = "", ...) 
#   standardGeneric("readGPR")
# )



#' @name reverse
#' @rdname reverse
#' @export
setGenericVerif("reverse", function(x, id = NULL,  tol = 0.3) 
  standardGeneric("reverse"))


#' @name setGridCoord
#' @rdname setGridCoord-methods
#' @export
setGeneric("setGridCoord<-",function(x,value){standardGeneric("setGridCoord<-")})


#' @name shiftEst
#' @rdname shiftEst
#' @export
setGenericVerif("shiftEst", function(x, y = NULL, 
                                     method=c("phase", "WSSD"), dxy = NULL, ...) 
  standardGeneric("shiftEst"))








setGenericVerif("timeCorOffset", function(x, t0 = NULL, track = TRUE) 
  standardGeneric("timeCorOffset"))


setGenericVerif("corAntElev", function(x, c0 = 0.3)
  standardGeneric("corAntElev"))

#' @name filter1D
#' @rdname filter1D
setGenericVerif("filter1D", 
                function(x, 
                         type = c("runmed", "runmean", "MAD", "Gaussian"), 
                         w = NULL,
                         track = TRUE) 
                  standardGeneric("filter1D"))


setGenericVerif("dewow", 
                function(x,
                         type = c("runmed", "runmean", "MAD", "Gaussian"), 
                         w = NULL, 
                         track = TRUE ) 
  standardGeneric("dewow"))





setGenericVerif("trAmplCor", 
                function(x, type=c("power", "exp", "agc"),  ...) 
                  standardGeneric("trAmplCor"))

setGenericVerif("dcshift", function(x, u = NULL, FUN = mean, ..., track = TRUE) 
  standardGeneric("dcshift"))

setGenericVerif("firstBreak", function(x, method = c("coppens",
                                                     "threshold",  "MER"), thr = 0.12, w = 11, ns = NULL, 
                                       bet = NULL)
  standardGeneric("firstBreak"))





setGenericVerif("traceScaling", 
                function(x, 
                         type = c("stat", "min-max", "95", "eq", "sum", "rms", 
                                  "mad", "invNormal"), 
                         track = TRUE) 
  standardGeneric("traceScaling"))


setGenericVerif("fFilter", function(x, f = 100, 
                                    type = c('low', 'high', 'bandpass'),
                                    L = 257, plotSpec = FALSE, track = TRUE) 
  standardGeneric("fFilter"))

setGenericVerif("fkFilter", function(x, fk = NULL, L = c(5, 5), npad = 1, 
                                     track = TRUE) 
  standardGeneric("fkFilter"))

setGenericVerif("eigenFilter", function(x, eigenvalue = NA, center = TRUE, 
                                        scale = FALSE, track = TRUE) 
  standardGeneric("eigenFilter"))

setGenericVerif("traceShift", 
                function(x,  ts, 
                         method = c("pchip", "linear", "nearest", 
                                    "spline", "cubic", "none"), 
                         crop = TRUE, track = TRUE)
                  standardGeneric("traceShift"))

setGenericVerif("interpTrace", 
                function(x,  z, method = c("pchip", "linear", 
                                           "nearest", "spline", "cubic"), 
                         crop = TRUE, track = TRUE) 
  standardGeneric("interpTrace"))




setGenericVerif("traceAverage", function(x, w = NULL, FUN = mean, ..., 
                                         track = TRUE) 
  standardGeneric("traceAverage"))

setGenericVerif("traceStat", function(x, w = NULL, FUN = mean, ..., 
                                      track = TRUE) 
  standardGeneric("traceStat"))



setGenericVerif("time0Cor",  function(x, t0 = NULL, 
                                      method = c("spline", "linear", "nearest", "pchip", "cubic", 
                                                 "none"), crop = TRUE, keep = 0, track = TRUE) 
  standardGeneric("time0Cor"))


setGenericVerif("rotatePhase", function(x, phi, track = TRUE) standardGeneric("rotatePhase"))


#------------------------------GRPsurvey
#' @name getGPR
#' @rdname getGPR
#' @export
setGenericVerif("getGPR", function(x,id) standardGeneric("getGPR"))

#' @name surveyIntersect
#' @rdname surveyIntersect
#' @export
setGenericVerif("surveyIntersect", function(x) 
  standardGeneric("surveyIntersect"))

#' @name writeSurvey
#' @rdname writeSurvey
#' @export
setGenericVerif("writeSurvey", function(x, fPath, overwrite=FALSE){ 
  standardGeneric("writeSurvey")})


#' @name interpSlices
#' @rdname interpSlices
#' @export
setGenericVerif("interpSlices", function(x, dx = NULL, dy = NULL, dz = NULL, 
                                         h = 6, extend = TRUE){ 
  standardGeneric("interpSlices")})

#' @name tpShift
#' @rdname tpShift
#' @export
setGenericVerif("tpShift", function(x, i, dx = 0, dy = 0){ 
  standardGeneric("tpShift")})


#' Georeferencing
#'
#' Perform on a set of x,y coordinates
#' (1) a translation by \code{-cloc}, then
#' (2) a rotation by \code{alpha} (radian), and (3)
#' a translation by \code{creg}. If \code{creg}
#' is \code{NULL}, then \code{creg} is set equal
#' to \code{cloc}.
#' @param x A matrix with the first two columns corresponding
#'          to coordinates.
#' @param alpha A length-one numeric vector corresponding to 
#'              the rotation angle in radians. If \code{alpha = NULL},
#'              \code{alpha} is estimated from the pairs of points in
#'              the local reference system (\code{ploc}) and in the
#'              regional reference system (\code{preg}).
#' @param cloc A length-two numeric vector corresponding to the coordinate
#'             center of the local reference system
#' @param creg A length-two numeric vector corresponding to the coordinate
#'             center of the regional reference system. Setting 
#'             \code{creg = NULL} (default) is equivalent to apply a rotation
#'             of angle \code{alpha} and center \code{cloc}.
#' @param ploc A matrix with the first two columns corresponding
#'             to coordinates in the local reference system.
#' @param preg A matrix with the first two columns corresponding
#'             to coordinates in the regional reference system.
#' @param FUN If \code{alpha = NULL}, a function to estimate the rotation angle
#'            from the angles computed for each pairs of coordinates of
#'            \code{ploc}-\code{preg}.
#' @export
#' @name georef
#' @rdname georef
setGeneric("georef", function(x, alpha = NULL, cloc = NULL, creg = NULL,
                               ploc = NULL, preg = NULL, FUN = mean){ 
  standardGeneric("georef")})

#------------------------------BOTH

#' Robust smoothing
#' 
#' A wrapper for the functions \code{robfilter::hybrid.filte} and
#' \code{smooth.spline}
#' @param x       a numeric vector or (univariate) time series object.
#' @param spar    smoothing parameter, typically (but not necessarily) in (0,1].
#'                See \code{\link[stats]{smooth.spline}}.
#' @param width	  an odd positive integer (>=3) defining the window width 
#'                used for fitting.
#'                See \code{\link[robfilter]{hybrid.filter}}.
#' @param method	a (vector of) character string(s) containing the method(s) 
#'                to be used for the estimation of the signal level. 
#'                Method choice: "MED", "RM", 
#'                "MEAN", FMH, "PFMH", "CFMH", "MH", "PRMH", "CRMH", "MMH", 
#'                "PRMMH", "CRMMH".
#'                See \code{\link[robfilter]{hybrid.filter}}.
#' @param extrapolate	a logical indicating whether the level estimations 
#'                    should be extrapolated to the edges of the time series.  
#'                    See \code{\link[robfilter]{hybrid.filter}}.  
#' @export            
robustSmooth <- function(x, spar = NULL, width,  method = "PRMMH", extrapolate = TRUE){
  if (missing(width)) {
    stop("argument 'width' is missing with no default")
  }
  xf <- robfilter::hybrid.filter(x, width = width, method = method, extrapolate = extrapolate)
  xfs <- smooth.spline(x = xf$level$PRMMH,  spar = spar)$y
  return(xfs)
}


#setGenericVerif("adimproSmooth", function(x,hmax=2,...) standardGeneric("
# adimproSmooth"))



#' Structure tensor field of GPR data 
#'
#' @name strTensor
#' @rdname strTensor
#' @export
setGenericVerif("strTensor", function(x,  blksze = c(2, 4),
                                      kBlur   = list(n = 1, m = 1, sd = 1), 
                                      kEdge   = list(n = 5, m = 5, sd = 1), 
                                      kTensor = list(n = 5, m = 5, sd = 1),
                                      thresh = 0.02, what = c("tensor", "mask", "orientation"), ...)
  standardGeneric("strTensor"))





################## GPR PROCESSING ######################3


# TAPER WINDOWS
# tapertype = MinPhase.tapertype;
# tabpZL = nlags;

# switch lower(tapertype)
# case 'cos'
# taper = (sin(linspace(0,pi/2,tabpZL)).^2)';
# ACFoutput(1:tabpZL) = ACFoutput(1:tabpZL).*taper;
# ACFoutput(end-tabpZL+1:end) = ACFoutput(end-tabpZL+1:end).*
#           flipud(taper);
# case 'triang'
# taper = triang(2*tabpZL+1);
# ACFoutput = ACFoutput.*taper;
# case 'hamming'
# taper = hamming(2*tabpZL+1);
# ACFoutput = ACFoutput.*taper;
# case 'bartlett'
# taper = bartlett(2*tabpZL+1);
# ACFoutput = ACFoutput.*taper;
# case 'none'
# % taper = ones(2*tabpZL+1,1);
# % ACFoutput = ACFoutput-ACFoutput(end);
# case 'limtaper'
# % taper = ones(size(ACFoutput));
# % taper = cs_taper(taper,'hann',round(length(ACFoutput)*0.25));
# % ACFoutput = cs_taper(ACFoutput,'cos',round(length(ACFoutput)*0.25));
# ACFoutput = cs_taper(ACFoutput,'cos',round(length(ACFoutput)*0.05));
# case 'hann'
# ACFoutput = cs_taper(ACFoutput,'hann');
# case 'flattop'
# taper = flattopwin(2*tabpZL+1);
# ACFoutput = ACFoutput.*taper;
# end


##----------- helper functions -------------------##
# FID <- choose.files(caption = " txt files",filters = c("txt","*.txt"))
# output = list of data frame (one for each file from FID) 
#    with c("x","y","z","trace_number") structure
#' read fiducial marker files
#' 
#' read fiducial marker files
#' @export
readFID <- function(FID, sep = NULL, verbose = TRUE){
  myFid <- list() 
  for(i in seq_along(FID)){
    if(verbose) message("read ", FID[[i]], "...")
    pp <- verboseF( detectASCIIProp(FID[[i]]), verbose = verbose)
    A <- read.table(file             = FID[[i]], 
                    sep              = pp$sep, 
                    stringsAsFactors = FALSE, 
                    header           = pp$header,
                    skip             = pp$skip)
    # A <- read.table(FID[[i]], sep=",", stringsAsFactors=FALSE,header=TRUE)
    # colnames(A) <- toupper(colnames(A))
    # if(!all(c("E","N","Z","TRACE") %in% colnames(A))){
    #   stop("The headers should be \"E\",\"N\",\"Z\",\"TRACE\"!\n")
    # }
    # myFid[[i]] <- A[,c("E","N","Z","TRACE")]
    if(ncol(A) < 4){
      stop(FID[[i]], " must have 4 columns: x, y, z and trace number!")
    }
    # else if(ncol(A) > 4 && verbose){
    #   warning(FID[[i]], " has ", ncol(A), ". I take only the 4 first columns.")
    # }
    if(!is.null(colnames(A))){
      colnames(A) <- toupper(colnames(A))
      if(all(c("E", "N", "Z", "TRACE") %in% colnames(A))){
        A <- A[, c("E", "N", "Z", "TRACE")]
      }else if(all(c("X", "Y", "Z", "TRACE") %in% colnames(A))){
        A <- A[, c("X", "Y", "Z", "TRACE")]
      }
    }
    colnames(A)[1:4] <- c("x", "y", "z", "tn")
    myFid[[i]] <- A[, 1:4]
  }
  return(myFid)
}

#' read topo file
#' 
#' read topo file
#' @export
readTopo <- function(TOPO, sep = NULL, verbose = TRUE){
  myTopo <- list() 
  for(i in seq_along(TOPO)){
    if(verbose) message("read ", TOPO[[i]], "...")
    pp <- verboseF( detectASCIIProp(TOPO[[i]]), verbose = verbose)
    A <- read.table(file             = TOPO[[i]], 
                    sep              = pp$sep, 
                    stringsAsFactors = FALSE, 
                    header           = pp$header,
                    skip             = pp$skip)
    if(ncol(A) < 3){
      stop(TOPO[[i]], " must have 3 columns: x, y and z!")
    }else if(ncol(A) > 3 && verbose){
      warning(TOPO[[i]], " has ", ncol(A), ". I take only the 3 first columns.")
    }
    colnames(A)[1:3] <- c("x", "y", "z")
    myTopo[[i]] <- A[,1:3]
  }
  return(myTopo)
}

# If there are more recorded positions than fiducials
# remove duplicates from the recorded positions
# P = Position (topo)
# F = fiducials (from GPR data)
rmDuplicates <- function(xyz, fid, tol = 0.1){
  dn <- nrow(xyz) - nrow(fid)
  if(dn > 0){
    dd <- as.matrix(dist(xyz[, 1:2]))
    diag(dd) <- NA
    dd[upper.tri(dd)] <- NA
    ij <- which(dd <= min(sort(dd)[dn], tol, arr.ind = TRUE))
    # handle "duplicate"
    if(length(ij) > 0){
      A <- list()
      it <- 1
      for(k in 1:nrow(xyz)){
        ik <- apply(ij, 1, function(x, k0 = k) any(x %in% k0))
        if(sum(ik) > 0){
          A[[it]] <- unname(ij[ik,])
          ij <- ij[-ik,]
          it <- it + 1
        }else if( !(k %in% unlist(A, use.names = FALSE))){
          A[[it]] <- k
          it <- it + 1
        }
      }
      ##TODO ##FIXME
      if(length(A) != nrow(fid)) stop("case to be implemented!!")
      ##TODO ##FIXME
      nL <- sapply(A, length)
      B <- matrix(ncol = length(A), nrow = prod(nL))
      for(k in 1:length(A)){
        B[,k] <- rep(A[[k]], prod(nL[-k]))
      }
      regres <- numeric(nrow(B))
      for(k in 1:nrow(B)){
        posTopo <- posLine(xyz[B[k,], 1:2])
        reg <- lm(fid$POSITION ~ posTopo)
        regres[k] <- summary(reg)$r.squared
      }
      iOK <- which.max(regres)
      xyz <- xyz[B[iOK,], ]
    }
  }
  return(xyz)
}

# tt = x@time
# pos = x@pos
# xyz <- TOp
# fid <- fi
# fidx = x@fid
addFid <- function(xyz, fid, tt, pos, fidx){
  dn <- nrow(xyz) - nrow(fid)
  if(dn > 0){
    dtime  <- c(0, diff(tt))
    sel    <- pos %in% fid$POSITION
    idtime <- which(dtime > 1.5 * sd(dtime[!sel]) & !sel)
    dn <- nrow(xyz) - nrow(fid)
    if(length(idtime) > 0){
      # en fait il faudrait regarder au cas par cas pour chaque
      # coordonnee auquelle il manque un fiducial
      if(length(idtime) >= dn){ 
        posTopo <- posLine(xyz[, 1:2])
        vx <- combn(seq_along(idtime), dn)
        regres <- numeric(ncol(vx))
        for(k in seq_len(ncol(vx))){
          dxt <- sort(c(idtime[vx[,k]], which(sel)))
          reg <- lm( pos[dxt] ~ posTopo)
          regres[k] <- summary(reg)$r.squared
        }
        iOK <- which.max(regres)
        dxt <- sort(c(idtime[vx[,iOK]], which(sel)))
        ffid <- fidx[dxt]
        ffid[1] <- "START"
        ffid[length(ffid)] <- "END"
        if(length(ffid) > 2){
          ffid[2:(length(ffid)-1)] <- paste0("FID", seq_len(length(ffid)-2))
        }
      }else{
        stop("TO BE IMPLEMENTED")
      }
      fid <- data.frame(TRACE    = dxt, 
                        POSITION = pos[dxt], 
                        COMMENT  = ffid)
      com <- paste0("  find ", dn," new FIDs!")
    }
  }
  return(fid)
}

rmxyz <- function(xyz, fid){
  dn <- nrow(xyz) - nrow(fid)
  if(dn > 0){
    vx <- combn(1:nrow(xyz), nrow(fid))
    regres <- numeric(ncol(vx))
    coeff <- numeric(ncol(vx))
    for(k in 1:ncol(vx)){
      posTopo <- posLine(xyz[vx[,k], 1:2])
      reg <- lm(fid$POSITION ~ posTopo)
      regres[k] <- summary(reg)$r.squared
      coeff[k] <- reg$coef[2]
    }
    if(nrow(fid) == 2){
      iOK <- which.min(abs(coeff - 1))
    }else{
      iOK <- which.max(regres)
    }
    #com <- "   remove a coord!"
    xyz <- xyz[vx[,iOK],]
  }
  return(xyz)
}

rmfid <- function(xyz, fid){
  dn <- nrow(xyz) - nrow(fid)
  if(dn < 0){
    # remove fiducials!!
    vx <- combn(1:nrow(fid), nrow(xyz))
    regres <- numeric(ncol(vx))
    coeff <- numeric(ncol(vx))
    posTopo <- posLine(xyz[, 1:2])
    for(k in 1:ncol(vx)){
      reg <- lm(fid$POSITION[vx[,k]] ~ posTopo)
      regres[k] <- summary(reg)$r.squared
      coeff[k] <- reg$coef[2]
    }
    if(nrow(fid) == 2){
      iOK <- which.min(abs(coeff - 1))
    }else{
      iOK <- which.max(regres)
    }
    com <- "   remove a fiducial!"
    fid <- fid[vx[,iOK],]
  }
  return(fid)
}

#' Link coordinates to fiducial marker
#'
#' To interpolate topo
#' @param y object of class GPSsurvey
#' @param xyz matrix of coordinates
#' @param pcode character vector (length(pcode) = nrow(xyz)) indicating which
#'              coordinates to which GPR data belongs
#' @param tol Tolerance to detect duplicates from topo data   
#' @export
linkCoordFid <- function(y, xyz, pcode, tol = 0.1 ){
  FIDs <- list()
  sn <- names(y)
  for(i in seq_along(y)){
    tp <- grep(sn[i], pcode)
    x <- y[[i]]
    fi <- exportFid(x)
    xyzp <- xyz[tp,]
    xyzp <- rmDuplicates(xyz = xyzp, fid = fi)
    fi <- addFid(xyz = xyzp, fid = fi, tt = x@time, pos = x@pos, fidx = x@fid)
    xyzp <- rmxyz(xyz = xyzp, fid = fi)
    fi <- rmfid(xyz = xyzp, fid = fi)
    if( nrow(fi) == nrow(xyzp)){
      fi$E <- xyzp[, "E" ]
      fi$N <- xyzp[, "N" ]
      fi$Z <- xyzp[, "Z" ]
      FIDs[[i]] <- fi
    }
  }
  return(FIDs)
}
# # 1. too much coordinates  n(coord) > n(fid)
# #   a. remove duplicates
# #     i. n(coord) = n(fid) -> OK
# #     ii. too much coordinates  n(coord) > n(fid)
# #       > maybe a fiducial is missing
# #         find potential fiducials
# #         make all possible combination fiducials - coords
# #         * n(coord) = n(fid) -> OK
# #         * ii. too much coordinates  n(coord) > n(fid)
# #           - remove a fiducial
# fids <- linkCoordFid(y = SU, xyz = TO[, c("E", "N", "Z")], pcode = TO$PCODE)
# which(sapply(fids, is.null))

# for(i in seq_along(SU)){
# posTopo <- posLine(fids[[i]][, c("E", "N")])
# reg <- lm(fids[[i]]$POSITION ~ posTopo)
# plot(posTopo, fids[[i]]$POSITION, main = SU@names[i], asp = 1)
# abline(reg, col = "red")
# title(sub = summary(reg)$r.squared)
# Sys.sleep(0.5)
# if(max(reg$res) < 1.5 | summary(reg)$r.squared > 0.99){
# title("\n\nOK!")
# Sys.sleep(1.5)
# }else{
# message(SU@names[i])
# }
# }


##------------- FILENAME/FILEPATH/EXTENSION -------------------##

#' Filepath(s) with correct extension(s)
#' 
#' Returns the filepaths with the correct extension and check for 
#' upper and lower case extension (e.g., ".txt" or ".TXT")
#' @param fPath length-one character vector (e.g., "xline01.dt1")
#' @param ext   character vector of the extension required
#' @param throwError boolean. If TRUE, an error is thrown if the filepath
#'                   with one of the extension does not exist. If FALSE,
#'                   it returns NULL for the missing extension
#' @return A list whose keys correspond to \code{ext} and the values to 
#'         the filepaths:
#'         $hd  -> xline01.hd
#'         $dt1 -> xline01.dt1
#' @export  
getFName <- function(fPath, ext = c(".hd", ".dt1"), throwError = TRUE){
  fp <- file.path(dirname(fPath), .fNameWExt(fPath))
  ext <- tolower(ext)
  Ext <- toupper(ext)
  mfile <- list()
  for(i in seq_along(ext)){
    if(file.exists(f1 <- paste0(fp, Ext[i]))){
      #mfile[[gsub("^[.]",  "", ext[i])]] <- paste0(fp, Ext[i])
      mfile[[gsub("^[.]",  "", ext[i])]] <- f1
    }else if(file.exists(f2 <- paste0(fp, ext[i]))){
      #mfile[[gsub("^[.]",  "", ext[i])]] <- paste0(fp, ext[i])
      mfile[[gsub("^[.]",  "", ext[i])]] <- f2
    }else{
      if(isTRUE(throwError)){
        stop("Files '", f1, "' and '", f2, "' do not exist!\n",
             "Check the filepath!")
      }else{
        mfile[[gsub("^[.]",  "", ext[i])]] <- NULL
      }
    }
  }
  return(mfile)
}

# NAME: safe file path
# test if the file already exists
# if yes, add a suffix to the filepath
safeFPath <- function(fPath = NULL){
  dirName   <- dirname(fPath)
  fName <- .fNameWExt(fPath)
  ext <- .fExt(fPath)
  if(dirName == '.'){
    fPath <- fName
  }else{
    fPath <- paste0(dirName, '/' , fName)
  }
  fPath_orgi <- fPath
  k <- 0
  while(file.exists(paste0(fPath, ".", ext)) || 
        file.exists( paste0(fPath, ".HD"))){
    fPath <- paste0(fPath_orgi, "_", k)
    k <- k + 1
  }
  newfPath <- paste0(fPath, ".", ext)
  return(newfPath)
}

safeName <- function(x, y){
  xold <- x
  k <- 1
  while(any(x == y)){
    x <- paste0(xold, "_", k)
    k <- k + 1
  }
  return(x)
}

#' Trim string
#'
#' returns string w/o leading or trailing whitespace
#' @export
trimStr <- function (x) gsub("^\\s+|\\s+$", "", x)

# return filename without extension
#' @export
.fNameWExt <- function(x){
  unlist(lapply(strsplit(basename(x),"[.]"), head , 1 ), use.names = FALSE)
}

# return the file extension.

#' @export
.fExt <- function(x){
  #   cat("with caution... because split \'.\' may not be so good\n")
  unlist(lapply(strsplit(basename(x),"[.]"), tail , 1 ), use.names = FALSE)
}


.saveTempFile <- function(x){
  tmpf <- tempfile(x@name)
  writeGPR(x, type = "rds", overwrite = FALSE, fPath = tmpf)
  return(paste0(tmpf, ".rds"))
}



# Compute the orientation angle of GPR profile
# TODO: compute all the angles (maybe not a good idea...)
gprAngle <- function(x){
  #dEN <- x@coord[1:(length(x) - 1),1:2] - x@coord[2:length(x),1:2]
  # return(atan2(dEN[1,2], dEN[1,1]))
  dEN <- x@coord[1,1:2] - tail(x@coord[,1:2],1)
  return(atan2(dEN[2], dEN[1]))
}

# is angle b between aref - 1/2*atol and aref + 1/2*atol?

#' @export
inBetAngle <- function(aref, b, atol = pi/10){
  dot <- cos(b)*cos(aref) + sin(b) * sin(aref)
  return(acos(dot) <= atol)
}



# select a box on plot(mySurvey) and return list(xlim,ylim)
# that can be used in plot3D:
#   plot(mySurvey,asp=1)
#   bbox <- selectBBox()
#   plot3D(mySurvey, addTopo=TRUE,clip=30, xlim=bbox$xlim,
# ylim=bbox$ylim, add=FALSE)

selectBBox <- function(border="red",lwd=2,...){
  bbox <- locator(type="p",n=2)
  LIM <- sapply(bbox, range)
  rect(LIM[1,"x"], LIM[1,"y"], LIM[2,"x"], LIM[2,"y"], border=border)
  return(list("xlim"=LIM[,"x"], "ylim" =LIM[,"y"]))
}

# source "whuber" from stackexchange.com
# https://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points/181883#181883
# Oriented Bounding Box
OBB <- function(p) {
  # Analyze the convex hull edges     
  a <- chull(p)                                   # Indexes of extremal points
  a <- c(a, a[1])                                 # Close the loop
  e <- p[a[-1],] - p[a[-length(a)], ]             # Edge directions
  norms <- sqrt(rowSums(e^2))                     # Edge lengths
  v <- e / norms                                  # Unit edge directions
  w <- cbind(-v[,2], v[,1])                       # Normal directions to the edges
  
  # Find the MBR
  vertices <- p[a, ]                              # Convex hull vertices
  x <- apply(vertices %*% t(v), 2, range)         # Extremes along edges
  y <- apply(vertices %*% t(w), 2, range)         # Extremes normal to edges
  areas <- (y[1,]-y[2,])*(x[1,]-x[2,])            # Areas
  k <- which.min(areas)                           # Index of the best edge (smallest area)
  
  # Form a rectangle from the extremes of the best edge
  cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% rbind(v[k,], w[k,])
}

#' Points perdicular to a polyline
#'
#' Compute oriented transects along a polyline (one transect per points)
#' @param xy matrix n row, 2 column (x and y positions)
#' @param d numeric vector of length m defining the distance of the transect from the
#'          polyline points. If length m > 1 several transects are returned.
#' @return a list with elements x and y of dimension (n, m).
#' @export
perpPoints <- function(xy, d){
  xy2 <- xy[c(1,1:nrow(xy), nrow(xy)),]
  xlat <- matrix(nrow = nrow(xy), ncol = length(d))
  ylat <- matrix(nrow = nrow(xy), ncol = length(d))
  for(i in 2:(nrow(xy2) - 1)){
    if( xy2[i - 1, 2] == xy2[i + 1, 2]){
      xlat[i-1, ] <- xy2[i, 1]
      ylat[i-1, ] <- xy2[i, 2] + d
    }else if(xy2[i - 1, 1] == xy2[i + 1, 1] ){
      xlat[i-1, ] = xy2[i, 1] + d
      ylat[i-1, ] = xy2[i, 2]
    }else{
      #get the slope of the line
      m <- ((xy2[i - 1, 2] - xy2[i + 1, 2])/(xy2[i - 1, 1] - xy2[i + 1, 1]))
      #get the negative reciprocal, 
      n_m <- -1/m
      sng <- sign(xy2[i + 1, 1] - xy2[i - 1, 1])
      DD <- d / sqrt( n_m^2 + 1)
      if( m < 0){
        DD <- -DD
      }
      if(sng > 0){
        xlat[i-1, ] = xy2[i, 1] +  DD
        ylat[i-1, ] = xy2[i, 2] + n_m * DD
      }else{
        xlat[i-1, ] = xy2[i, 1] - DD
        ylat[i-1, ] = xy2[i, 2] - n_m * DD
      }
    }  
  }
  return(list(x = xlat, y = ylat))
}


# NOT USED!
# .fidpos <- function(xyz,fid){
#   return(xyz[trimStr(fid)!="",,drop=FALSE])
# }

.plotLine <- function(xyz,...){
  lines(xyz[,1:2],...)
}

.plotArrows <- function(xyz, ...){
  arrows(xyz[nrow(xyz)-1,1], xyz[nrow(xyz)-1,2], xyz[nrow(xyz),1], 
         xyz[nrow(xyz),2], ...)
}

.whichMin <- function(x,y){
  which.min(abs(x-y))
}

.which <- function(x,y){
  which(x==y)
}

.lengthList <- function(x){
  if(typeof(x)=="list"){
    return(length(x))
    # print(typeof(x))
  }else{
    return(1)
  }
}

# flatte a nested list
#' @export
flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1] == "list")
  if(sum(morelists)){ 
    out <- lapply(seq_along(morelists), .unlist, x = x, z = morelists)
    out <- unlist(out, recursive = FALSE)
    Recall(out)
  }else{
    return(x)
  }
}

.unlist <- function(i, x, z){
  if(isTRUE(z[i])){
    names(x[[i]]) <- rep(names(x[i]), length(x[[i]]))
    x[[i]]
  }else{
    x[i]
    # print(names(x[i]))
  }
}

#-- not exported!
# Georeferencing
#
# Perform on a set of x,y coordinates
# (1) a translation by \code{-cloc}, then
# (2) a rotation by \code{alpha} (radian), and (3)
# a translation by \code{creg}. If \code{creg}
# is \code{NULL}, then \code{creg} is set equal
# to \code{cloc}.
# @param x A matrix with the first two columns corresponding
#          to coordinates.
# @param alpha A length-one numeric vector corresponding to 
#              the rotation angle in radians. If \code{alpha = NULL},
#              \code{alpha} is estimated from the pairs of points in
#              the local reference system (\code{ploc}) and in the
#              regional reference system (\code{preg}).
# @param cloc A length-two numeric vector corresponding to the coordinate
#             center of the local reference system
# @param creg A length-two numeric vector corresponding to the coordinate
#             center of the regional reference system. Setting 
#             \code{creg = NULL} (default) is equivalent to apply a rotation
#             of angle \code{alpha} and center \code{cloc}.
# @param ploc A matrix with the first two columns corresponding
#            to coordinates in the local reference system.
# @param preg A matrix with the first two columns corresponding
#             to coordinates in the regional reference system.
# @param FUN If \code{alpha = NULL}, a function to estimate the rotation angle
#            from the angles computed for each pairs of coordinates of
#            \code{ploc}-\code{preg}.
.georef <- function(x, alpha = NULL, cloc = c(0,0), creg = NULL,
                    ploc = NULL, preg = NULL, FUN = mean){
  x0 <- as.matrix(unname(x[, 1:2, drop = FALSE]))
  if(is.null(alpha)){
    cloc <- as.double(cloc)
    creg <- as.double(creg)
    ploc <- as.matrix(ploc)
    preg <- as.matrix(preg)
    alphaloc <- atan2(ploc[,1] - cloc[1], ploc[,2] - cloc[2])
    alphareg <- atan2(preg[,1] - creg[1], preg[,2] - creg[2])
    alpha <- alphareg - alphaloc
    message(paste0("rotation angles: ", 
                   paste0(round(alpha,4), collapse = ", "), "."))
    alpha <- FUN(alpha)
  }
  ROT <- matrix(c( cos(alpha), sin(alpha),
                   -sin(alpha), cos(alpha)), nrow=2, ncol=2)
  TRL <-  matrix(as.double(cloc[1:2]), nrow = nrow(x0), 
                 ncol = 2, byrow = TRUE)
  if(is.null(creg)){
    TRL2 <- TRL
  }else{
    TRL2 <-  matrix(creg[1:2], nrow = nrow(x0), ncol = 2, byrow = TRUE)
  }
  x[,1:2] <- (x0 - TRL) %*% ROT + TRL2
  return(x)
}

#' Relative position on a multiline
#'
#' Relative position on a multiline
#' @export
posLine <- function(loc, last = FALSE){
  loc <- as.matrix(loc)
  all_dist <- cumsum(c(0, sqrt(apply(diff(loc)^2, 1, sum))))
  if(last){
    return(tail(all_dist, 1))
  }else{
    return(as.numeric(all_dist))
  }
}



#' Closest trace
#' 
#' Return the indice of the closest trace to the point \code{y}
#' @param x Object of the class GPR.
#' @param y Length-two numeric vector of the (x, y)-coordinates.
#' @return Indice (integer) of the closest trace.
#' @export
closestTr <- function(x, y){
  ymat <- matrix(as.numeric(y[1:2]), 
                 nrow = length(x),
                 ncol = 2,
                 byrow = TRUE)
  which.min(rowSums((ymat - x@coord[,1:2])^2))
}




.doubleVector <- function(v,n=2L){
  if(n > 1){
    m <- length(v)
    dxpos <- rep( diff(v)/n,n-1)
    vv <- v[-m] + rep(seq(1,n-1),each=m-1)*dxpos
    xvalues  <- sort(c(v,vv ,v[m] + cumsum(rep(dxpos[length(dxpos)],n-1))))
    xvalues <- xvalues[1:(length(xvalues))]
    return(xvalues)
  }else{
    return(v)
  }
}


#--------------------------------
#' wapply: A faster (but less functional) "rollapply" for vector setups

#' April 23, 2013.
#' By A.N. Spiess, senior scientist at the Department of Andrology at the 
#' University Hospital Hamburg-Eppendorf.
#' This is what turned out (wapply for "window apply").
#' @export
wapply <- function(x=NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- length(x)
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}

#' Wapply on the row of a matrix (windowed)
#'
#' NOT CURRENTLY USED.
#' mod by MANU.
#' @export
wapplyRow <- function(x = NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- nrow(x)
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a,,drop=FALSE], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}

#' Wapply on the row of a matrix (windowed + CENTERED)
#'
#' NOT CURRENTLY USED.
#' mod by MANU.
#' @export
wapplyRowC <- function(x = NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- nrow(x)
  SEQ1 <- seq(-(width-1)/2 + 1, lenX -(width-1)/2, by = by)
  SEQ2 <- lapply(SEQ1, function(x){ xnew <- x:(x + width - 1)
  xnew <- xnew[xnew > 0]
  xnew <- xnew[xnew <= lenX]})
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a,,drop=FALSE], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}

#' windowing with centered window
#'
#' based on wapply and modified by Manu.
#' centered moving window.
#' return a matrix of the same dimension than x.
#' some border effect at a distance < width/2 at the first and last col/row
#' @export
wapplyMat <- function(x = NULL, width = NULL, by = NULL, FUN = NULL, 
                      MARGIN = 1, ...){
  FUN <- match.fun(FUN)
  width <- ifelse(width %% 2 == 0, width + 1, width)
  if (is.null(by)) by <- width
  lenX <- ifelse(MARGIN == 1, ncol(x), nrow(x))
  SEQ1 <- seq(-(width-1)/2 + 1, lenX -(width-1)/2, by = by)
  SEQ2 <- lapply(SEQ1, function(x){ xnew <- x:(x + width - 1)
  xnew <- xnew[xnew > 0]
  xnew <- xnew[xnew <= lenX]})
  if(MARGIN == 1){
    OUT <- lapply(SEQ2, function(a) apply(x[, a, drop = FALSE], MARGIN, FUN, ...))
  }else if( MARGIN == 2) {
    OUT <- lapply(SEQ2, function(a) apply(x[a,, drop = FALSE], MARGIN, FUN, ...))
  }
  OUT <- base::simplify2array(OUT, higher = TRUE)
  if(MARGIN == 2){
    return(t(OUT))
  }else{
    return(OUT)
  }
}

#' windowing with not centered window
#'
#' based on wapply and modified by Manu.
#' not centered moving window! start first row/column and 
#' stop when the extremity of the windwo reach the last row/column.
#' return a matrix of with smaller dimension than x (margin - 2*width)
#' @export
wapplyMat2 <- function(x = NULL, width = NULL, by = NULL, FUN = NULL, 
                       MARGIN = 1, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- ifelse(MARGIN == 1, ncol(x), nrow(x))
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
  if(MARGIN == 1){
    OUT <- lapply(SEQ2, function(a) apply(x[, a, drop = FALSE], MARGIN, FUN))
  }else if( MARGIN == 2) {
    OUT <- lapply(SEQ2, function(a) apply(x[a,, drop = FALSE], MARGIN, FUN))
  }
  OUT <- base::simplify2array(OUT, higher = TRUE)
  if(MARGIN == 2){
    return(t(OUT))
  }else{
    return(OUT)
  }
}

xyToLine <- function(x){
  sp::Line(x[,1:2])
}

LineToLines <- function(i,pp, myNames){
  sp::Lines(pp[i],myNames[i])
}





# http://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
extrema <- function(x, type=c("max","min")){
  type <- match.arg(type, c("max", "min"))
  if( type == "min" ){
    x <- -x
  }
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  return(y)
}

#--- see trTime in "ClassGPR.R"
#  @export                  
# trRecTime <- function(x, origin = "1970-01-01"){
#   return(as.POSIXct(x@time, origin = origin))
# }

#' time to depth conversion
#'
#' time to depth conversion
#' @export                  
timeToDepth <- function(tt, time_0, v = 0.1, antsep = 1){
  # t0 <- time_0 - antsep/c0
  if(length(v) == 1){
    y <- v^2 * (tt - time_0)^2 - antsep^2
    test <- (y >= 0) & ((tt - time_0) >= 0)
  }else if(length(v) == length(tt)){
    y <- cumsum( c(0, diff(tt)) * v )^2  - antsep^2
    test <- (y >= 0)
  }
  y[!test] <- NA
  y[test] <- sqrt(y[test])/2
  return(y)
  # sqrt(v^2*(tt - t0)- antsep^2)/2
}

#' Depth to time conversion
#' 
#' Depth to time conversion
#' @export
depthToTime <- function(z, time_0, v = 0.1, antsep = 1, c0 = 0.299){
  #FIXME
  t0 <- time_0 - antsep/c0
  sqrt((4*z^2 + antsep^2)/(v^2)) + t0
}

#' Depth zero 
#' 
#' Depth zero 
#' @export
depth0 <- function(time_0, v=0.1, antsep=1, c0 = 0.299){
  time_0 - antsep/c0 + antsep/v
}

#' Convert first wave break to time-zero
#'
#' Account for the delay time between time of wave emission and time of first
#' wave break recording due to the antenna separation (offset).
#' @rdname timeCorOffset
#' @export
firstBreakToTime0 <- function(fb, x, c0 = 0.299){
  if(length(x@antsep) == 0 || (!is.numeric(x@antsep))){
    stop("You must first define the antenna separation",
         "with `antsep(x)<-...`!")
  }
  fb - x@antsep/c0
}

.plot3DRGL <- function(A, x, y, z, z0, col = palGPR(n = 101), 
                       back = "fill", smooth = TRUE, lit = FALSE,
                       lwd = 0, empty = FALSE, ...){
  nr = nrow(A)
  nc = ncol(A)
  if(empty == TRUE){
    X <- matrix( x, ncol = nc, nrow = 2, byrow = TRUE)
    Y <- matrix( y, ncol = nc, nrow = 2, byrow = TRUE)
    Z <- matrix(z0, ncol = nc, nrow = 2, byrow = TRUE) - 
      matrix(z[c(1, nr)], ncol = nc, nrow = 2, byrow = FALSE)
    colA <- col[1]
    if(!is.null(list(...)$alpha) && (list(...)$alpha==0 || is.null(col))){
      
    }else{
      rgl::rgl.surface(Y, X, Z, color = colA, back = back, 
                       smooth = smooth, lit = lit, lwd = lwd, ...) 
    }
    rgl::lines3d(y, z0, x, col = "black", alpha = 1, lwd = lwd)   
    rgl::lines3d(y, (z0 - z[length(z)]), x, col = "black", alpha = 1, lwd = lwd)   
    rgl::lines3d(rep(y[1], 2), (z0[1] - z), rep(x[1], 2), col = "black", 
            alpha = 1, lwd = lwd)
    rgl::lines3d(rep(y[length(y)], 2), (z0[length(z0)] - z), rep(x[length(x)], 2),
            col = "black", alpha = 1, lwd = lwd)   
    
  }else{
    X <- matrix( x, ncol = nc, nrow = nr, byrow = TRUE)
    Y <- matrix( y, ncol = nc, nrow = nr, byrow = TRUE)
    Z <- matrix(z0, ncol = nc, nrow = nr, byrow = TRUE) - 
      matrix(z, ncol = nc, nrow = nr, byrow = FALSE)
    A = (A - min(A, na.rm =TRUE))/(max(A, na.rm =TRUE) - min(A, na.rm =TRUE))
    # assign colors to heights for each point 
    colA <- col[A * (length(col) - 1) + 1] 
    rgl::rgl.surface(Y, X, Z, color = colA, back = back, smooth = smooth, 
                     lit = lit, lwd = lwd,...) 
  }
}

# .plot3DSlice <- function(XYZ,slice=c("x","y","z"),section=1,col=palGPR(n=101), 
#                           sampling = c(0.25,0.25,0.04),rmStripes = TRUE){
#   # k=100
#   # j=25
#   # i=40
#   # col <- tim.colors(101) # height color lookup table
#   slice = match.arg(slice)
#   if(length(slice)>1){
#     slice = slice[1]
#   }
#   
#   dimXYZ = dim(XYZ)
#   vz = seq(0,dimXYZ[3]-1,by=1)*sampling[3]  # dtime / 2 * v
#   vx = seq(0,dimXYZ[1]-1,by=1)*sampling[1]
#   vy = seq(0,dimXYZ[2]-1,by=1)*sampling[2]
#   if(rgl::rgl.cur()==0){  # si la fenêtre rgl est ouverte, on plot dedans...
#     rgl::rgl.open()
#     rgl::rgl.bg( color=c("white"))
#   }
#   i = section
#   j=i
#   k=i
#   if(slice=="x"){
#     if(rmStripes == TRUE){ 
#       Xside = normalizeGPR(removeStripes(t(XYZ[,j,])))
#     }else{  
#       Xside = normalizeGPR((t(XYZ[,j,])))  
#     }
#     
#     Xside_x = matrix(vx,nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=TRUE)
#     Xside_y = matrix( vy[j],nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=TRUE)
#     Xside_z = matrix( max(vz)-vz,nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=FALSE)
# 
#     CCX = (Xside-min(Xside))/(max(Xside)-min(Xside))
#     ClimX <- range(CCX)
#     ClenX <- ClimX[2] - ClimX[1] + 1
#     # col <- tim.colors(101) # height color lookup table
#     #col = palette(gray(0:101 / 101))
#     colCX <- col[ (CCX)*100+1 ] 
#     
#     surface3d(Xside_x, Xside_z, Xside_y, col= setCol(Xside), lit=FALSE,
#             front="fill",back="fill")#, alpha=0.5)
#   }else if(slice=="z"){
#     if(rmStripes == TRUE){ Zside = (removeStripes(t(XYZ[,,k])))
#     }else{  Zside = ((t(XYZ[,,k])))  }
#     
#     Zside_x = matrix(vx,nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=TRUE)
#     Zside_y = matrix( vy,nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=FALSE)
#     Zside_z = matrix(max(vz) - vz[k],nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=FALSE)
# 
#     CCZ = (Zside-min(Zside))/(max(Zside)-min(Zside))
#     ClimZ <- range(CCZ)
#     ClenZ <- ClimZ[2] - ClimZ[1] + 1
#     #col = palette(gray(0:101 / 101))
#     colCZ <- col[ (CCZ)*100+1 ]
#     
#     surface3d(Zside_x, Zside_z, Zside_y, col= setCol(Zside), lit=FALSE,
#               front="fill",back="fill")#, alpha=0.5)
#   }else if(slice=="y"){
#     if(rmStripes == TRUE){ Yside = normalizeGPR(removeStripes(t(XYZ[i,,])))
#     }else{  Yside = normalizeGPR((t(XYZ[i,,])))  }
#     
#     Yside_x = matrix(vx[i],nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=TRUE)
#     Yside_y = matrix( vy,nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=TRUE)
#     Yside_z = matrix( max(vz)-vz,nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=FALSE)
#     
# #     CCY = (Yside-min(Yside))/(max(Yside)-min(Yside))
# #     ClimY <- range(CCY)
# #     ClenY <- ClimY[2] - ClimY[1] + 1
# #     colCY <- col[ (CCY)*100+1 ] 
#     colCY <- colFromPal(Yside , col = col )
# 
#     surface3d(Yside_x, Yside_z, Yside_y, col= setCol(Yside), lit=FALSE,
#               front="fill",back="fill")#, alpha=0.5)
#   }
# }

# # shift the topography of the GPR profile (to display its topography)
# .topoShift <- function(x, topo, z){
#   zShift <- (max(topo) - topo)
#   # in fact >> old_t = x@depth
#   # old_t <- seq(0, length.out = nrow(x), by = dz)
#   old_t <- z
#   xShifted <- matrix(0, nrow = nrow(x) + floor(max(zShift)/dz), ncol = ncol(x))
#   n <- 1:(nrow(x)-2)
#   for(i in 1:ncol(x)){
#     # compute new t-vector for each trace
#     new_t <- old_t + zShift[i]
#     xit <- seq(ceiling(new_t[1]/dz), ceiling(new_t[nrow(x)-2]/dz))
#     # interpolate
#     # FIX ME! does not work well (not nice interpolation)
#     xShifted[xit + 1, i] = signal::interp1(new_t, x[,i], xi = xit * dz, 
#                                            method = "spline", extrap = TRUE)  
#   }
#   return(xShifted)
# }


# FIXME > vectorise that!
.traceShiftMat <- function(A, ts = 0, tt = NULL, dz = 0.4, method = "linear"){
  ps <- ts/dz
  Anew <- matrix(NA, nrow = nrow(A), ncol = ncol(A))
  v0 <- 1:nrow(A)
  for(i in seq_len(ncol(A))){
    relts <- floor(ps[i])*dz - ts[i]
    if(method == "none"){
      ynew <- A[,i]
    }else{
      ynew <- signal::interp1(tt, A[,i], tt + relts, 
                              method = method, extrap = NA)
    }
    vs <- v0 + floor(ps[i])
    test <- vs > 0 & vs <= nrow(A)
    vs <- vs[test]
    Anew[vs,i] <- ynew[test]
  }
  return(Anew)
}





#==============================#
#========== SPATIAL FILTER =========#


.medianFilter1D <- function(a,w){
  b <- a  # <- matrix(0, 364,364)
  for(i in (w+1):(length(a)-w-1)){
    xm <- a[i+(-w:w)]
    b[i] <- xm[order(xm)[w+1]]
  }
  return(b)
}

# run med mean mad
.runmmmMat <- function(x, w, type = c("runmed", "runmean", "runmad", "hampel")){
  type <- match.arg(type, c("runmed", "runmean", "runmad", "hampel"))
  if( (w %% 2) == 0 )  w <- w + 1 
  xdata <- matrix(0, nrow = nrow(x) + 2*w , ncol = ncol(x) )
  xdata[1:nrow(x) + w, ] <- x
  if(type == "runmed"){
    xdata <- apply(xdata, 2, stats::runmed, k = w)
  }else if(type == "runmean"){
    runmean <- function(x, k = 5){stats::filter(x, rep(1 / k, k), sides = 2)}
    xdata <- apply(xdata, 2, runmean, k = w)
  }
  else if(type == "hampel"){
    xdata <- apply(xdata, 2, rollapplyHampel, w ,  .fHampel)
  }
  # x <- x - xdata[1:nrow(x) + w, ]
  return(xdata[1:nrow(x) + w, ])
}


rollapplyHampel <- function(x, w, FUN){
  k <- trunc((w - 1)/ 2)
  locs <- (k + 1):(length(x) - k)
  num <- vapply(
    locs, 
    function(i) FUN(x[(i - k):(i + k)], x[i]),
    numeric(1)
  )
  x[locs] <- num
  return(x)
}
.fHampel <- function(x, y){
  x0 <- median(x)
  S0 <- 1.4826 * median(abs(x - x0))
  if(abs(y - x0) > 3 * S0) return(x0)
  # y[test] <- x0[test]
  return(y)
}





# histogram transformation to normal distributed data with same mean and sd
# https://msu.edu/~ashton/temp/nscore.R
.nScoreTrans <- function(x, inverse = FALSE, tbl = NULL){
  if(isTRUE(inverse)){
    if(is.null(tbl)){
      stop("tbl must be provided")
    }
    min_x <- min(tbl[,1])
    max_x <- max(tbl[,1])
    min_sc <- min(x)
    max_sc <- max(x)
    x1 <- c(min_x, tbl[,1], max_x)
    nsc <- c(min_sc, tbl[,2], max_sc)
    
    back.xf <- approxfun(nsc,x1) # Develop the back transform function
    val <- back.xf(x)
    return(val)
  }else{
    #     fx <- ecdf(x)
    #     x1 <- head(knots(fx), -1)
    #     xCDF <- fx(x1)
    #     y <- qnorm(xCDF, mean(x), sd = sd(x))
    if(!is.null(tbl)){
      x1 <- tbl[,1]
      y  <- tbl[,2]
    }else{
      x1 <- sort(x)
      y <- sort(qqnorm(x, plot.it = FALSE)$x)
      tbl <- data.frame(x = x1, nscore = y)
    }
    y_n <- approx(x1, y, xout = x, rule = 2)$y
    y_n <- y_n * sd(x) + mean(x)
    #return(list(x = y_n, table = tbl))
    return(y_n)
  }
}

# x = output of scale(...)
# y = object to back-transform, same dimension as x
# check:
# x <- scale(x0, center = TRUE, scale = TRUE)
# x0 <- unscale(x, x)

#' Unscale
#'
#' Back-transform/unscale from \code{scale}
#' @export
unscale <- function(x, y){
  xCenter <- attr(x, 'scaled:center')
  xScale <- attr(x, 'scaled:scale')
  if(is.null(xCenter)) xCenter <- rep(0, ncol(x))
  if(is.null(xScale))  xScale <- rep(1, ncol(x))
  ynew <- scale(y, center = -xCenter/xScale, scale = 1/xScale)
  attr(ynew,'scaled:center') <- NULL
  attr(ynew,'scaled:scale') <- NULL
  return(ynew)
}

#=============================================#
#======== CLIP/GAMMA/NORMALIZE ================#






#.rms <- function(num) sqrt(sum(num^2)/length(num))

scaleCol <- function(A, type = c("stat", "min-max", "95",
                                 "eq", "sum", "rms", "mad", "invNormal")){
  A <-  as.matrix(A)
  test <- suppressWarnings(as.numeric(type))
  if(!is.na(test) && test >0 && test < 100){
    A_q95 <- apply(A, 2, quantile, test/100, na.rm = TRUE)
    A_q05 <- apply(A, 2, quantile, 1 - test/100, na.rm = TRUE)
    Ascl <- scale(A, center = FALSE, scale = A_q95 - A_q05)
    # matrix(A_q95 - A_q05, nrow = nrow(A), ncol = ncol(A), byrow=TRUE)
    #A <- A/Ascl
  }else{
    type <- match.arg(type)
    if( type == "invNormal"){
      Ascl <- apply( A, 2, .nScoreTrans)
      return(Ascl)
    }else if(type == "stat"){
      # A <- scale(A, center=.colMeans(A, nrow(A), ncol(A)), 
      #            scale = apply(A, 2, sd, na.rm = TRUE))
      Ascl <- scale(A)
    }else if(type == "sum"){
      Ascl <- scale(A, center=FALSE, scale = colSums(abs(A)))
    }else if(type == "eq"){
      # equalize line such each trace has same value for 
      # sqrt(\int  (A(t))^2 dt)
      Aamp <- matrix(apply((A)^2,2,sum), nrow = nrow(A), 
                     ncol = ncol(A), byrow=TRUE)
      Ascl <- A*sqrt(Aamp)/sum(sqrt(Aamp))
    }else if(type == "rms"){
      Ascl <- scale(A, center = FALSE)
      # Ascl <- matrix(apply(A ,2, .rms), nrow = nrow(A), 
      #                ncol = ncol(A), byrow=TRUE)
    }else if(type == "min-max"){  # min-max
      Ascl <- scale(A, center = FALSE, 
                    scale = apply(A, 2, max, na.rm = TRUE) - 
                      apply(A, 2, min, na.rm = TRUE))
    }else if(type == "mad"){  # mad
      Ascl <- scale(A, center = apply(A, 2, median), 
                    scale = apply(A, 2, mad))
    }
  }
  test <- (!is.na(Ascl[1,] ) & abs(Ascl[1,]) > .Machine$double.eps^0.75) 
  A[,test] <- Ascl[,test]
  A[, !test] <- 0
  return(A)
}

rmsScaling <- function(...){
  stop("Deprecated! Use instead 'traceScaling(x,type=\"rms\")'\n")
}

#=============================================#
#======== SPECTRAL FUNCTIONS ================#


# @param [matrix]/[vector]   A     (each column represent a trace / a trace)
# @param [double]       dT     (sampling time in nanoseconde)  


# @return power spectrum (frequency, power, phase)
# -------------------------------------------

.fFilter1D <- function(A, f = c(100), type = c('low', 'high', 'bandpass'), 
                       L = 257, dT = 0.8, plotSpec = FALSE, fac = 1000000){
  type <- match.arg(type)
  A <- as.matrix(A)
  nr <- nrow(A)      # signal length
  
  # samping interval GPR = 0.8 ns
  Ts    <- dT*(10^(-9))     # [s] Sample time
  Fs    <- 1/Ts             # [Hz] Sampling frequency
  
  # cut-off frequency/ies fc in (MHz)
  f <- sort(f) * 10^6    # cut-off frequency in Hz
  
  # FIXME > write a function setFFilter(f, type=..., Fs)
  if(type == "low" || type == "high"){
    # Design the filter using the window method:
    if(length(f) > 1) {
      BW <- (f[2] - f[1])/Fs          # filter bandwidth
      fc <- f[1] + (f[2] - f[1])/2    # cut frequency
      L <- round(4 / BW)
      if(L %% 2 == 0){
        L <- L + 1  
      }
    }else if(length(f)==1){
      fc <- f[1]
    }
    h <- winSincKernel(L, fc/Fs, type)
  }else if(type == "bandpass"){
    if(length(f)==2 ) {
      h1 <- winSincKernel(L, f[1]/Fs, "low")
      h2 <- winSincKernel(L, f[2]/Fs, "high")
    }else if(length(f) == 4 ){
      BW <- (f[2] - f[1])/Fs
      fc <- f[1] + (f[2] - f[1])/2
      L <- round(4 / BW)
      if(L %% 2 == 0){
        L <- L + 1
      }
      h1 <- winSincKernel(L, fc/Fs, "low")
      BW <- (f[4] - f[3])/Fs
      fc <- f[3] + (f[4] - f[3])/2
      L <- round(4 / BW)
      if(L %% 2 == 0){
        L <- L + 1
      }
      h2 <- winSincKernel(L, fc/Fs, "high")
    }
    L = max(length(h1),length(h2))
    if(length(h2) < L ){
      h2 = c(rep(0,(L-length(h2))/2),h2,rep(0,(L-length(h2))/2))
    }
    if(length(h1) < L ){
      h1 = c(rep(0,(L-length(h1))/2),h1,rep(0,(L-length(h1))/2))
    }
    # change the band-reject filter kernel into a band-pass 
    h = -h1 - h2
    h[(L+1)/2] = h[(L+1)/2] + 1
  }
  
  # L <- length(h)
  
  # Choose the next power of 2 greater than L+nr-1 
  Nfft = 2^(ceiling(log2(L + nr-1)))
  # Zero pad the signal and impulse response:
  h_long = c( h, rep(0, Nfft - L) )
  A = rbind(as.matrix(A) , matrix(0,nrow=Nfft-nr,ncol=ncol(A)) )
  
  fft_A = stats::mvfft(A)    # signal
  fft_h = stats::fft(h_long)        # filter
  
  # Now we perform cyclic convolution in the time domain using 
  # pointwise multiplication in the frequency domain:
  Y = fft_A * fft_h
  
  pow_A = Mod(fft_A)
  pow_h = Mod(fft_h)
  pow_y = Mod(Y)
  # si matrix -> moyenne sur les colonnes
  if(!is.null(dim(A))){
    pow_A = apply(pow_A,1, mean, na.rm=TRUE)
    pow_y = apply(pow_y,1, mean, na.rm=TRUE)
  }
  # select only first half of vectors
  pow_A = pow_A[1:(Nfft/2+1)] 
  pow_y = pow_y[1:(Nfft/2+1)] 
  pow_h = pow_h[1:(Nfft/2+1)] 
  
  fre = Fs*(0:(Nfft/2))/Nfft/fac  #[MHz]
  
  if(plotSpec == TRUE){
    op <- par(no.readonly=TRUE)
    # plot the power spectrum
    m = seq(0,900,by=50)
    #par(mfrow=c(2,1), mar=c(5, 4, 4, 6) + 0.1 )
    par( mar=c(0, 4, 0.3, 2) + 0.1, oma=c(3,2,1,2) )
    plot(fre,pow_A, type="l",
         ylim=c(0,max(pow_A,pow_y)),
         ylab="power",lwd=2)
    lines(fre,pow_y, type="l",col="blue",lwd=2)
    par(new=TRUE)
    plot(fre, pow_h, type = "l", col = "red", yaxt = "n", ylab = "")
    legend("topright", c("input signal", "filter", "filtered signal"),
           col = c("black", "red", "blue"), lwd = c(2, 1, 2), bg = "white")
    abline(v = f/1000000, col = "grey", lty = 2)
    par(op)
  }
  a = (L-1)/2
  y = stats::mvfft(Y, inverse = TRUE)
  y = y[a:(a+nr-1),]/nrow(y)
  return(Re(y))
}

winSincKernel <- function(L, f, type = c("low", "high")){
  type = match.arg(type)  
  # if L is even (because L - filter length - must be odd)
  x = (-(L-1)/2):((L-1)/2)
  # low-pass
  h = hammingWindow(L) * sincMod(x,2*pi*f)  # h is our filter
  h = h/sum(h)
  # high-pass
  if(type == "high"){
    h = -h
    h[(L+1)/2] = h[(L+1)/2] + 1
  }
  return(h)
}

sincMod <- function(x,ff){
  r = length(x)
  n0 = which(x == 0)
  v = rep(0,r)
  ww <- c(1:(n0-1),(n0+1):r)
  #x = x[c(1:(n0-1),(n0+1):r)]
  v[ww] = sin(ff*x[ww])/(x[ww])
  v[n0] = ff
  return(v)
}
hammingWindow <- function(L){
  N = L-1
  n <- 0:N
  return(0.54 - 0.46*cos(2*pi*n/N))
}

# Choose the next power of 2 greater than L+M-1 
# Nfft = 2^(ceiling(log2(L+M-1)))  # -1)))    # or 2^nextpow2(L+M-1)
nextpower2 <- function(x){
  return(2^(ceiling(log2(x))))
}



# -------------------------------------------
# ------------addProfile3D--------------------------
# -------------------------------------------
# @name  addProfile3D (plot/add a 2D profile in rgl)
# @description read one or several DT1 file and plot/add 
# the 2D profiles in a rgl plot

# # @date 14.10.2013 15:15
# # @auteur Emanuel Huber
# require(rgl)
# # @require function load_install_package()
# source('load_install_package.R')
# cat('> Function(s) loaded: "load_install_package.R" ')
# # @require function normalizeGPR
# source('GPR_normalize.R')
# source('GPR_readDT1.R')
# source('GPR_gain.R')
# cat('"GPR_normalize.R"')
# cat('"GPR_readDT1.R" \n')
# cat('"GPR_gain.R" \n')

# @param [list]   LINE       (list containing several fPath of the DT1 file)
# @param [c(1)]   col=NULL   (palette of color)  
# @param [boolean]   plotNew=FALSE  (if true, open a new rgl window)
# @return void
# -------------------------------------------



# plot/add a 2D profile in rgl

# addProfile3D <- function(LINES, col=palGPR(n=101),plotNew=FALSE, normalize=TRU
# E, v=1, zlim=NULL, AGC=FALSE, sig=10){
# if(plotNew){
# # rgl.open()
# open3d()
# }
# for(i in seq_along(LINES)){
# #------------- READ DATA ------------------#
# lineName2   <- strsplit(LINES,split="[.]")
# lineName   <- lineName2[[i]][1]
# fileNameHD   <- paste(lineName,".HD",sep="")
# fileNameDT1 <- paste(lineName,".DT1",sep="")
# cat(basename(lineName),"\n")
# GPR <- readDT1(LINES[[i]])
# #------------- read data ------------------#

# myGPRdZ <- as.numeric(as.character(GPR$hd[7,2]))/as.numeric(as.character(
# GPR$hd[5,2]))
# HD <- GPR$dt1hd

# A <- GPR$data
# A[is.na(A)] <- 0
# if(!is.null(zlim)){
# sel <- seq(1, zlim/myGPRdZ/v,by=myGPRdZ)
# A <- A[sel,]
# }
# if(normalize){
# A <- normalizeGPR(A)
# }
# if(AGC){
# A <- apply(A,2,gain,sig=sig)
# }
# # example with GPR profile A 
# nr = nrow(A)
# nc = ncol(A)
# X <- matrix(HD$recx, ncol=nc, nrow=nr, byrow=TRUE)
# Y <- matrix(HD$recy, ncol=nc, nrow=nr, byrow=TRUE)
# Z <-  matrix(HD$topo, ncol=nc, nrow=nr, byrow=TRUE) - 
# matrix(myGPRdZ*v*(0:(
# nr-1)), ncol=nc, nrow=nr, byrow=FALSE)
# if(all(HD$topo==0)){
# warning("No topography \n")
# }
# if(all(HD$recx==0)){
# warning("No x-coordinates \n")
# }
# if(all(HD$recy==0)){
# warning("No y-coordinates \n")
# }
# A = (A-min(A))/(max(A)-min(A))
# Alim <- range(A)
# Alen <- Alim[2] - Alim[1] + 1

# # if(is.null(col))     col <- tim.colors(101)  # height color lookup table

# colA <- col[ (A)*100+1 ] # assign colors to heights for each point 
# rgl.surface(X, Y, Z, color=colA, back="fill", smooth = TRUE, lit=FALSE, 
# lwd=0) 
# # surface3d(X, Y, Z, color=colA, back="fill", smooth = FALSE, lit=FALSE, 
# lwd=0) 
# }
# }


#' Bytes to volt conversion
#'       
#' Convert bytes to volt values
#' @param Vmax length-one numeric vector: maximal nominal analog input voltage. 
#'             If \code{Vmax = NULL} it returns \code{1} (no bytes to volt 
#'             transformation)
#' @param Vmin length-one numeric vector: minimal nominal analog input voltage. 
#'             If missing, then \code{Vmin = -Vmax}.
#' @param nBytes Length-one numeric vector: number of bytes
#' @export
byte2volt <- function( Vmax = 50, Vmin = 50, nBytes = 16) {
  warnings("deprecated")
  if(is.null(Vmax)){
    return(1L)
  }else{
    if( missing(Vmin) ){
      Vmin <- -Vmax
    }
    return( abs(Vmax - Vmin) / ( 2^nBytes ) )
  }
}

#' Bytes to volt conversion
#'       
#' Convert bytes to volt values
#' @param Vmax length-one numeric vector: maximal nominal analog input voltage. 
#'             If \code{Vmax = NULL} it returns \code{1} (no bytes to volt 
#'             transformation)
#' @param Vmin length-one numeric vector: minimal nominal analog input voltage. 
#'             If missing, then \code{Vmin = -Vmax}.
#' @param nbits Length-one numeric vector: number of bits
#' @export
bits2volt <- function( Vmax = 50, Vmin = 50, nbits = 16) {
  if(is.null(Vmax)){
    return(1L)
  }else{
    if( missing(Vmin) ){
      Vmin <- -Vmax
    }
    return( abs(Vmax - Vmin) / ( 2^nbits ) )
  }
}

#-------------------------------------
#------- PRIVAT FUNCTION --------#




.minCommon10 <- function(xmin,xmax){
  xmin <- as.numeric(xmin)
  xmax <- as.numeric(xmax)
  D <- xmax-xmin
  n <- nchar(D)
  if( as.numeric(substr(xmin,nchar(xmin)-n+1,nchar(xmin))) + D < 10^(n)){
    return(as.numeric(substr(xmin,1,n+1))*10^(nchar(xmin)-n-1))
  }else{
    return(xmin)
  }
}








#========================================================#
#================= LOCAL ORIENTATION ====================#
#========================================================#

# -------------------------------------------
# ------------localOrientation--------------------------
# -------------------------------------------
# @name  localOrientation (Compute the local orientation)
# @description Compute the local orientation based on the tensor of the
# gradient field and on SVD decomposition.

# @date 04.09.2012 12:45
# @auteur Emanuel Huber

# @require source("convolution2D.R") -> tim.colors + image.plot
# source('convolution2D.R')
# cat('> Function(s) loaded: "convolution2D.R" \n')

# @return void
# -------------------------------------------

# return inv(A) %*% B.
# assume that
#      | a1  c1 |
#  A = |        |
#      | c1  b1 |
#
#      | a2  c2 |
#  B = |        |
#      | c2  b2 |

#' @export
invAxB <- function(a1,b1,c1,a2,b2,c2){
  D <- a1*b1 - c1*c1
  return(list( a11 = (a2*b1 - c1*c2)/D,
               a22 = (a1*b2 - c1*c2)/D,
               a12 = (b1*c2 - b2*c1)/D,
               a21 = (a1*c2 - a2*c1)/D))
}

# eigendecomposition tensor field.
# assume that
#      | a   d |
#  J = |       |
#      | d   b |

#' Eigendecomposition of 2x2 matrices
#'
#' @name eigenDecomp2x2SymMatrix 
#' @export
eigenDecomp2x2SymMatrix <- function(a,b,d){
  D <- sqrt((a-b)^2 + 4*d^2)
  #   l1 <- 0.5*(a+b + D)
  #   l2 <- 0.5*(a+b - D)
  #   u1x <- 0.5 *(a-b + D)
  #   u1y <- d
  #   u2x <- 0.5 *(a-b - D)
  #   u2y <- d
  return(list(l1 = 0.5*(a+b + D),
              l2 = 0.5*(a+b - D),
              u1x = 0.5 *(a-b + D),
              u1y = d,
              u2x = 0.5 *(a-b - D),
              u2y = d))
}

# return eigenvalues of A.
# assume that
#      | a11  a12 |
#  A = |          |
#      | a21  a22 |

#' @export
eigenValue2x2Mat <- function(a11,a12,a21,a22){
  D <- a11*a22 - a21*a12
  tr <- a11 + a22
  tr24d <- tr^2 - 4*D
  tr24d[abs(tr24d ) < .Machine$double.eps^0.75] <- 0
  return(list(l1 = 0.5 * (tr + sqrt(tr24d)),
              l2 = 0.5 * (tr - sqrt(tr24d))))
  
}

# return A^n
# assume that
#      | a   d |
#  A = |       |
#      | d   b |

#' @export
matPow <- function(a, b, d, n){
  eg <- eigenDecomp2x2SymMatrix(a, b, d)
  l1 <- eg$l1^n
  l2 <- eg$l2^n
  return(list(a11 = eg$u1x^2 * l1 + eg$u2x^2 * l2,
              a22 = eg$u1y^2 * l1 + eg$u2y^2 * l2,
              a12 = eg$u1x*eg$u1y * l1 + eg$u2x*eg$u2y * l2,
              a21 = eg$u1y*eg$u1x * l1 + eg$u2y*eg$u2x * l2))
}

#' @export
matProd2x2 <- function(a11, a12, a21, a22, b11, b12, b21, b22){
  return(list(a11 = a11*b11 + a12*b21,
              a12 = a11*b12 + a12*b22,
              a21 = a21*b11 + a22*b21,
              a22 = a21*b12 + a22*b22))
}

#' Distance between structure tensors
#'
#' @name distTensors
#' @rdname distTensors
#' @export
distTensors <- function(J1, J2, method=c("geodesic", "log-Euclidean",
                                         "angular", "L2"), normalise = FALSE){
  method <- match.arg(method, c("geodesic", "log-Euclidean", 
                                "angular", "L2"))
  if(normalise == TRUE){
    # J1[[1]] = J_xx
    # J1[[2]] = J_yy
    # J1[[3]] = J_xy
    J1 <- normTensor(J1[[1]], J1[[2]], J1[[3]])
    J2 <- normTensor(J2[[1]], J2[[2]], J2[[3]])
  }
  if(method == "geodesic"){
    return(distTensorGeod(J1[[1]], J1[[2]], J1[[3]], 
                          J2[[1]], J2[[2]], J2[[3]]))
  }else if(method == "log-Euclidean"){
    return(distTensorLogE(J1[[1]], J1[[2]], J1[[3]], 
                          J2[[1]], J2[[2]], J2[[3]]))
  }else if(method == "angular"){
    angle1 <- 1/2*atan2(2*J1[[3]], (J1[[1]] - J1[[2]]) ) + pi/2  
    angle2 <- 1/2*atan2(2*J2[[3]], (J2[[1]] - J2[[2]]) ) + pi/2
    return( (angle1 - angle2) )# %% pi )
  }else if(method == "L2"){
    return(  (J1[[1]] - J2[[1]])^2 + 
               2*(J1[[3]] - J2[[3]])^2 + 
               (J1[[2]] - J2[[2]])^2   )
  }
}
#

#' @export
normTensor <- function(a1,b1,c1){
  val_1 <- eigenValue2x2Mat(a1, c1, c1, b1)
  #   l1 <- (val_1$l1 + val_1$l2)
  l1 <- sqrt(val_1$l1^2 + val_1$l2^2)
  return(list(a1/l1, b1/l1,  c1/l1))
}

# geometric-based distance d g that measures the distance between two tensors
# J1 and J2 in the space of positive definite tensors 
# (based on Riemannian geometry)
# assume that
#       | a1   c1 |
#  J1 = |         |
#       | c1   b1 |
# 
#       | a2   c2 |
#  J2 = |         |
#       | c2   b2 |

#' @export
distTensorGeod <- function(a1,b1,c1,a2,b2,c2){
  ABA <- invAxB(a1,b1,c1,a2,b2,c2)
  #   A <- matPow(a1, b1, c1, -0.5)
  #   AB <- matProd2x2(A$a11, A$a12, 
  #                              A$a21, A$a22,
  #                              a2,c2,c2,b2)
  #   ABA <- matProd2x2(AB$a11, AB$a12, 
  #                     AB$a21, AB$a22,
  #                     A$a11, A$a12, 
  #                     A$a21, A$a22)
  val <- eigenValue2x2Mat(ABA$a11, ABA$a12, ABA$a21, ABA$a22)
  val$l1[val$l1 < 10^-100] <- 10^-100
  val$l2[val$l2 < 10^-100] <- 10^-100
  #   val$l1 <- 1
  #   val$l2 <- val$l2/val$l1
  return(sqrt(log(val$l1)^2 + log(val$l2)^2))
}

# log-Euclidean-based distance between two tensors
# J1 and J2 in the space of positive definite tensors 
# (based on Riemannian geometry)
# assume that
#       | a1   c1 |
#  J1 = |         |
#       | c1   b1 |
# 
#       | a2   c2 |
#  J2 = |         |
#       | c2   b2 |

#' @export
distTensorLogE <- function(a1,b1,c1,a2,b2,c2){
  a1[a1 < 10^-100] <- 10^-100
  a2[a2 < 10^-100] <- 10^-100
  b1[b1 < 10^-100] <- 10^-100
  b2[b2 < 10^-100] <- 10^-100
  c1[c1 < 10^-100] <- 10^-100
  c2[c2 < 10^-100] <- 10^-100
  a11 <- log(a1) - log(a2)
  a12 <- log(c1) - log(c2)
  #   a21 <- log(c1) - log(c2)
  a22 <- log(b1) - log(b2)
  tr <- a11^2 + 2*a12^2 + a22^2
  return(sqrt( tr))
  
}

# return structure tensor
#------------------------------
# Structure tensor field
# 
# name strucTensor
# rdname strucTensor

#' structure tensor for matrices
#'
#' @name strucTensor
#' @export
.strucTensor <- function(P, dxy = c(1, 1), mask = c(2, 2),
                         kBlur   = list(n = 3, m =  3, sd = 1), 
                         kEdge   = list(n = 7, m =  7, sd = 1), 
                         kTensor = list(n = 5, m = 10, sd = 2),
                         thresh=0.1, ...){
  n <- nrow(P)
  m <- ncol(P)
  
  #-------------------------------------------------
  #- Identify ridge-like regions and normalise image
  #-------------------------------------------------
  # normalization (mean = 0, sd = 1)
  
  # apply standard deviation block-wise
  if(!is.null(mask)){
    if(length(mask)==2 && is.null(dim(mask))){
      blksze2 <- round(mask/2)
      Pn <- (P - mean(P, na.rm = TRUE))/sd(as.vector(P), na.rm = TRUE)
      nseq <- c(seq(1, n, mask[1]),n)
      mseq <- c(seq(1, m, mask[2]),m)
      P_sd <- matrix(NA, nrow = n, ncol = m)
      for(i in seq_len(n)){
        for(j in seq_len(m)){
          nv <- (i - blksze2[1]):(i + blksze2[1])
          mv <- (j - blksze2[2]):(j + blksze2[2])
          nv <- nv[nv <= n & nv > 0]
          mv <- mv[mv <= m & mv > 0]
          std <- sd(array(Pn[nv,mv]),na.rm=TRUE)
          P_sd[nv,mv] <- std
        }
      }
      mask <- P_sd < thresh
    }else{
      # P <- (P - mean(P[!mask], na.rm = TRUE))/
      #        sd(as.vector(P[!mask]), na.rm = TRUE)
    }
  }else{
    mask <- matrix(FALSE, nrow = n, ncol = m)
  }
  
  #------------------------------
  #- Determine ridge orientations
  #------------------------------
  #  BLURING
  if(!is.null(kBlur)){
    k <- do.call( gkernel, kBlur)
    P_f  <- convolution2D(P, k)
  }else{
    P_f <- P
  }
  
  # GRADIENT FIELD
  # window size for edge dectection
  kdx <- do.call( dx_gkernel, kEdge)
  kdy <- do.call( dy_gkernel, kEdge)
  vx <- convolution2D(P_f, kdx)/dxy[1]
  vy <- convolution2D(P_f, kdy)/dxy[2]
  
  # local TENSOR
  Gxx <- vx^2
  Gyy <- vy^2
  Gxy <- vx*vy 
  
  # LOCAL AVERAGED TENSOR
  kg <- do.call(gkernel, kTensor)
  Jxx  <- convolution2D(Gxx, kg)
  Jyy  <- convolution2D(Gyy, kg)
  Jxy  <- convolution2D(Gxy, kg)
  Jxx[Jxx < .Machine$double.eps^0.75] <- 0
  Jyy[Jyy < .Machine$double.eps^0.75] <- 0
  
  
  # polar parametrisation
  energy <- Jxx + Jyy                               # energy
  anisot  <- sqrt((Jxx-Jyy)^2 + 4*(Jxy)^2)/energy   # anisotropy
  orient <- 1/2*atan2(2*Jxy, (Jxx - Jyy) ) + pi/2   # orientation
  mask2 <- mask | is.infinite(energy) | is.infinite(anisot)
  anisot[mask2] <- 0
  energy[mask2] <- 0
  orient[mask2] <- 0
  
  # ANALYTIC SOLUTION BASED ON SVD DECOMPOSITION
  Jeig <- eigenDecomp2x2SymMatrix(Jxx, Jyy, Jxy)
  Jeig$u1x[mask2] <- 0
  Jeig$u1y[mask2] <- 0
  Jeig$u2x[mask2] <- 0
  Jeig$u2y[mask2] <- 0
  Jeig$l1[mask2] <- 0
  Jeig$l2[mask2] <- 0
  mode(mask2) <- "integer"
  
  # APPLY MASK TO local tensor and structure tensor
  Gxx[mask2] <- 0
  Gyy[mask2] <- 0
  Gxy[mask2] <- 0 
  Jxx[mask2] <- 0
  Jyy[mask2] <- 0
  Jxy[mask2] <- 0
  
  return(list(tensor  = list("xx" = Jxx,
                             "yy" = Jyy,
                             "xy" = Jxy),
              gradtens = list("xx" = Gxx,
                              "yy" = Gyy,
                              "xy" = Gxy),
              vectors = list("u1x" = Jeig$u1x,
                             "u1y" = Jeig$u1y,
                             "u2x" = Jeig$u2x,
                             "u2y" = Jeig$u2y),
              values  = list("l1" = Jeig$l1,
                             "l2" = Jeig$l2),
              polar   = list("energy" = energy,
                             "anisotropy" = anisot,
                             "orientation" = orient),
              mask = mask2))
}

#' Plot structure tensor on GPR data
#' 
#' @name plotTensor
#' @rdname plotTensor
#' @export
plotTensor <- function(x, O, type=c("vectors", "ellipses"), normalise=FALSE,
                       spacing=c(6,4), len=1.9, n=10, ratio=1,...){
  type <- match.arg(type, c("vectors", "ellipses"))
  n <- nrow(x)
  m <- ncol(x)
  
  len1 <- len*max(spacing*c(x@dx,x@dz));  # length of orientation lines
  
  # Subsample the orientation data according to the specified spacing
  v_y = seq(1,(m),by=spacing[1])
  v_x = seq(1,(n), by=spacing[2])
  
  # Determine placement of orientation vectors
  if(length(x@coord)>0){
    xvalues <- posLine(x@coord)
  }else{
    xvalues <- x@pos
  }
  X = matrix(xvalues[v_y],nrow=length(v_x),ncol=length(v_y),byrow=TRUE)
  Y = matrix(x@depth[v_x],nrow=length(v_x),ncol=length(v_y),byrow=FALSE)
  
  angle = O$polar$orientation[v_x, v_y];
  l1 <- O$values[[1]][v_x, v_y]
  l2 <- O$values[[2]][v_x, v_y]
  
  if(type == "vectors"){
    #Orientation vectors
    dx0 <- cos(angle)
    dy0 <- sin(angle)
    normdxdy <- 1
    if(isTRUE(normalise)){
      normdxdy <- sqrt(dx0^2 + dy0^2)
    }
    dx <- len1*dx0/normdxdy/2
    dy <- len1*dy0/normdxdy/2*ratio
    segments(X - dx, (Y - dy), X + dx , (Y + dy))#,...)
  }else if(type == "ellipses"){
    l1[l1 < 0] <- 0
    l2[l2 < 0] <- 0
    a <- 1/sqrt(l2)
    b <- 1/sqrt(l1)
    for(i in seq_along(v_x)){
      for(j in seq_along(v_y)){
        aij <- ifelse(is.infinite(a[i,j]), 0, a[i,j])
        bij <- ifelse(is.infinite(b[i,j]), 0, b[i,j])
        maxab <- 1
        if(isTRUE(normalise)){
          maxab <- max(aij, bij)
        }
        if(!(aij == 0 & bij == 0)){
          E <- RConics::ellipse(saxes = c(aij/maxab, bij*ratio/maxab)*len1,
                                loc   = c(X[i,j], Y[i,j]), 
                                theta = angle[i,j], n=n)
          polygon(E, ...)
        }
      }
    }
  }
}


#' Plot structure tensor on GPR data
#' 
#' @name plotTensor0
#' @rdname plotTensor0
#' @export
plotTensor0 <- function(alpha, l1, l2,  x, y, col = NULL ,
                        type=c("vectors", "ellipses"), normalise=FALSE,
                        spacing=c(6,4), len=1.9, n=10, ratio=1,...){
  type <- match.arg(type, c("vectors", "ellipses"))
  alpha <- t(alpha[nrow(alpha):1, ])
  l1 <- t(l1[nrow(l1):1, ])
  l2 <- t(l2[nrow(l2):1, ])
  
  n <- nrow(alpha)
  m <- ncol(alpha)
  
  dxy <- c(mean(diff(x)), mean(diff(y)))
  
  len1 <- len*max(spacing * dxy);  # length of orientation lines
  
  # Subsample the orientation data according to the specified spacing
  v_x = seq(spacing[1],(n-spacing[1]),by=spacing[1])
  v_y = seq(spacing[2],(m-spacing[2]), by=spacing[2])
  
  # Determine placement of orientation vectors
  #   xpos <- seq(0, by = dxy[1], length.out = n)
  #   ypos <- seq(0, by = dxy[2], length.out = m)
  xsub <- x[v_x]
  ysub <- y[v_y]
  X = matrix(xsub,nrow=length(v_x),ncol=length(v_y),byrow=FALSE)
  Y = matrix(ysub,nrow=length(v_x),ncol=length(v_y),byrow=TRUE)
  
  #   angle <- O$polar$orientation[v_x, v_y]
  angle <- alpha[v_x, v_y]
  #   l1 <- O$values[[1]][v_x, v_y]
  l1 <- l1[v_x, v_y]
  #   l2 <- O$values[[2]][v_x, v_y]
  l2 <- l2[v_x, v_y]
  
  if(type == "vectors"){
    #Orientation vectors
    dx0 <- sin(angle)
    dy0 <- cos(angle)
    normdxdy <- 1
    if(isTRUE(normalise)){
      normdxdy <- sqrt(dx0^2 + dy0^2)
    }
    dx <- len1*dx0/normdxdy/2
    dy <- len1*dy0/normdxdy/2*ratio
    segments(X - dx, (Y - dy), X + dx , (Y + dy),...)
  }else if(type == "ellipses"){
    if(is.null(col)){
      col <- colorspace::heat_hcl(101, c = c(80, 30), l = c(30, 90), 
                                  power = c(1/5, 2))
    }
    
    #     l1[l1 < .Machine$double.eps] <- .Machine$double.eps
    #     l2[l2 < .Machine$double.eps] <- .Machine$double.eps
    l1[l1 < 0] <- 0
    l2[l2 < 0] <- 0
    b <- 1/sqrt(l2)
    a <- 1/sqrt(l1)
    lcol <- l1 + l2
    lcol <- 1+(lcol - min(lcol))/(max(lcol)-min(lcol)) * 100
    #     normdxdy <- matrix(max(a[is.finite(a)],b[is.finite(b)]),
    normdxdy <- matrix(max(a[is.finite(a)]),
                       nrow=length(v_x),ncol=length(v_y))
    if(isTRUE(normalise)){
      normdxdy <- b
    }
    for(i in seq_along(v_x)){
      for(j in seq_along(v_y)){
        aij <- ifelse(is.infinite(a[i,j]), 0, a[i,j])
        bij <- ifelse(is.infinite(b[i,j]), 0, b[i,j])
        if(isTRUE(normalise)){
          maxab <- max(aij, bij)
          if(maxab != 0){
            aij <- aij/maxab
            bij <- bij/maxab
            cat(i,".",j,"  > a =",aij, "  b =", bij, "\n")
            #           normdxdy <- b
          }
        }
        if(!(aij == 0 & bij == 0)){
          E <- RConics::ellipse(saxes = c(aij, bij*ratio)*len1,
                                #           /normdxdy[i,j], 
                                loc   = c(X[i,j], Y[i,j]), 
                                theta = pi/2 - angle[i,j], n=n)
          polygon(E, col=col[lcol[i,j]])
        }
      }
    }
  }
}


#   image2DN(x@data)
#    X = matrix(v_y,nrow=length(v_x),ncol=length(v_y),byrow=TRUE)
#   Y = matrix(v_x,nrow=length(v_x),ncol=length(v_y),byrow=FALSE)
#   for(i in seq_along(v_x)){
#     for(j in seq_along(v_y)){
#       E <- ellipse(saxes = c(l1[i,j], l2[i,j])/max(m,n)/5, 
#                    loc   = c(X[i,j]/m, 1-Y[i,j]/n), 
#                    theta = -angle[i,j], n=10)
#       lines(E, lwd=0.5)
#     }
#   }
#   for(i in seq_along(v_x)){
#     for(j in seq_along(v_y)){
#       E <- ellipse(saxes = c(l1[i,j], l2[i,j])/max(l1,l2)*len1, 
#                    loc   = c(X[i,j], Y[i,j]), 
#                    theta = -angle[i,j], n=10)
#       lines(E, lwd=0.5)
#     }
#   }


#-----------------
#-----------------


# n = nrow
# m = mrow
#' Gaussian 2d-kernel
#'
# sigma = sd
#' @name gkernel
#' @rdname kernels
#' @export
gkernel <- function(n, m, sd=1){
  n <- ifelse(n %% 2 == 0, n + 1, n)
  m <- ifelse(m %% 2 == 0, m + 1, m)
  siz <- (n - 1)/2;
  y <- matrix(-siz:siz, nrow = n, ncol = m)
  siz <- (m - 1)/2;
  x <- matrix(-siz:siz, nrow = n, ncol = m, byrow = TRUE)
  g <- exp(-(x^2+y^2)/(2*sd^2))
  sumg <- sum(g)
  if(sumg != 0){
    return( g/sumg )
  }else{
    return( g )
  }
}

#' Gaussian x-derivative kernel (edge detector)
#'
#' @name dx_gkernel
#' @rdname kernels
#' @export
dx_gkernel <- function(n, m, sd=1){
  n <- ifelse(n %% 2 == 0, n + 1, n)
  m <- ifelse(m %% 2 == 0, m + 1, m)
  siz <- (n - 1)/2;
  y <- matrix(-siz:siz, nrow = n, ncol = m)
  siz <- (m - 1)/2;
  x <- matrix(-siz:siz, nrow = n, ncol = m, byrow = TRUE)
  g <- x*exp(-(x^2+y^2)/(2*sd^2))
  return(g)
  
}

#' Gaussian y-derivative kernel (edge detector)
#'
#' @name dy_gkernel
#' @rdname kernels
#' @export
dy_gkernel <- function(n, m, sd=1){
  n <- ifelse(n %% 2 == 0, n + 1, n)
  m <- ifelse(m %% 2 == 0, m + 1, m)
  siz <- (n - 1)/2;
  y <- matrix(-siz:siz, nrow = n, ncol = m)
  siz <- (m - 1)/2;
  x <- matrix(-siz:siz, nrow = n, ncol = m, byrow = TRUE)
  g <- y*exp(-(x^2+y^2)/(2*sd^2))
  return(g)
}



#' Displacement to align two matrix
#'
#' Displacement to align two matrix
#' @export
displacement <- function(x, y, method=c("phase", "WSSD"), dxy = NULL){
  nm <- c(max(nrow(x), nrow(y)), max(ncol(x), ncol(y)))
  #--- weighted sum of squared differences
  if(method == "WSSD"){
    nmx <- dim(x)
    nmy <- (dim(x) + 2*nm - dim(y))/2
    x0 <- paddMatrix(x, nm[1], nm[2], zero=TRUE)
    y0 <- paddMatrix(y, nmy[1], nmy[2], zero=TRUE)
    # weights (zero outside the image range)
    wx <- matrix(1, nrow = nrow(x), ncol = ncol(x))
    wx <- paddMatrix(wx, nm[1], nm[2], zero = TRUE)
    wy <- matrix(1,nrow = nrow(y), ncol = ncol(y))
    wy <- paddMatrix(wy, nmy[1], nmy[2], zero = TRUE)
    WX <- fft(wx)
    WY <- fft(wy)
    X  <- fft(wx * x0)
    Y  <- fft(wy * y0)
    X2  <- fft(wx * x0^2)
    Y2  <- fft(wy * y0^2)
    WSSD <- Mod(fft(WX * Conj(Y2) + X2 * Conj(WY) - 2 * X * Conj(Y), 
                    inverse = TRUE))
    WSSD <- WSSD[1:nm[1] , 1:nm[2]]
    d <- which(WSSD == max(WSSD), arr.ind = TRUE)[1,]
    #--- phase correlation
  }else if(method == "phase"){
    #     n <- min(nrow(x), nrow(y))
    #     m <- min(ncol(x), ncol(y))
    #     X <- fft(x[1:n, 1:m])
    #     Y <- fft(y[1:n, 1:m])
    x0 <- padmat(x, n = nm[1], m = nm[2])
    y0 <- padmat(y, n = nm[1], m = nm[2])
    X <- fft(x0)
    Y <- fft(y0)
    R <- Mod(fft( X * Conj(Y) / (Mod(X*Y)), inverse = TRUE))
    #     R <- R[1:nm[1] , 1:nm[2]]
    if(!is.null(dxy)){
      R <- R[c(1:(dxy[1] + 1), (nm[1] - dxy[1]+1):nm[1]), 
             c(1:(dxy[2] + 1), (nm[2] - dxy[2]+1):nm[2]), drop = FALSE]
      nm <- 2*dxy
    }
    d <- which(R == max(R), arr.ind = TRUE)[1,] - 1
    if(d[1] > nm[1]/2) d[1] <- d[1] - nm[1]
    if(d[2] > nm[2]/2) d[2] <- d[2] - nm[2]
  }
  return(d)
}


#' shift a matrix by n and m
#'     
#' shift a matrix by n and m
#' @export
shiftmat <- function(x, n, m){
  xs <- matrix(0,nrow=nrow(x), ncol=ncol(x))
  vx0 <- 1:nrow(xs) + n
  vx0 <- vx0[vx0 > 1 & vx0 <= nrow(xs)]
  vx1 <- vx0 - n
  vy0 <- 1:ncol(xs) + m
  vy0 <- vy0[vy0 > 1 & vy0 <= ncol(xs)]
  vy1 <- vy0 - m
  xs[vx0, vy0] <- x[vx1, vy1]
  return(xs)
}

#' pad a matrix
#' 
#' pad a matrix
#' @export
padmat <- function(x, n, m, what = 0){
  x0 <- matrix(what, ncol=m, nrow=n)
  x0[1:nrow(x),1:ncol(x)] <- x
  return(x0)
}

# pads the edges of an image to minimize edge effects 
# %during convolutions and Fourier transforms. 
# %Inputs %I - image to pad 
# %p - size of padding around image 
# %Output %Ipad - padded image 
# SOURCE: http://matlabgeeks.com/tips-tutorials/how-to-blur-an-image-with-a-
# fourier-transform-in-matlab-part-i/
# service@matlabgeeks.com i
paddMatrix <- function(I, p1, p2=NULL, zero = FALSE){
  if(is.null(p2)){
    p2 <- p1
  }
  nI <- nrow(I)
  mI <- ncol(I)
  Ipad <- matrix(0, nrow = nI + 2*p1, ncol = mI + 2*p2)
  # middle
  Ipad[(p1+1):(p1+nI),(p2+1):(p2+mI)] <- I
  # top and bottom
  if(zero == FALSE){
    Ipad[1:p1,(p2+1):(p2+mI)] <- repmat(I[1,,drop=FALSE], p1, 1)
    Ipad[(p1+nI+1):(nI+2*p1), (p2+1):(p2+mI)] <- repmat(I[nI,,drop=FALSE], 
                                                        p1, 1)
  }
  if(p2 > 0 && zero == FALSE){
    # left and right
    Ipad[(p1+1):(p1+nI), 1:p2] <- repmat(I[,1,drop=FALSE], 1, p2)
    Ipad[(p1+1):(p1+nI), (p2+mI+1):(mI + 2*p2)] <- 
      repmat(I[,mI, drop=FALSE],1,p2)
    # corner
    Ipad[1:p1, 1:p2] <- I[1,1]
    Ipad[(p1+nI+1):(nI+2*p1), (p2+mI+1):(mI+2*p2)] <- I[nI,mI]
    Ipad[1:p1,(p2+mI+1):(mI + 2*p2)] <- I[1,mI]
    Ipad[(p1+nI+1):(nI+2*p1), 1:p2] <- I[nI,1]
  }
  return(Ipad)
}



#' Repeat matrix
#'
#' Repeat a matrix row-wise n times and column-wise m times.
#' 
#' Source
#' A replication of MatLab repmat function!
#' R FOR OCTAVE USERS
#' version 0.4, Copyright (C) 2001 Robin Hankin
#' http://cran.r-project.org/doc/contrib/R-and-octave.txt
#' @name repmat
#' @rdname repmat
#' @export
repmat <- function(A,n,m) {
  kronecker(matrix(1,n,m),A)
}



#---------------- CONVOLUTION --------------------#

# # linear convolution with fft
# # a = vector
# # b = vector
# convolution <- function(a,b){
#   na <- length(a)
#   nb <- length(b)
#   L <- na + nb - 1
#   a0 <- c(a,rep(0,nb-1))
#   b0 <- c(b, rep(0,na-1))
#   y <- Re(fft(fft(a0)*fft(b0),inverse=TRUE))/L
#   return(y[1:(max(na,nb))])
# }


#' Linear convolution based on FFT
#'
#' If A (or B) is a numeric vector, it is converted into a one-column 
#' matrix. Then if A and B do not have the same number of column, then the 
#' first column of the matrix with the smallest number of column is repeated to
#' match the dimension of the other matrix.
#' match the dimension of the other matrix.
#' @param A A numeric vector or matrix.
#' @param k B numeric vector or matrix.
#' @name convolution
#' @rdname convolution
#' @export
convolution <- function(A,k){
  if(is.null(dim(A))){
    dim(A) <- c(length(A),1)
  }
  if(is.null(dim(k))){
    dim(k) <- c(length(k),1)
  }
  if(ncol(k) < ncol(A)){
    k <- repmat(k[,1, drop = FALSE], 1, ncol(A)) 
  }
  #   else if(ncol(B) > ncol(A)){
  #     A <- repmat(A[,1, drop = FALSE], 1, ncol(B)) 
  #   }
  nA <- nrow(A)
  nk <- nrow(k)
  Apad <- paddMatrix(A, nk, 0)
  k0 <- matrix(0, nrow = nrow(Apad), ncol= ncol(Apad))
  k0[1:nk, ] <- k
  #   B0 <- rbind(B0, B, B0)
  Y <- Re(stats::mvfft(stats::mvfft(Apad) * stats::mvfft(k0), 
                       inverse=TRUE))/nrow(Apad)
  return(Y[1:nA + nk + nk/2 + 1, ])
}


# cf. matlab
# A convolution matrix is a matrix, formed from a vector, 
# whose product with another vector 
# is the convolution of the two vectors.

# A = convmtx(y,nf) returns the convolution matrix, A, 
# such that the product of A and a vector, x, 
# is the convolution of y and x. 
# If y is a column vector of length m, A is (m+nf-1)-by-nf and the 
# product of A and a column vector, x, of length n is the 
# convolution of y and x. 
convmtx <- function(y, nf){
  ny <- length(y)
  L <- nf + ny -1
  # convolution matrix Y
  yext <- rep(c(y,rep(0,L-ny+1)),nf)
  yext <- yext[1:(L*nf)]
  return( matrix(yext,nrow=L,ncol=nf))
}










# version vectoriel!!!!
#' Return points that are within a polygon
#' 
#' Return points that are within a polygon
#' @export
inPoly <- function(x, y, vertx, verty){
  inPo <- rep(0L, length(x))
  nvert <- length(vertx)
  for(i in 1:nvert){
    j <- ifelse(i==1, nvert,i-1)
    myTest <- ((verty[i] > y) != (verty[j]>y)) &
      (x < (vertx[j]-vertx[i]) * (y-verty[i]) / 
         (verty[j]-verty[i]) + vertx[i])
    inPo[myTest] <- !inPo[myTest]
  }
  return(inPo)
}






.FKFilter <- function(A, fk, L = c(5, 5), npad=1){
  nr <- nrow(A)  # time  
  nc <- ncol(A)  # x  
  
  #============== PLOT F-K SPECTRUM ===============#
  # padding (try also 2*(2^nextpow2(nc))
  nk <- npad*(nextpower2(nc))
  nf <- npad*(nextpower2(nr))
  A1 <- matrix(0,nrow=nf,ncol=nk)
  A1[1:nr,1:nc] <- A
  
  # function to center the spectrum!! (no need of fttshift!)
  #centres spectrum: Gonzalez & Wintz (1977) Digital Image Processing p.53
  # A1  <- A1 * (-1)^(row(A1) + col(A1))
  A1_fft <- stats::fft(A1)
  
  # plotGPR(Mod(A1_fft)^0.05)
  # plotGPR(Re(fft(A1_fft,inv=TRUE))[1:nr,1:nc])
  # plotGPR(A)
  
  #============== FILTER F-K SPECTRUM ===============#
  myFlong <- matrix(0,nrow=nf,ncol=nk)
  myFlong[1:(nf/2),1:(nk/2)] <- fk[(nf/2):1,(nk/2):1]
  # myFlong  <- myFlong * (-1)^(row(myFlong) + col(myFlong))
  myFlong[(nf/2+1):(nf),(nk/2 + 1):nk] <- fk[1:(nf/2),1:(nk/2)]
  myFlong[1:(nf/2),(nk/2 + 1):nk] <- fk[(nf/2):1,(nk):(nk/2 + 1)]
  # myFlong[(nf/2+1):(nf),1:(nk/2)] <- fk[1:(nf/2),(nk/2 + 1):(nk)]
  myFlong[(nf/2+1):(nf),1:(nk/2)] <- fk[1:(nf/2),(nk/2 + 1):nk]
  # plotGPR(myFlong)
  
  
  # hamming window
  if(length(L)==1) L <- c(L,L)
  if(all(L!=0)){
    ham2D <- hammingWindow(L[1])%*%t(hammingWindow(L[2]))
    ham2Dlong <- matrix(0,nrow=nf,ncol=nk)
    ham2Dlong[1:L[1],1:L[2]] <- ham2D
    # plotGPR(ham2Dlong)
    FF <-  Re(stats::fft(stats::fft(myFlong) * stats::fft(ham2Dlong),inv=TRUE))
  }else{
    FF <- myFlong
  }
  FF <- FF/sum(FF)
  
  # plotGPR(Re(fft(fft(myFlong) * fft(ham2Dlong),inv=TRUE))[1:nr,1:nc])
  
  A_back <- Re(stats::fft(A1_fft * FF,inv=TRUE))[1:nr,1:nc]
  # plotGPR(A_back)
  # plotGPR(A_back)
  # scaling
  return(A_back/(max(A_back)-min(A_back))*(max(A)-min(A)))
}

#------------------------------------------------------------------------------#

# used in function readGPS()
#' @export
extractPattern <- function(x, pat, shift1 = 0, shift2 = 0){
  # pat_tr <- "(\\#[0-9]+)"
  matches <- regexpr(pat, x, perl=TRUE)
  first <- attr(matches, "capture.start") + shift1
  last <- first + attr(matches, "capture.length") + shift2
  return(mapply(substring, x, first, last, USE.NAMES = FALSE))
}









rmNaCol <- function(x){
  # remove NA columns
  rmCol <- which(apply(x, 2, function(x) sum(is.na(x))) > 0)
  if(length(rmCol) > 0)    x <- x[, - rmCol]
  return(x)
}


# readVOL <- function(fPath){
#   if( inherits(fPath, "connection") ){
#     x <- fPath
#   }else if(is.character(fPath)){
#     fName <- getFName(fPath, ext = c(".vol"))
#     x <- file(fName$vol , "rb")
#   }
#   hd <- c()
#   
#   #================================ HEADER ======================================#
#   # The header consists of at least 60 bytes of binary data. 
#   # Each field in the header is a 32 bit (4 byte) word in 
#   # network byte order (“big endian”), making a total of 
#   # at least 15 header words. This implies that the byte order has to be swapped 
#   # to read the values on an Intel-based PC.
#   
#   # 0 Magic token. This is always 192837465 (decimal)
#   hd$magic_token <- readBin(x, what = "integer", size = 4, endian = "big")
#   # 1 Header size in bytes, including the magic token and size fields
#   hd$header_size <- readBin(x, what = "integer", size = 4, endian = "big")
#   # 2 The size of the 3d matrix size in the z dimension
#   hd$z_dim <- readBin(x, what = "integer", size = 4, endian = "big")
#   # 3 The size of the 3d matrix size in the y dimension
#   hd$y_dim <- readBin(x, what = "integer", size = 4, endian = "big")
#   # 4 The size of the 3d matrix size in the x dimension
#   hd$x_dim <- readBin(x, what = "integer", size = 4, endian = "big")
#   # 5 Bits per sample. This should always be 64 for radar data
#   hd$bits <- readBin(x, what = "integer", size = 4, endian = "big")
#   # reserved bits
#   seek(x, where = 40, origin = "start")
#   # 10 Major file format version
#   hd$major_vers <- readBin(x, what = "integer", size = 4, endian = "big")
#   # 11 Minor file format version
#   hd$minor_vers <- readBin(x, what = "integer", size = 4, endian = "big")
#   # 12 File format revision number
#   hd$rev <- readBin(x, what = "integer", size = 4, endian = "big")
#   
#   
#   # These two words define the file offset and size of a block of XML data 
#   # in 8 bit ASCII that define further metadata for the volume file.
#   seek(x, where = 60, origin = "start")
#   if(hd$header_size >= 68){
#     # 15 XML data file offset
#     hd$xml_fo <- readBin(x, what = "integer", size = 4, endian = "big")
#     # 16 XML data size
#     hd$xml_size <- readBin(x, what = "integer", size = 4, endian = "big")
#     
#     seek(x, where = hd$xml_fo, origin = "start")
#     hd$XML <- readBin(x, what = "character", n = 1, size = 1, endian = "big")
#     
#     data <- XML::xmlParse(hd$XML)
#     
#     els <- XML::getNodeSet(data, "//MetadataDictionary/entry[@name]")
#     if(length(els) > 0){
#       metaD <- sapply(els, function(el) XML::xmlGetAttr(el, "value"))
#       names(metaD) <- sapply(els, function(el) XML::xmlGetAttr(el, "name"))
#       hd$meta_cst <- metaD
#     }
#     
#     els2 <- XML::getNodeSet(data, "//meta-data[@DataDomainType]")
#     if(length(els2) > 0){
#       metaD2 <- XML::xmlAttrs(els2[[1]])
#       if(length(metaD2) > 0 && !is.null(metaD2)){
#         if(!is.null(metaD2["DeltaValueZ"])){
#           hd$dz   <- as.numeric(metaD2["DeltaValueZ"])
#         }
#         if(!is.null(metaD2["MinValueZ"])){
#           hd$zmin <- as.numeric(metaD2["MinValueZ"])
#         }
#         if(!is.null(metaD2["MaxValueZ"])){
#           hd$zmax <- as.numeric(metaD2["MaxValueZ"])
#         }
#       }
#     }
#   }
#   
#   #================================ Binary Data =================================#
#   seek(x, where = hd$header_size , origin = "start")
#   XYZ_dim <- c(hd$x_dim, hd$y_dim, hd$z_dim)
#   test <- which(XYZ_dim == 1)
#   if(length(test) > 0){
#     hd$dim <- "2D"
#     XYZ_dim <- XYZ_dim[-test]
#     XYZ <- array(dim = XYZ_dim)
#     for(i in 1:XYZ_dim[1]){
#       for(j in 1:XYZ_dim[2]){
#         XYZ[i,j] <-  readBin(x, what = "numeric", size = 4, endian = "big")
#       }
#     }
#   }else{
#     hd$dim <- "3D"
#     XYZ <- array(dim = XYZ_dim)
#     for(k in seq_len(hd$z_dim)){
#       for(i in seq_len(hd$x_dim)){
#         for(j in seq_len(hd$y_dim)){
#           XYZ[i,j,k] <- readBin(x, what = "numeric", size = 8, endian = "big")
#           realPart <- readBin(x, what = "integer", size = hd$bits/8/2, endian = "big")
#           imagPart <- readBin(x, what = "integer", size = hd$bits/8/2, endian = "big")
#           XYZ[i,j,k] <- complex(real = realPart,
#                                 imaginary = imagPart)
#         }
#       }
#     }
#   }
#   
#   if( !inherits(fPath, "connection") ){
#     close(x)
#   }
#   
#   return(list(hd = hd, data = XYZ))
# }

# # Data is stored in 16 bits as raw data. The only parameters that affect the 
# # recorded data directly are Tsweep and Read.  The other parameters affect the 
# # display and may be varied during or after completion of the survey 
# # (see sections 5.3 and 6.3).  
# # The data is stored under the run name as RUNNAME.dat. The run details are 
# # stored in the file RUNNAME.hdr.  The stored data format is 2 bytes per point 
# # with LSB byte followed by MSB byte. There are 256 points (512 bytes) 
# # followed by 1 byte of marker (ASCII).
# # In addition to the data and header files, GPS files (.gps) and gps number 
# # files (.gpt) are generated, irrespective of whether or not a GPS is used.  
# # If a GPS is not used, the .gps and .gpt files will be 0kB in size.
# # The HDR file is an ASCII file (can be read using notepad) that contains the 
# # radar parameters and notes about the run.
# 
# readUtsi <- function(dsn, dsn2 = NULL){
#   
#   if( inherits(dsn, "connection") ){
#     if(!inherits(dsn2, "connection")){
#       stop("Please add an additional connection to 'readGPR()' for ",
#            "the header file '*.hdr'")
#     }
#   }else if(is.character(dsn) && is.null(dsn2)){
#     fName <- getFName(dsn, ext = c(".hdr", ".dat"))
#     # open dt1 file
#     dsn  <- file(fName$dat , "rb")
#     dsn2 <- file(fName$hdr , "rb")
#   }else{
#     if(!file.exists(dsn)){
#       stop("File ", dsn, " does not exist!")
#     }
#     if(!file.exists(dsn2)){
#       stop("File ", dsn2, " does not exist!")
#     }
#     dsn_save <- c(dsn, dsn2)
#     dsn  <- file(dsn_save[grepl("(\\.dat)$", dsn_save)], "rb")
#     dsn2 <- dsn_save[grepl("(\\.hdr)$", dsn_save)]
#   }
#   
#   hd <- readUtsiHDR(dsn2) 
#   z <- readUtsiDat(dsn, splPerScan = hd$splPerScan, bits = hd$bits)
#   z[["hd"]] <- hd
#   close(dsn)
#   close(dsn2)
#   return(z)
#   
# }
# 
# readUtsiHDR <- function(con){
#   hd <- c()
#   
#   seek(con, where = 0, origin = "start")
#   #------------------ Utsi header *.hdr -----------------------------------------#
#   u <- readBin(con, what = "raw", n = 2, size = 1)
#   hd$magic_number <- sf::rawToHex(u)
#   if(hd$magic_number != "0f20"){
#     message("Magic number in '", 
#             summary.connection(con)$description, 
#             "' is '",
#             hd$magic_number,
#             "' instead of '0f20'")
#   }
#   u <- readLines(con, n = 1)
#   u <- strsplit(u, split = ", ")[[1]]
#   hd$software_version <- u[1]
#   hd$software_date <- u[2]
#   
#   # scan(con, what = "character", n = 1, skipNul = TRUE)
#   
#   # u <- readLines(con, n = 1, skipNul = TRUE, warn = FALSE)
#   u <- readBin(con, what = "character", n = 1)
#   hd$date <- as.Date(u[1], "%d\\%m\\%y")
#   
#   invisible(readBin(con, what = "character", n = 1))
#   u <- readBin(con, what = "character", n = 1)
#   u <- strsplit(gsub("\005", "", u), " ")[[1]]
#   hd$time <- u[1]
#   hd$site_text <- u[2]
#   
#   
#   invisible(readBin(con, what = "character", n = 5))
#   u <- readBin(con, what = "character", n = 1)
#   hd$time_sweep <- as.numeric(gsub("\002|\005|\n|\004", "", u))
#   
#   u <- readBin(con, what = "character", n = 1)
#   hd$depth_scaling <- as.numeric(gsub("\0016|\002|\005|\n|\004", "", u))
#   
#   u <- readBin(con, what = "character", n = 1)
#   hd$encoder_div_selection <- as.numeric(gsub("\004|\005|\n|\002", "", u))
#   
#   u <- readBin(con, what = "character", n = 1)
#   hd$antsep <- as.numeric(trimStr(gsub("\002|\005|\n|\004", "", u)))
#   
#   u <- readBin(con, what = "character", n = 1)
#   hd$unused_zero <- as.numeric(gsub("\002|\005|\n|\004", "", u))
#   
#   u <- readBin(con, what = "character", n = 1)
#   hd$splPerScan <- as.numeric(gsub("\002|\005|\n|\004", "", u))
#   
#   invisible(readBin(con, what = "character", n = 1))
#   
#   u <- readBin(con, what = "character", n = 1)
#   hd$bits <- as.numeric(gsub("\002|\005|\n|\004", "", u))
#   
#   return(hd)
# }
# 
# 
# 
# readUtsiDat <- function(con, splPerScan = 512, bits = 16){
#   con_len <- .flen(con)
#   # seek(con, where = 0, "start")
#   # xraw <- readBin(con, what = "integer", n = con_len, size = 2, endian = "little")
#   # close(con)
#   nr <- splPerScan
#   # nc <- length(xraw)/(nr+1+nr)
#   nc <- con_len/(nr*bits/8 + 1)
#   xdata <- matrix(nrow = nr, ncol = nc)
#   
#   mrkr <- character(nc)
#   
#   seek(con, where = 0, "start")
#   for(i in seq_len(nc)){
#     xdata[,i] <- readBin(con, what = "integer", n = nr, size = bits/8, endian = "little")
#     mrkr[i] <- readBin(con, what = "character", n = 1, size = 1)
#   }
#   return(list(data = xdata, fid = mrkr))
# }


#--------------------------------------

# https://stackoverflow.com/questions/50561768/r-get-argument-names-from-function-call
# Using the same formalArgs suggested by @Akrun 
# (and also in the almost duplicate Get the argument names of an R function):
#   
#   getArgNames <- function(value) formalArgs(deparse(substitute(value)[[1]]))
# 
# substitute(value) quotes the input, to prevent immediate evaluation, [[1]] 
# retrieves the function from the parsed input, deparse turns it into character 
# (since formalArgs can take the function name as character).
# 
# getArgNames(testFun())
# 
# #[1] "x" "z"



# http://stackoverflow.com/questions/17256834/getting-the-arguments-of-a-parent-
# function-in-r-with-names
# Ryan Grannell
# website   twitter.com/RyanGrannell
# location   Galway, Ireland
#' @export
getArgs <- function (returnCharacter = TRUE, addArgs = NULL) {
  # print(sys.nframe())
  # 50 -> 1 error with devtools::test() and opencpu
  # 100 -> works with devtools::test() and does not work with opencpu
  if(sys.nframe() >= 2){
    arg <- as.list(match.call(definition = sys.function( -1 ),
                              call = sys.call(-1),
                              expand.dots = TRUE )
    )
    narg <- length(arg)
    if(returnCharacter){
      if(narg  >= 3){
        eval_arg <- tryCatch(sapply(arg[3:narg], eval, simplify = FALSE),
                             error = function(cond){return(NULL)})
        if(!is.null(eval_arg)){                     
          argChar <- paste0(arg[[1]],"//", 
                            paste(names(arg[3:narg]), 
                                  mapply(pasteArgs, eval_arg, arg[3:narg]), 
                                  #sapply(eval_arg, pasteArgs, arg[3:narg]), 
                                  sep = "=", collapse = "+"))
        }else{
          return(c())
        }  
      }else{
        argChar <- paste0(arg[[1]],"//")
      }
      if(!is.null(addArgs)){
        argChar <- addArg(argChar, addArgs)
      }
      return(argChar)
    }else{
      return(arg)
    }
  }else{
    message("getargs rerror, frame = ", sys.nframe())
  }
}

pasteArgs <- function(eval_arg, arg){
  arg <- deparse((arg))
  # print(deparse(eval_arg))
  # print(class(eval_arg))
  if(class(eval_arg) == "function" || class(eval_arg) == "standardGeneric"){
    return(arg)
  }else if(is.list(eval_arg)){
    return( paste0(names(eval_arg), "<-", (eval_arg), collapse = "," ) )
  }else if(is.matrix(eval_arg)){
    return(paste(arg))
    # if eval_arg == "1:10", returns "1:10" instead of "1,2,3,4,5,6,7,8,9,10"
  }else if(is.null(eval_arg)){
    return("NULL")
  }else if(all(grepl(pattern = '^([[:digit:]]+):([[:digit:]]+)$', arg))){
    return(paste0(arg))
  }else{
    return( paste0(eval_arg, collapse = ",") )
  }
}


addArg <- function(proc, arg){
  proc_add <- paste(names(arg), sapply(pasteArgs, arg, arg),
                    sep = "=", collapse = "+")
  if(substr(proc, nchar(proc), nchar(proc)) == "//"){
    proc <- paste(proc, proc_add, sep = "")
  }else{
    proc <- paste(proc, "+", proc_add, sep = "")
  }
  return(proc)
}

# return a character vector containing the name of the FUN function
getFunName <- function(FUN){
  if(class(FUN) == "function"){
    funName <- "FUN"
  }else{
    #  if(isGeneric("FUN")){
    funName0 <- selectMethod(FUN, "numeric")
    funName <-funName0@generic[1]
  }
  return(funName)
}







#' Suppressing output from cat(), warnings & messages
#' @export
verboseF <- function(g, verbose = TRUE){
  if(verbose){
    g
  }else{
    suppressWarnings(suppressMessages(quiet(g)))
  }
}


#' Suppressing output from cat() or print()
#' 
#' This function suppresses the output from cat() or print() in a function. 
#' It was proposed by Hadley Wickham 
#' https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
#' @export
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 

# -------------------------------------------
# ------------writeDT1--------------------------
# -------------------------------------------
# @name  writeDT1 
# @description This function writes *.HD and associated *.DT1
# files  (Sensors & Software)

# @date 07.11.2012 08:33
# @auteur Emanuel Huber
# @param [text]    fileNameHD       (file path of *.hd file)
# @param [text]    fileNameDT1       (file path of *.dt1 file)


# @return list((hd = headerHD, dt1hd = headerDT1, data=myData))
# -------------------------------------------
.writeDT1 <- function(x, fPath){
  #-------------------------
  # DT1 FILE: traces
  traceData <- x@data  # should ranges between -32768 and 32767
  if(max(traceData)/max(abs(traceData))*32768 <= 32767){
    traceData <- round(traceData/max(abs(traceData))*32768)
  }else{
    traceData <- round(traceData/max(abs(traceData))*32767)
  }
  #   cat(range(traceData),"\n")
  if(min(traceData)< -32768 || max(traceData) > 32768){
    stop("problem real > integer conversion")
  }
  #   A
  #   traceData <- A$data
  storage.mode(traceData) <- "integer"
  
  # DT1 FILE: header
  indexDT1Header=c("traces", "position", "samples","topo", "NA0", "bytes","dz", 
                   "stacks","NA1","NA2", "NA3", "NA4", "NA5", "NA6", "recx","recy",
                   "recz","transx","transy","transz","time0","zeroflag", "NA7", "time",
                   "x8", "com","com1","com2","com3","com4","com5","com6")
  traces_hd <- list()
  traces_hd$traces <- x@traces
  traces_hd$position <- x@pos
  traces_hd$samples <- rep(nrow(x@data),ncol(x@data))
  if(length(x@coord) > 0 && sum(is.na(x@coord)) > 0){
    traces_hd$topo <- x@coord[,3]
  }else{
    traces_hd$topo <- rep.int(0L,ncol(x@data))
  }
  traces_hd$NA0 <- rep.int(0L,ncol(x@data))
  #   traces_hd$NA0 <- A$dt1hd$NA1
  traces_hd$bytes <- rep.int(2L,ncol(x@data))
  traces_hd$dz <- rep(x@dz*1000,ncol(x@data)) # time window 
  traces_hd$stacks <- rep(as.numeric(x@hd$NUMBER_OF_STACKS),ncol(x@data))
  traces_hd$NA1 <- rep.int(0L,ncol(x@data))
  traces_hd$NA2 <- traces_hd$NA1 
  traces_hd$NA3 <- traces_hd$NA1 
  traces_hd$NA4 <- traces_hd$NA1 
  traces_hd$NA5 <- traces_hd$NA1 
  traces_hd$NA6 <- traces_hd$NA1 
  if(length(x@rec) > 0 && sum(is.na(x@rec)) > 0){
    traces_hd$recx <- x@rec[,1]
    traces_hd$recy <- x@rec[,2]
    traces_hd$recz <- x@rec[,3]
  }else{
    traces_hd$recx <- rep.int(0L,ncol(x@data))
    traces_hd$recy <- rep.int(0L,ncol(x@data))
    traces_hd$recz <- rep.int(0L,ncol(x@data))
  }
  if(length(x@trans) > 0 && sum(is.na(x@trans)) > 0){
    traces_hd$transx <- x@trans[,1]
    traces_hd$transy <- x@trans[,2]
    traces_hd$transz <- x@trans[,3]
  }else{
    traces_hd$transx <- rep.int(0L,ncol(x@data))
    traces_hd$transy <- rep.int(0L,ncol(x@data))
    traces_hd$transz <- rep.int(0L,ncol(x@data))
  }
  # traces_hd$time0 <- x@time0 
  traces_hd$time0 <- 1L + round(x@time0/x@dz,2)
  traces_hd$time0 <- traces_hd$NA1 
  traces_hd$zeroflag <- rep.int(0L,ncol(x@data)) 
  traces_hd$NA7 <- traces_hd$NA1 
  aa <-as.POSIXct(x@time[1], origin = "1970-01-01")
  bb <- format(aa, format = "%Y-%m-%d")
  myDay <- as.double(as.POSIXct(as.Date(bb), origin="1970-01-01"))
  traces_hd$time <- x@time - myDay
  traces_hd$x8 <- rep.int(0L, ncol(x@data)) 
  if(length(x@fid) == 0){
    traces_hd$com <- rep("", ncol(x@data))
  }else{
    traces_hd$x8[trimStr(x@fid) != ""] <- 1L
    traces_hd$com <- x@fid 
  }
  
  # FILE NAMES
  dirName   <- dirname(fPath)
  splitBaseName <- unlist(strsplit(basename(fPath), '[.]'), use.names = FALSE)
  baseName   <- paste0(splitBaseName[1:(length(splitBaseName)-1)])
  if(dirName == '.'){
    fPath <- baseName
  }else{
    fPath <- paste0(dirName, '/', baseName)
  }
  # if(isTRUE(overwrite)){
  # cat("file may be overwritten\n")
  # }else{
  # fPath_orgi <- fPath
  # k <- 0
  # while(file.exists(paste(fPath,".DT1",sep="")) || 
  # file.exists(paste(fPath,".HD",sep=""))){
  # fPath <- paste(fPath_orgi,"_",k,sep="")
  # k <- k+1
  # }
  # }
  
  
  # WRITE DT1 FILE
  dt1_file <- file(paste0(fPath, ".DT1") , "wb")
  for(i in 1:ncol(x@data)){
    for(j in 1:25){
      realData4 <- traces_hd[[j]][i]
      #       realData4 <- A$dt1hd[[j]][i]
      storage.mode(realData4) <- "double"
      writeBin(realData4, dt1_file, size = 4, endian = "little")
    }
    comment28 <- as.character(traces_hd$com[i])
    # nnchar <- 28-nchar(comment28)
    com_add <- paste(c(rep(" ", 28-nchar(comment28)), comment28), 
                     sep = "", collapse = "")
    writeChar(com_add, dt1_file,nchars =28,eos = NULL)
    # suppressWarnings( writeChar(comment28, dt1_file,nchars = 28,eos = NULL))
    # for(k in 1:nnchar){
    # writeChar("^@", dt1_file)
    # }
    # two-byte integers
    writeBin(traceData[,i], dt1_file, size = 2, endian = "little")
  }
  close(dt1_file)
  
  # j<-0
  
  # j<-j+1
  # traces_hd[[indexDT1Header[j]]][i]
  
  #-------------------------
  # HD FILE: traces
  hd_file <- file(paste0(fPath, ".HD") , "w+")
  writeLines("1234", con = hd_file, sep = "\r\n")
  if(!is.null(x@hd$gprdevice)){
    writeLines(as.character(x@hd$gprdevice), con = hd_file, sep = "\r\n")
  }else{
    writeLines("Data from RGPR", con = hd_file, sep = "\r\n")
  }
  writeLines(as.character(x@date), con = hd_file, sep = "\r\n")
  writeLines(paste0("NUMBER OF TRACES"," = ", as.character(ncol(x@data))), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("NUMBER OF PTS/TRC"," = ", as.character(nrow(x@data))), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("TIMEZERO AT POINT", " = ",
                    as.character(1+round(mean(x@time0)/x@dz,2))), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("TOTAL TIME WINDOW", " = ", 
                    as.character(x@dz*(nrow(x@data)))), 
             con = hd_file, sep = "\r\n")
  startpos <- 0
  if(!is.null(x@hd$startpos)){
    startpos <- x@hd$startpos
  }
  writeLines(paste0("STARTING POSITION", " = ", as.character(startpos)), 
             con = hd_file, sep = "\r\n")
  endpos <- (ncol(x@data)-1)*x@dx
  if(!is.null(x@hd$endpos)){
    endpos <- x@hd$endpos
  }
  writeLines(paste0("FINAL POSITION"," = ", as.character(endpos)), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("STEP SIZE USED"," = ",as.character(x@dx)),
             con = hd_file, sep = "\r\n")
  writeLines(paste0("POSITION UNITS", " = ", "m"), 
             con = hd_file, sep = "\r\n")
  if(x@posunit != "m"){
    warning('Position units were defined as "metres"!\n')
  }
  writeLines(paste0("NOMINAL FREQUENCY"," = ", as.character(x@freq)), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("ANTENNA SEPARATION"," = ", as.character(x@antsep)), 
             con = hd_file, sep = "\r\n")
  pulservoltage <- 0
  if(!is.null(x@hd$PULSER_VOLTAGE_V)){
    pulservoltage <- x@hd$PULSER_VOLTAGE_V
  }
  writeLines(paste0("PULSER VOLTAGE (V)", "=", as.character(pulservoltage)), 
             con = hd_file, sep = "\r\n")
  nstacks <- 1
  if(!is.null(x@hd$NUMBER_OF_STACKS)){
    nstacks <- x@hd$NUMBER_OF_STACKS
  }
  writeLines(paste0("NUMBER OF STACKS", " = ", as.character(nstacks)), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("SURVEY MODE", " = ", as.character(x@surveymode)), 
             con = hd_file, sep = "\r\n")
  
  if(length(x@hd) > 0){
    hdNames <- names(x@hd)
    hdNames <- hdNames[!(hdNames %in% c("startpos", "endpos",
                                        "NUMBER_OF_STACKS",
                                        "PULSER_VOLTAGE_V", "gprdevice"))]
    for(i in seq_along(hdNames)){
      hdName <- gsub("_", replacement = " ", as.character(hdNames[i]))
      hdName <- gsub("Serial", replacement = "Serial#", hdName)
      hdName <- gsub("CAL tm", replacement = "CAL (t/m)", hdName)
      
      writeLines(paste0(as.character(hdName), " = ", 
                        as.character(x@hd[[hdNames[i]]])), 
                 con = hd_file, sep = "\r\n")
    }
  }
  close(hd_file)
  return(fPath)
}
