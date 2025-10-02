#' Reverse the trace position.
#'
#' @param x (`GPR`|`GPRsurvey`) Object of the class `GPR` or `GPRsurvey`
#' @param id (`NULL`|`integer[n]`|`zigzag`) Wokrs only if `x` is an object of the class `GPRsurvey`.
#'            Either the index of the GPR data to reverse (e.g., `id = c(1, 3, 4, 7)`) or
#'            `id = "zigzag` to reverse every second GPR data.
#'            If `id = NULL` and `x` has coordinates, \code{reverse()} 
#'           will cluster the GPR data according to their names (e.g., 
#'           cluster 1 = XLINE01, XLINE02, XLINE03; cluster 2 = YLINE01, 
#'           YLINE02; cluster 3 = XYLINE1, XYLINE2, XYLINE3) and reverse the
#'           data such that all GPR lines within the same cluster have the
#'           same orientation (up to a tolerance value \code{tol}).
#' @param to Length-one numeric vector. Tolerance angle in radian to determine
#'           if the data have the same orientation. The first data of the 
#'           cluster is set as reference angle \eqn{\alpha_0}, then for data 
#'           \eqn{i} in the same cluster, if \eqn{\alpha_i} is not between
#'           \eqn{\alpha_0 - \frac{tol}{2}} and 
#'           \eqn{\alpha_0 + \frac{tol}{2}}, then the data is reversed.
#' @name reverse
#' @rdname reverse
#' @examples 
#' \dontrun{
#' # SU class GPRsurvey
#' SU <- reverse(SU, id = "zigzag")
#' # identical to above
#' SU <- reverse(SU, id = seq(from = 2, to = length(SU), by = 2))
#' }
#' @export
#' @concept processing
setGeneric("reverse", 
           function(x, id = NULL,  tol = 0.3, track = TRUE)
             standardGeneric("reverse"))


#' @rdname reverse
#' @export
setMethod("reverse", "GPR", function(x, id = NULL,  tol = 0.3, track = TRUE){
  
  #------------------- check arguments
  # NOT REALLY USEFUL, since id and tol are not used!
  # msg <- checkArgInit()
  # msg <- checkArg(id, msg, 
  #                 "INDEX_VECTOR_NULL_UPPER" , 
  #                 n)
  # msg <- checkArg(w,    msg, "NUMERIC1_SPOS_NULL", Inf)
  # checkArgStop(msg)
  #-----------------------------------
  
  xnew <- x
  xnew@data <- x@data[,length(x):1]
  xnew@z0 <- rev(x@z0)
  xnew@time <- rev(x@time)
  xnew@markers <- rev(x@markers)
  xnew@ann <- rev(x@ann)
  if(length(x@coord)>0){
    xnew@coord <- x@coord[nrow(x@coord):1,]
  }
  if(length(x@rec)>0){
    xnew@rec <- x@rec[nrow(x@rec):1,]
  }
  if(length(x@trans)>0){
    xnew@trans <- x@trans[nrow(x@trans):1,]
  }
  if(length(x@delineations) > 0){
    #FIXME NO IDEA IF IT WORKS!
    xnew@delineations <- rapply(x@delineations, .revMat, 
                                how = "replace", n = ncol(x))
  }
  if(isTRUE(track)) proc(x) <- getArgs()
  return(xnew)
}
)

.revMat <- function(x, n){
  x[, 1] <- n - x[, 1] + 1
  x <- apply(x, 2, rev)
  return(x)
}


#' Reverse the trace position.
#'
#' @name reverse
#' @rdname reverse
#' @export
setMethod("reverse", "GPRsurvey", function(x, id = NULL, tol = 0.3, track = TRUE){
  id <- as.integer(id)
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(id, msg, "INDEX_VECTOR_NULL_UPPER" , length(x))
  msg <- checkArg(tol,    msg, "NUMERIC1_SPOS", Inf)
  checkArgStop(msg)
  #-----------------------------------
  
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
          x@paths[[i]]     <- .saveTempFile(y)
          x@coords[[y@name]]   <- y@coord
          x@markers[[y@name]]      <- y@markers
        }
      }
    }
    x@intersections <- list()
    x <- setCoordref(x)
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
        x@paths[[id[i]]]     <- .saveTempFile(y)
        if(length(y@coord) > 0){
          # x@coords[[y@name]]   <- y@coord
          x@coords[[id[i]]]   <- y@coord
        }
        # x@fids[[y@name]]      <- y@fid
        x@markers[[id[i]]]      <- y@markers
      }
      x@intersections <- list()
      x <- setCoordref(x)
      return(x) 
    }else{
      stop("id must be between 1 and ", length(x),"!")
    }
  }
  # if is.character(id) (<- name of data)
})
