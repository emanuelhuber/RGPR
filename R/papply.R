
#' Apply batch processing to a GPRsurvey object
#'
#' Applies a list of processing functions to each GPR line of a materialized
#' `GPRsurvey`. The processed lines are written back to the HDF5 backing
#' file. Survey-level metadata and intersections are updated once at the
#' end, which is more efficient than replacing each line through `[[<-`.
#'
#' All lines are rewritten under a **single** HDF5 update transaction (see
#' `.h5_update_survey()` in `hdf5_update.R`): one lock, one temporary copy
#' of the backing file, one open handle for the whole loop, one checksum
#' verification pass, one atomic swap into place at the end. If processing
#' fails partway through (e.g. one of the functions in `prc` errors on some
#' line), the backing file is left completely untouched -- you get the
#' error back with the *original* file still intact, rather than a file
#' that's been partially reprocessed.
#'
#' @param obj Object of class `GPRsurvey`.
#' @param prc A named list of processing functions and their arguments.
#'
#' @return A processed `GPRsurvey` object.
#'
#' @name papply
#' @rdname papply
#' @export
#' @concept processing
setGeneric("papply", function(obj, prc = NULL) standardGeneric("papply"))


#' @rdname papply
#' @export
setMethod("papply", "GPRsurvey", function(obj, prc = NULL) {
  
  if (isTRUE(obj@view)) {

    stop(
      "Cannot modify a read-only GPRsurvey view. ",
      "Use materialize() first.",
      "This GPRsurvey object is a view of an existing HDF5 file. ",
      "The processing was not written to the HDF5 file, and the object ",
      "is returned unchanged. Use materialize(x, dsn = ...) first if ",
      "you want an independent, writable survey."
    )
    # return(obj)
  }
  
  if (!is.list(prc)) {
    stop("'prc' must be a list.", call. = FALSE)
  }
  
  if (length(prc) == 0L) {
    return(obj)
  }
  
  if (is.null(names(prc)) || any(!nzchar(names(prc)))) {
    stop("'prc' must be a named list of processing functions.", call. = FALSE)
  }
  
  # -----------------------------------------------------------------------
  # Process every line, then rewrite all of them (plus survey metadata and
  # intersections) in a single HDF5 transaction. Reading each source line
  # (`obj[[i]]`) happens outside the transaction, since it only needs
  # read-only access to the file and doesn't need to hold the write lock.
  # -----------------------------------------------------------------------
  obj <- .h5_update_survey(obj@path, function(h5) {
    local_obj <- obj
    
    for (i in seq_along(obj)) {
      y <- verboseF(obj[[i]], verbose = FALSE)
      
      message("Processing ", y@name, "...", appendLF = FALSE)
      
      for (k in seq_along(prc)) {
        fun <- names(prc)[k]
        y <- do.call(fun, c(list(obj = y), prc[[k]]))
      }
      
      local_obj <- .replace_one_GPRsurvey_line_hdf5(
        h5    = h5,
        x     = local_obj,
        i     = i,
        value = y
      )
      
      message(" done!", appendLF = TRUE)
    }
    
    .finalize_replace_write_hdf5(h5, local_obj)
  })
  
  validObject(obj)
  
  obj
})


#' @name papply
#' @rdname papply
#' @export
setMethod("papply", "GPR", function(obj, prc = NULL){
  if(typeof(prc) != "list") stop("'prc' must be a list")
  for(k in seq_along(prc)){
    obj <- do.call(names(prc[k]), c(obj = obj,  prc[[k]]))
    message("*", appendLF = FALSE)
  }
  message("")
  return(obj)
} 
)
