#' Remove delineations from the GPR data
#'
#' @param obj (`GPR* object`) An object of the class `GPR`
#' @param value (`integer[n]|all`) Either a vector of indexes of delineations to
#' to remove or `all` to remove all delineations, 
#' @name rmDelineations<-
#' @rdname delineation
#' @export
setGeneric("rmDelineations<-", function(x,value=NULL) 
  standardGeneric("rmDelineations<-"))


#' @rdname delineation
#' @export
setReplaceMethod("rmDelineations", "GPR", function(x,value=NULL){
  deli <- x@delineations
  n_d <- length(deli)
  if(!is.null(value) && n_d > 0 && inherits(value, "numeric")){
    n_tot <- sum(sapply(deli, .lengthList))
    it <- 0
    value <- n_tot - value + 1
    for(i in n_d:1){
      if(typeof(deli[[i]]) == "list"){
        n_sub_d <- length(deli[[i]])
        for(j in n_sub_d:1){
          it <- it + 1
          if(it %in% value){
            x@delineations[[i]][j] <- NULL
            if(length(x@delineations[[i]])==0 || 
               is.null(unlist(x@delineations[[i]], use.names = FALSE))){
              x@delineations[i] <- NULL
              break
            }
          }
        }
      }else{
        it <- it + 1
        if(it %in% value){
          x@delineations[i] <- NULL
        }
      }
    }
  }else if(n_d <1){
    warning("No delineation to delete\n")
  }else if(is.null(value)){
    stop("You must specified the no of the delineation you want to delete!\n")
  }else if(value == "all"){
    x@delineations <- list()
  }
  return(x)
})