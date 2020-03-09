

# for class GPRsurvey!
# FIXME -> use that as crs() function!!
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