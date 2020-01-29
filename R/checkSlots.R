#' @export
checkSlotInit <- function(){
  return( "Check slots\n" )
}

#' @export
checkSlotStop <- function(x){
  if(length(x) > 1) stop(x)
}

#' @export
checkSlotEmpty <- function(x, name, u){
  slt_x <- slot(x, name)
  test1 <- length(slt_x) == 0
  msg <- NULL
  switch(name,
         "antsep" = {
           if(test1 || is.na(slt_x)){
             msg <- paste0("Antenna separation must be ",
                           "first set with `antsep(x) < -... `!")
           }
         },
         "vel" = {
           if(test1  || length(slt_x[[1]]) == 0){
             msg <- paste0("GPR wave velocity must be ",
                           "first set with `vel(x) < -... `!")
           }
         },
         "fid" = {all(slt_x == "")},
         "ann" = {all(slt_x == "")},
         "version" = {slt_x == ""},
         "name"= {slt_x == ""},
         "description"= {slt_x == ""},
         "filepath"= {slt_x == ""},
         "depthunit"= {slt_x == ""},
         "posunit"= {slt_x == ""},
         "surveymode"= {slt_x == ""},
         "date"= {slt_x == ""},
         "crs"= {slt_x == ""},
         "proc"= {slt_x == ""},
         stop("Unknown input"))
  if(is.null(msg)){
    return(u)
  }else{
    u1 <- paste0("slot '", name, "': ", msg, "\n")
    return(c(u, u1))
  }
}