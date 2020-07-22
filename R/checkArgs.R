#' @export
checkArgInit <- function(){
  return( "Check arg\n" )
}

#' @export
checkArgStop <- function(x){
  if(length(x) > 1) stop(x)
}

#' @export
checkArg <- function(x, u, type, y, ...){
  arg <- sapply(match.call(expand.dots=TRUE)[-1], deparse)
  msg <- NULL
  switch(type,
         "COUNT" = {
           if(!checkmate::testCount(x)){
             msg <- "must be a positiv integer"
           }
         },
         "STRING" = {
           if(!checkmate::testString(x)){
             msg <- "must be a string"
           }           
         },
         "STRING_CHOICE" = {
           if(!(checkmate::testString(x) && x %in% y)){
             msg <- paste0("string must be one of '", 
                           paste0(y, collapse = "', '"), "'")
           }
         },
         "INDEX_VECTOR_NULL_UPPER" = {
           if(!checkmate::testInteger(x, lower = 1, min.len = 1, 
                                      upper = y, null.ok = TRUE,
                                      any.missing = FALSE, 
                                      all.missing = FALSE  )){
             msg <- paste0("must be a vector of strictly positiv integer",
                           " with max value <= ", y)
           } 
         },
         "FUNCTION_NULL" = {
           if(!(checkmate::testFunction(x, null.ok = TRUE))){
             msg <- "must be a function"
           }
         },
         "FUNCTION" = {
           if(!(checkmate::testFunction(x))){
             msg <- "must be a function"
           }
         },
         "INTEGER" = {
           
         },
         "PERCENT1" = {
           if(!checkmate::testNumeric(x, lower = 0, upper = 1, finite = TRUE,
                                      any.missing = FALSE, 
                                      all.missing = FALSE,
                                      len = 1)){
             msg <- "must be a numeric value between 0 and 1"
           } 
         },
         "PERCENT1_NULL" = {
           if(!checkmate::testNumeric(x, lower = 0, upper = 1, finite = TRUE,
                                      any.missing = FALSE, 
                                      all.missing = FALSE,
                                      len = 1,
                                      null.ok = TRUE)){
             msg <- "must be a numeric value between 0 and 1"
           } 
         },
         "NUMERIC" = {
           if(!checkmate::testNumeric(x,
                                      finite = TRUE,
                                      any.missing = FALSE, 
                                      all.missing = FALSE)){
             msg <- paste0("must be a numeric value/vector")
           }
         },
         "NUMERIC_LEN" = {
           if(! (checkmate::testNumeric(x,
                                        finite = TRUE,
                                        any.missing = FALSE, 
                                        all.missing = FALSE) &&
                 length(x) %in% y ) ){
             if(length(y) < 2){
               msg <- paste0("must be a numeric value")
             }else{
               msg <- paste0("must be a numeric vector of length ",
                             paste0(y[-length(y)], collapse = ", "),
                             " or ", y[length(y)])
             }
           }
         },
         # length = 1,  POSitive
         "NUMERIC1_POS" = {
           if(!checkmate::testNumeric(x, lower = -sqrt(.Machine$double.eps),
                                      finite = TRUE,
                                      any.missing = FALSE, 
                                      all.missing = FALSE,
                                      len = 1,
                                      upper = y)){
             msg <- "must be a numeric value >= 0" 
             if(is.finite(y)) msg <- paste0(msg, " and < ", y)
           }
         },
         "NUMERIC1_SPOS" = {
           if(!checkmate::testNumeric(x, lower = sqrt(.Machine$double.eps),
                                      finite = TRUE,
                                      any.missing = FALSE, 
                                      all.missing = FALSE,
                                      len = 1,
                                      upper = y)){
             msg <- "must be a numeric value > 0"
             if(is.finite(y)) msg <- paste0(msg, " and < ", y)
           }
         },
         # length = 1, Strictly POSitive
         "NUMERIC1_NULL" = {
           if(!checkmate::testNumeric(x, 
                                      finite = TRUE,
                                      any.missing = FALSE, 
                                      all.missing = FALSE,
                                      len = 1,
                                      upper = y,
                                      null.ok = TRUE)){
             msg <- "must be a numeric value"
             if(is.finite(y)) msg <- paste0(msg, " < ", y)
             msg <- paste0(msg, " or a 'NULL'")
           }
         },
         # length = 1, Strictly POSitive
         "NUMERIC1_SPOS_NULL" = {
           if(!checkmate::testNumeric(x, lower = sqrt(.Machine$double.eps),
                                      finite = TRUE,
                                      any.missing = FALSE, 
                                      all.missing = FALSE,
                                      len = 1,
                                      null.ok = TRUE,
                                      upper = y)){
             msg <- "must be a numeric value > 0"
             if(is.finite(y)) msg <- paste0(msg, " and < ", y)
           }
         },
         "LOGICAL_LEN" = {
           if(!(checkmate::testLogical(x,
                                       any.missing = FALSE, 
                                       all.missing = FALSE,
                                       null.ok = FALSE) &&
                length(x) %in% y ) ){
             if(length(y) < 2){
               msg <- paste0("must be a logical value (TRUE/FALSE)")
             }else{
               msg <- paste0("must be a logical vector (TRUE/FALSE)  of length ",
                             paste0(y[-length(y)], collapse = ", "),
                             " or ", y[length(y)])
             }
           }
         },
         warning("Arg not checked. ",
                 "Please contact me with a copy of this message: \n",
                 "emanuel.huber@alumni.ethz.ch")
         # return(u)
  )
  if(is.null(msg)){
    return(u)
  }else{
    u1 <- paste0("arg '", arg[1], "': ", msg, "\n")
    return(c(u, u1))
  }
}
