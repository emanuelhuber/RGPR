library(RGPR)

exportFunctionArgs <- function(x, pkg, fPath = NULL){
  mtext <-  showMethods(class = x, printTo = FALSE )
  i <- grepl('Function', mtext) & grepl(paste0('package ', pkg), mtext) 
  fvec <- gsub( "Function(\\:\\s|\\s\\\")(.+)(\\s\\(|\\\")(.+$)",
                "\\2", mtext[i] )
  # fvec
  
  u <- character(length(fvec))
  for(i in seq_along(fvec)){
    u[i] <- paste0(paste0(fvec[i], ": "), paste0(names(formals(fvec[i])), collapse = ", "))
  }
  if(!is.null(fPath)){
    write.table(u, fPath, row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  return(u)
}

u <- exportFunctionArgs(x = "GPR", pkg = "RGPR", fPath = "inst/list_fxArgs_GPR.txt")
u
