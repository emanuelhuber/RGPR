# LINES <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
#                    paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))
# 
# x <- readGPR(LINES[1]) 
# x2 <- readGPR(LINES[2]) 
# x3 <- readGPR(LINES[3]) 
# 
# 
# min(x)
# min(x, x2, x3 - 10)
# 
# max(x)
# max(x, x2, x3 + 10)
# 
# range(x)
# range(x, x2, x3 + 10)
# 
# sum(x)
# sum(x, x2)
# 
# sum(x@data) + sum(x2@data)
# 
# 
# prod(x, 10)
# prod(x)
# prod(x@data)
# 
# 
# 
# plot(abs(x))
# plot(sin(x))
# 
# .GPR.min <- function(x,...,na.rm=FALSE){
#   dots <- list(...)
#   if(length(dots) == 0){
#     min(x@data, na.rm = na.rm)
#   }else{
#     z <- sapply(dots, function(x){ .GPR.min (x, na.rm = na.rm)})
#     return(min(min(x@data), z))
#     # Reduce("+", c(z, sum(x@data, na.rm = na.rm)))
#   }
# }
# 
# 
# .GPR.min(x)
# .GPR.min(x, x2-100)
