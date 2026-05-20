

readOUT <- function(dsn){
  # fName <- getFName(dsn, ext = c(".txt"))
  if(inherits(dsn, "connection")){
    stop("no file connection are accepted for 'dsn' argument\n",
         "please provide characters") #dsn <- file(dsn, 'rb')
  }
  
  file.h5 <- hdf5r::H5File$new(dsn, mode = "r")
  
  name1 <- names(file.h5)
  name2 <- names(file.h5[[name1[1]]])
  name3 <- names(file.h5[[name1[1]]][[name2[1]]])
  A <- file.h5[[name1[1]]][[name2[1]]][[name3[1]]]
  
  x <- list(data = t(A[,]),
            freq = 0,      # MHz (antenna frequency)
            dx   = 1,      # metres (spatial sampling)
            dz   = 1,     # ns (vertical sampling)
            antsep = 1       # antenna separation 1 m
  )
  
  gprdata <- as(x, "GPR")
  return(gprdata)
}