readSGPR <- function(dsn){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  
  hd <- c()
  
  invisible(seek(dsn, where = 0, origin = "start"))
  
  hd$wgpr <- readBinChar(dsn, n = 4L, size = 1L)
  hd$size <- readBin(dsn, what = integer(), n = 1L, size = 4L)
  hd$size_header <- readBin(dsn, what = integer(), size = 4L)
  hd$format <- readBin(dsn, what = integer(), size = 1L)
  hd$typeGPR <- readBin(dsn, what = integer(), size = 1L)
  hd$size_comments <- readBin(dsn, what = integer(), size = 1L)
  sz <- hd$size_comments*2 + 1
  hd$comments <- readBinChar(dsn, n = sz - 1, size = 1L)
  hd$wave_speed <- readBin(dsn, what = integer(), size = 4L) # in m/micro-second
  invisible(seek(dsn, where = 14 + sz + 5, origin = "start"))
  hd$window <- readBin(dsn, what = double(), size = 8L) # in micro-second
  hd$trace_length <- readBin(dsn, what = integer(), size = 4L) # samples
  # seek(dsn, where = 14 + sz + 15, origin = "start")
  readBin(dsn, what = integer(), size = 1L) # not used
  hd$stacking_nb <- readBin(dsn, what = integer(), size = 1L)
  hd$was_flip <- readBin(dsn, what = integer(), size = 1L)
  invisible(readBin(dsn, what = integer(), size = 1L)) # not used
  # seek(dsn, where = 14 + sz + 18, origin = "start")
  # trigger mode
  # 1 = rmStep = data acquisition by button
  # 2 = rmCont = internal frequency
  # 3 = rmWheel = external trigger device (e.g., measuring wheel)
  hd$run_mod <- readBin(dsn, what = integer(), size = 1L)
  # 14+sz+19+8
  hd$wheel_step <- readBin(dsn, what = double(), size = 8L) # in mm
  # horizontal step (distance between nearest traces) in mm
  hd$stape_of_measure <- readBin(dsn, what = integer(), size = 2L) 
  hd$topo_type <- readBin(dsn, what = integer(), size = 1L) 
  hd$null_depth_shift <- readBin(dsn, what = integer(), size = 4L) # in samples

  
  invisible(seek(dsn, where = hd$size_header, origin = "start"))
  n <- (hd$size - hd$size_header)/4
  nr <- hd$trace_length
  nc <- n/nr
  DD <- matrix(nrow = nr, ncol = nc)
  
  for(i in 1:nc){
    DD[, i] <- readBin(dsn, what = integer(), n = nr, size = 4L)
  }
  .closeFileIfNot(dsn)
  return(list(data = DD, HD = hd))
}

.gprSGPR <- function(x, fName = character(0), desc = character(0),
                     fPath = character(0), nbits = NULL, Vmax = NULL){  
  if(is.null(Vmax)) Vmax <- 50
  
  xpos <- seq(0, by = x$HD$wheel_step/100, length.out = ncol(x$data))
  ypos <- seq(0, to = x$HD$window/100, length.out = x$HD$trace_length)
  
  new("GPR",   
      version     = "0.2",
      data        = bits2volt(Vmax = Vmax)*x$data,
      traces      = 1:ncol(x$data),
      fid         = rep("", ncol(x$data)),
      coord       = matrix(nrow = 0, ncol = 0),
      pos         = xpos,
      depth       = ypos,
      rec         = matrix(nrow = 0, ncol = 0),
      trans       = matrix(nrow = 0, ncol = 0),
      time0       = rep(0, ncol(x$data)),
      time        = numeric(0),
      proc        = character(0),
      vel         = list(0.1),
      name        = fName,
      description = desc,
      filepath    = fPath,
      dz          = mean(abs(diff(ypos))),  
      dx          = mean(abs(diff(xpos))),
      depthunit   = "ns",
      posunit     = "m",
      freq        = 0,
      antsep      = 0, 
      surveymode  = "reflection",
      date        = format(Sys.time(), "%Y-%m-%d"),
      crs         = character(0),
      hd          = list()                    # header
  )
}