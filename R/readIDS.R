

#' @export
readDT <- function(dsn){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  
  hd <- c()
  seek(dsn, where = 0, origin = "start")
  hd$code <- int2ascii(dsn, n = 1L)
  # hd$code <- readBin(dsn, what = character(), n = 1L, size = 1)
  if(substr(hd$code, 1, 1) != "V") stop("Error reading file. Please contact me!")
  # hd$file_version <- substr(hd$code, 2, 2)
  hd$file_version <- readBin(dsn, what = integer(), n = 3L, size = 1)
  hd$len_rec <- readBin(dsn, what = integer(), n = 1L, size = 2) 
  
  pos <- hd$len_rec 
  # 2052 or 1024 (until now)
  
  invisible(seek(dsn, where = pos, origin = "start"))
  # seek(dsn, where = NA)
  while(substr((u <- readBin(dsn, what = character(), n = 1L, size = 1)),1 ,1) != "R"){
    
    # pos <- pos + hd$len_rec
    # invisible(seek(dsn, where = pos, origin = "start"))
    # i <- seek(dsn, where = NA)
    # u <- readBin(dsn, what = character(), n = 1L, size = 1)
    # # u <- ascii2num(dsn, 4)
    # j <- seek(dsn, where = NA)
    # # 
    # hd$HD <- c(hd$HD, u[1])
    # print(paste0(u[1] , "  ", i, " - ", j, "  diff = ", j-i))
    
    invisible(seek(dsn, where = pos + 4, origin = "start"))
    
    if(substr(u, 1, 2) == "FI"){
      hd$sweep_marker_1 <- ascii2num(dsn, 6)
    }else if(substr(u, 1, 1) == "I"){
      hd$survey_info <- readBinChar(dsn, n = hd$len_rec - 4, size = 1)
    }else if(substr(u, 1, 1) == "C"){
      hd$survey_info <- readBinChar(dsn, n = hd$len_rec - 4, size = 1)
    }else if(substr(u, 1, 2) == "AH"){
      hd$height <- ascii2num(dsn, 16)
    }else if(substr(u, 1, 2) == "FZ"){
      hd$zone <- readBinChar(dsn, n = hd$len_rec - 4, size = 1)
    }else if(substr(u, 1, 2) == "FX"){       # OK
      hd$offset_x <- ascii2num(dsn, 16)
    }else if(substr(u, 1, 2) == "FQ"){
      hd$marker_quantum <- ascii2num(dsn, 16)
    }else if(substr(u, 1, 2) == "FM"){
      hd$sweep_marker <- ascii2num(dsn, 6)
      hd$position <- ascii2num(dsn, 16)
    }else if(substr(u, 1, 2) == "AC"){
      hd$tx_n   <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$tx_seq <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$rx_n   <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$rx_seq <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$nacq   <- readBin(dsn, what = integer(), n = 1, size = 4)
    }else if(substr(u, 1, 2) == "AM"){
      hd$direct <-  readBinChar(dsn, n =1, size = 1)
      hd$coord_l <- ascii2num(dsn, 16)
      hd$coord_t <- ascii2num(dsn, 16)
    }else if(substr(u, 1, 3) == "ATR"){
      hd$tx_x0 <- ascii2num(dsn, 16)
      hd$tx_y0 <- ascii2num(dsn, 16)
      hd$tx_alpha <- ascii2num(dsn, 16)
      hd$tx_freq <- ascii2num(dsn, 5)
      hd$rx_x0 <- ascii2num(dsn, 16)
      hd$rx_y0 <- ascii2num(dsn, 16)
      hd$rx_alpha <- ascii2num(dsn, 16)
      hd$rx_freq <- ascii2num(dsn, 5)
    }else if(substr(u, 1, 2) == "AA"){
      hd$info <- trimStr(readBinChar(dsn, n = hd$len_rec - 4, size = 1))
    }else if(substr(u, 1, 1) == "S"){
      hd$S <- readBinChar(dsn, n = hd$len_rec - 4, size = 1)
    }else if(substr(u, 1, 2) == "FW"){
      hd$da_buttare <- readBin(dsn, what = integer(), n = 1, size = 4)
      #= fread(fid,1,'integer*4');
      hd$channel_n <- readBin(dsn, what = integer(), n = 1, size = 4)
      # dati.n_canali = fread(fid,1,'integer*4');
      hd$stacking <- readBin(dsn, what = integer(), n = 1, size = 4)
      # dati.stacking = fread(fid,1,'integer*4');
      hd$interleaving <- readBin(dsn, what = integer(), n = 1, size = 4)
      # dati.interleaving = fread(fid,1,'integer*4');
      hd$channel_id <- readBin(dsn, what = integer(), n = 1, size = 4)
      # dati.id_canale = fread(fid,1,'integer*4');
      hd$SOS_high <- readBin(dsn, what = integer(), n = 1, size = 4)
      # dati.SOS_high = fread(fid,1,'integer*4');
      hd$sampling_max <- ascii2num(dsn, 16)
      # dati.max_sampling_AD =ascii2num(fid,16);
      hd$sw_version <- readBinChar(dsn, n = 10, size = 1)
      # temp = fread(fid,10,'char*1');
      # dati.SW_version = char(temp');
      hd$build_version <- readBinChar(dsn, n = 10, size = 1)
      # temp = fread(fid,10,'char*1');
      # dati.build_version = char(temp');
      hd$fw_version <- readBinChar(dsn, n =7, size = 1)
      # temp = fread(fid,7,'char*1');
      # dati.FW_version = char(temp');
      hd$GPS_offset_x <- ascii2num(dsn, 16)
      hd$GPS_offset_y <- ascii2num(dsn, 16)
      # dati.GPS_offset_x = ascii2num(fid,16);
      # dati.GPS_offset_y = ascii2num(fid,16);
    }else if(substr(u, 1, 1) == "H"){
      # contiene la lunghezza del record vienecestinata
      hd$scratch <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$n_acq_sweep <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$n_acq_sample <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$n_sampler_x <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$n_sampler_y <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$enable_x_compress <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$n_x_compress <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$n_y_compress <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$enable_wheel <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$wheel_compress <- readBin(dsn, what = integer(), n = 1, size = 4)
      hd$ad_offset <- readBin(dsn, what = integer(), n = 1, size = 4)
      # datiscritti in ASCII
      hd$radar_freq <- ascii2num(dsn, 16)
      hd$prop_vel <- ascii2num(dsn, 16)
      hd$sweep_time <- ascii2num(dsn, 16)
      hd$sweep_time_tot <- ascii2num(dsn, 16)
      hd$scan_freq <- ascii2num(dsn, 16)
      hd$scan_time_acq <- ascii2num(dsn, 16)
      hd$sweep_dx <- ascii2num(dsn, 16)
      hd$wheel_dx <- ascii2num(dsn, 16)
      hd$x_cell <- ascii2num(dsn, 16)
      hd$y_cell <- ascii2num(dsn, 16)
    }else if(substr(u, 1, 2) == "FC"){       # OK
      hd$conv_int_volts <- ascii2num(dsn, 16)
    }else if(substr(u, 1, 2) == "FS"){
      hd$symbol <- readBinChar(dsn, n = hd$len_rec - 4, size = 1)
      # dati.simboli=strvcat(dati.simboli,deblank(char(fread(fid,len_rec-4,'integer*1');')));
    }else if(substr(u, 1, 2) == "FT"){       # OK
      hd$t_soil_sample <- ascii2num(dsn, 16)
    }else if(substr(u, 1, 2) == "FO"){
      # temp=fread(fid,len_rec-4,'integer*1');
      # dati.info_operazione=strvcat(dati.info_operazione,deblank(char(temp')));
      hd$info_operation <- readBinChar(dsn, n = hd$len_rec - 4, size = 1)
    }else if(substr(u, 1, 2) == "FN"){
      hd$id_sample_noise <- readBin(dsn, what = integer(), n = 1L, size = 4)
      
      # OBSOLETE HEADER PART
    }else if(substr(u, 1, 3) == "ATX"){
      # see "AC": hd$tx_seq
      # fseek(fid,53*(dati.tx_sequence-1),0);
      invisible(seek(dsn, where = 53 * (hd$tx_seq - 1), origin = "current"))
      # %rev 1.4 :   simuovesullaposizionedelleinformazioni del Txcorretto
      hd$tx_x0 <- ascii2num(dsn, 16)
      hd$tx_y0 <- ascii2num(dsn, 16)
      hd$tx_alpha <- ascii2num(dsn, 16)
      hd$tx_freq <- ascii2num(dsn, 5)
    }else if(substr(u, 1, 3) == "ARX"){
      invisible(seek(dsn, where = 53 * (hd$rx_seq - 1), origin = "current"))
      # # fseek(fid,53*(hd$rx_sequence-1),0);
      # %rev 1.4: simuovesullaposizionedelleinformazioni del Rx corretto
      hd$rx_x0 <- ascii2num(dsn, 16)
      hd$rx_y0 <- ascii2num(dsn, 16)
      hd$rx_alpha <- ascii2num(dsn, 16)
      hd$rx_freq <- ascii2num(dsn, 5)
    }else if(substr(u, 1, 2) == "GI"){
      #message("'GI' not yet implemented!")
      hd$GI <- ascii2num(dsn, 16)  # 0 in 1 file.
    }else{
      u <- gsub("[[:blank:]]", "", u)
      message("'", u, "' not yet implemented!")
    }
    # else if(substr(u, 1, 3) == "AC1"){
    #   message("'AC1' not yet implemented!")
    # }else if(substr(u, 1, 2) == "AM0"){
    #   message("'AM0' not yet implemented!")
    # }
    pos <- pos + hd$len_rec
    invisible(seek(dsn, where = pos, origin = "start"))
  }
  
  invisible(seek(dsn, where = pos, origin = "start"))
  
  if(!(hd$n_sampler_y > 0 && hd$n_sampler_x > 0)){
    warning("Something is weird with you file (", hd$n_sampler_y, ", ",
            hd$n_sampler_x, ") ... ",
            "Please contact me: emanuel.huber@pm.me")
  }
  
  DD <- matrix(nrow = hd$n_sampler_y, ncol = hd$n_sampler_x)
  DDhd1 <- numeric(hd$n_sampler_x)
  DDhd2 <- numeric(hd$n_sampler_x)
  for(i in seq_len(hd$n_sampler_x)){
    DDhd1[i] <- readBin(dsn, what = integer(),  n = 1, size = 2)
    DDhd2[i] <- readBin(dsn, what = integer(),  n = 1, size = 2)
    DD[, i] <- readBin(dsn, what = integer(),  n =  hd$n_sampler_y, size = 2)
  }
  .closeFileIfNot(dsn)
  return(list(data = DD, HD = hd, mrk1 = DDhd1, mrk2 = DDhd2))
}

.gprDT <- function(x, fName = character(0), desc = character(0),
                   fPath = character(0), nbits = NULL, Vmax = NULL){  
  # plot3D::image2D(DD)
  
  if(is.null(x$HD$conv_int_volts) || x$HD$conv_int_volts == 0){
    x$HD$conv_int_volts <- 10/32768
  }
  if(is.null(x$HD$ad_offset)){
    x$HD$ad_offset <- 0
  }
  
  if(is.null(nbits) && is.null(Vmax)){
    x$data <- (x$data - x$HD$ad_offset)*x$HD$conv_int_volts
  }else{
    x$data <- bits2volt(Vmax = Vmax, nbits = nbits)*x$data
  }
  
  xpos <- seq(0, by = 1, length.out = x$HD$n_sampler_x) * x$HD$x_cell + x$HD$offset_x
  #zpos <- seq(0, by = 1, length.out = x$HD$n_sampler_y) * x$HD$y_cell
  # zpos <- seq(0, by = (x$HD$scan_time_acq / 10^-9)/(x$HD$n_sampler_y - 1), length.out = x$HD$n_sampler_y) #* x$HD$y_cell
  zpos <- seq(0, by = (x$HD$sweep_time_tot / 10^-9)/(x$HD$n_sampler_y - 1), length.out = x$HD$n_sampler_y) #* x$HD$y_cell
  
  dx <- mean(abs(diff(xpos)))
  dz <- mean(abs(diff(zpos)))
  
  fid1_test <- ifelse(x$mrk1 == 2130, "MRK1", "") 
  fid2_test <- ifelse(x$mrk1 == 2386, "MRK2", "")
  fidGPS_test <- ifelse(x$mrk1 == 1874, "GPS", "")
  nonvalid_sweep <- ifelse(x$mrk1 == 338 & x$mrk2 == 6, "NVSWP", "")
  
  FID <- trimStr(paste(fid1_test, fid2_test, fidGPS_test, nonvalid_sweep))
  
  if(!is.null(x$HD$scan_freq)){
    antfreq <- as.integer(x$HD$scan_freq)
  }else if(!is.null(x$HD$tx_freq)){
    antfreq <- as.integer(x$HD$tx_freq)
  }else{
    antfreq <- 0
    message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
  }
  
  message("Antenna separation set to 0 ", "m", 
          ". Set it with 'antsep(x) <- ... '")
  
  # plot3D::image2D(z = t(DD2), x = xpos, y = ypos)
  new("GPR",   
      version     = "0.2",
      data        = x$data,
      traces      = 1:ncol(x$data),
      fid         = FID,
      #coord = coord,
      coord       = matrix(nrow = 0, ncol = 0),
      pos         = xpos,
      depth       = zpos,
      rec         = matrix(nrow = 0, ncol = 0),
      trans       = matrix(nrow = 0, ncol = 0),
      time0       = rep(0, ncol(x$data)),
      time        = numeric(0),
      proc        = character(0),
      vel         = list(v = 0.1),
      name        = fName,
      description = desc,
      filepath    = fPath,
      dz          = dz, 
      dx          = dx,
      depthunit   = "ns",
      posunit     = "m",
      freq        = antfreq[1], 
      antsep      = 0, 
      surveymode  = "reflection",
      date        = format(Sys.time(), "%Y-%m-%d"),
      crs         = character(0),
      hd         =  x$HD
  )
}

#' @export
readGEC <- function(dsn){
  # if(!inherits(dsn, "connection")){
    # dsn <- file(dsn, 'rt', raw = TRUE)
  # }
  dsn <- .openFileIfNot(dsn)  # in case there is some binary stuff
  tags <- scan(dsn,  what = character(), n = 100, skipNul = TRUE)
  # UTMzone <- strsplit(tags[which(tags == "<UTM_ZONE>")[1] + 1], ",")[[1]]
  # number of makers
  # nmrk <- as.integer(tags[which(tags == "<VALID_MARKERS_SWEEP>")[1] + 1])
  nstart <- max(grep("^<(.*?)>$", tags)) + 1
  invisible(seek(dsn, where = 0, origin = "start"))
  xyz <- read.table(dsn, header = FALSE,  skip = nstart, sep = ",", col.names = c("ID", "xpos", "ypos", "x", "y", "z", "crs", "crs_add"))
  .closeFileIfNot(dsn)
  return(xyz[, c("x", "y", "z", "ID", "crs", "crs_add")])
}
