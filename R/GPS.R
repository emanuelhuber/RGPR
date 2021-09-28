
# longitude = x
# latitude  = y

# Although the US convention may be (long, lat), that does not make such a 
# convention "US-centric." This actually is the international and very long 
# established convention in mathematics and physics. It is based on the concept 
# of orientation of a coordinate system in which (long, lat) agrees with the 
# conventional positive (x,y,z) orientation of 3D Cartesian coordinates when 
# an outward-pointing normal direction (i.e., "up") is used for the third 
# coordinate on the sphere.



#' @export
readGPGGA <- function(dsn, sep = ","){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  content <- verboseF(readLines(dsn), verbose = FALSE)
  if(length(content) == 0){
    .closeFileIfNot(dsn)
    return(NULL)
  }
  a <- read.table(textConnection(content), 
                  header = FALSE, colClasses = "character",
                  stringsAsFactors = FALSE, sep = ",")
  llz <- getLonLatFromGPGGA(a)
  sp::coordinates(llz) <- cbind(x = llz$lon, y = llz$lat)
  sp::proj4string(llz) <- sp::CRS("+proj=longlat +datum=WGS84")
  .closeFileIfNot(dsn)
  return(llz)
}

#' @export
getLonLatFromGPGGA <- function(a){  
  a <- as.data.frame(a, stringsAsFactors = FALSE)
  a <- a[a[,1]=="$GPGGA",]
  
  names(a) <- c("ID", "UTC", "lat", "NS", "lon", "EW", "fix", 
                "NbSat", "HDOP", "H", "mf", "HGeoid", "mf2", 
                "TDGPS", "DGPSID", "Checks")[1:ncol(a)]
  
  return(.getLatLonFromGPGGA(a))
}


.getLatLonFromGNGGA <- function(a){
  trctime <- strptime(paste(Sys.Date(), a$UTC), '%Y-%m-%d %H%M%OS', tz='UTC')
  
  # 3 latitude 
  #  The format for NMEA coordinates is (d)ddmm.mmmm
  lat <- as.numeric(a$lat) / 100
  
  # 5 longitude
  #  The format for NMEA coordinates is (d)ddmm.mmmm
  lon <- as.numeric(a$lon) / 100
  
  # 10 elevation
  z <- as.numeric(a$H)
  
  llz <- data.frame(lon = lon, lat = lat, z = z, time = trctime)
  colnames(llz) <- c("lon", "lat", "z", "time")
  return(llz)
}

.getLatLonFromGPGGA <- function(a){
  trctime <- strptime(paste(Sys.Date(), a$UTC), '%Y-%m-%d %H%M%OS', tz='UTC')
  
  # 3 latitude 
  #  The format for NMEA coordinates is (d)ddmm.mmmm
  lat <- sapply(a$lat, stringToLatLonGPGGA, NW = a$NS, USE.NAMES = FALSE, nn = 2)
  
  # 5 longitude
  #  The format for NMEA coordinates is (d)ddmm.mmmm
  lon <- sapply(a$lon, stringToLatLonGPGGA, NW = a$EW, USE.NAMES = FALSE, nn = 3)
  
  # 10 elevation
  z <- as.numeric(a$H)
  
  llz <- data.frame(lon = lon, lat = lat, z = z, time = trctime)
  colnames(llz) <- c("lon", "lat", "z", "time")
  return(llz)
}


stringToLatLonGPGGA <- function(x, NW = "N", nn = 2){
  ddmm_mmmm <- strsplit(x, "\\.")[[1]]
  n <- nchar(ddmm_mmmm[1])
  if(n > nn){
    dd <- as.numeric(substring(ddmm_mmmm[1],1, n-2))
  }else{
    dd <- 0
  }
  mm_mmmm <- paste0(substring(ddmm_mmmm[1],n-1, n), ".", ddmm_mmmm[2])
  lat <- dd + as.numeric(mm_mmmm) / 60
  if(any(c("S", "W") %in% NW)) lat <- -lat
  return(lat)
}

#' longitude, latitude to UTM
#' 
#' see https://stackoverflow.com/a/30225804
#' https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
#' 
#' check also https://stackoverflow.com/questions/176137/java-convert-lat-lon-to-utm
#' @export
llToUTM <- function(lon, lat, zone = NULL, south = NULL, west = FALSE){
  # todo: check if lat/long in hh:mm:ss and convert them into
  #       decimal with the function 'll2dc()' (see below)
  lat_mean <- median(lat)
  lon_mean <- median(lon)
  if(isTRUE(west)){
    lon_mean <- -lon_mean
  }
  if(is.null(zone)){
    zone <- getUTMzone(lat = lat_mean, lon = lon_mean)
  }

  if(is.null(south) && lat_mean < 0){
    south <- "+south "
    lat <- -lat
    print("SOUTH")
  }else if(isTRUE(south)){
    south <- "+south "
  }else{
    south <- ""
  }
  
  ll <- data.frame(ID = 1:length(lat), X = lon, Y = lat)
  sp::coordinates(ll) <- c("X", "Y")
  sp::proj4string(ll) <- sp::CRS("+proj=longlat +datum=WGS84")
  xy_crs <- paste0("+proj=utm ", south, "+zone=", zone, " +datum=WGS84",
                   " +units=m +no_defs", " +ellps=WGS84 +towgs84=0,0,0")
  xy <- sp::spTransform(ll, sp::CRS(xy_crs))
  return(list(xy = as.matrix(as.data.frame(xy)[,2:3]), crs = xy_crs))
}

#' Get UTM zone from lattidue and longitude
#'
#' @export
getUTMzone <- function(lat, lon){
  # see https://stackoverflow.com/a/9188972
  # The formula is to simple: it does not work for the both 
  # UTM Zone Exceptions in Norway and Svalbard
  # Special zones for Svalbard and Norway
  lat <- median(lat)
  lon <- median(lon)
  if (lat >= 72.0 && lat < 84.0 ) 
    if (lon >= 0.0  && lon <  9.0) 
      return(31)
  if (lon >= 9.0  && lon < 21.0)
    return(33)
  if (lon >= 21.0 && lon < 33.0)
    return(35)
  if (lon >= 33.0 && lon < 42.0) 
    return(37)
  zone <- (floor((lon + 180)/6) %% 60) + 1
  return(unique(zone)[1])
}

#' UTM to latitude-longitude
#' @return a 2-column-matrix (longitude (N), latitude (E))
#' 
#' @export
UTMToll <- function(xy, xy_crs = NULL){
  #if(max(xy[,1]) > 834000) stop("y-values (northing) are larger than 834000")
  #if(min(xy[,1]) < 166000) stop("x-values (easting) are smaller than 166000")
  if(is.null(xy_crs)){
    xy_crs <- "+proj=utm +zone=32 +ellps=WGS84"
  } 
  ll <- data.frame(ID = 1:nrow(xy), X = xy[,1], Y = xy[,2])
  sp::coordinates(ll) <- c("X", "Y")
  sp::proj4string(ll) <- sp::CRS(xy_crs)
  xy <- sp::spTransform(ll, sp::CRS("+init=epsg:4326"))
  as.matrix(as.data.frame(xy)[,2:3])
}

# conversion latitude longitude (hh:mm:ss into decimal
ll2dc <- function(x){
  NS <- gsub('[^[:alpha:]]', "", x)
  w <- gsub('[^0-9:.]', "", x)
  V <- matrix(as.numeric(do.call(rbind, strsplit(w, ":"))), ncol = 3)
  pm <- 2* (grepl("N", NS) | grepl("E", NS)) - 1
  dec <- (V[,1] + V[,2] / 60 + V[,3]/3600) * pm
  return(dec)
}  

