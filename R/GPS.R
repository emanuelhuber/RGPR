
# longitude = x
# latitude  = y

# Although the US convention may be (long, lat), that does not make such a 
# convention "US-centric." This actually is the international and very long 
# established convention in mathematics and physics. It is based on the concept 
# of orientation of a coordinate system in which (long, lat) agrees with the 
# conventional positive (x,y,z) orientation of 3D Cartesian coordinates when 
# an outward-pointing normal direction (i.e., "up") is used for the third 
# coordinate on the sphere.


#' Get longitude and latitude from GPGGA sentence information (NMEA)
#' 
#' @param a [\code{data.frame}] GPGGA sentence information 
#' @return [\code{data.frame}] Columns = latitude, longitude, elevation and
#'         time
#' @export
getLonLatFromGPGGA <- function(a){  
  a <- as.data.frame(a, stringsAsFactors = FALSE)
  a <- a[a[,1]=="$GPGGA",]
  
  names(a) <- c("ID", "UTC", "lat", "NS", "lon", "EW", "fix", 
                "NbSat", "HDOP", "H", "mf", "HGeoid", "mf2", 
                "TDGPS", "DGPSID", "Checks")[1:ncol(a)]
  
  return(.getLonLatFromGPGGA(a))
}


.getLonLatFromGPGGA <- function(a){
  trctime <- strptime(paste(Sys.Date(), a$UTC), '%Y-%m-%d %H%M%OS', tz='UTC')
  
  # 3 latitude 
  #  The format for NMEA coordinates is (d)ddmm.mmmm
  lat <- sapply(a$lat, stringToLat, NW = a$NS, USE.NAMES = FALSE)
  
  # 5 longitude
  #  The format for NMEA coordinates is (d)ddmm.mmmm
  lon <- sapply(a$lon, stringToLat, NW = a$EW,USE.NAMES = FALSE)
  
  # 10 elevation
  z <- as.numeric(a$H)
  
  llz <- data.frame(lon = lon, lat = lat, z = z, time = trctime)
  colnames(llz) <- c("lon", "lat", "z", "time")
  return(llz)
}


stringToLat <- function(x, NW = "N"){
  ddmm_mmmm <- strsplit(x, "\\.")[[1]]
  n <- nchar(ddmm_mmmm[1])
  if(n > 2){
    dd <- as.numeric(substring(ddmm_mmmm[1],1, n-2))
  }else{
    dd <- 0
  }
  mm_mmmm <- paste0(substring(ddmm_mmmm[1],n-1, n), ".", ddmm_mmmm[2])
  lat <- dd + as.numeric(mm_mmmm) / 60
  if(any(c("S", "W") %in% NW)) lat <- -lat
  return(lat)
}

#' Convert longitude, latitude to UTM
#' 
#' see https://stackoverflow.com/a/30225804
#' https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
#' @param lon [\code{numeric}] Longitude.
#' @param lat [\code{numeric}] Latitude
#' @param zone [\code{integer(1)}] UMT zone (optional).
#' @param south [\code{logical(1)}] \code{TRUE} if the coordinates are in the
#'              southern hemisphere, else \code{FALSE}.
#' @return [\code{list(2)}] \code{xy} the coordinates in UTM, 
#'         \code{crs} the UTM coordinate reference system (proj4string).
#' @export
lonlatToUTM <- function(lon, lat, zone = NULL, south = NULL){
  #FIXME
  # todo: check if lat/long in hh:mm:ss and convert them into
  #       decimal with the function 'lonlatToDeci()' (see below)
  lat_mean <- median(lat)
  lon_mean <- median(lon)
  if(is.null(zone)){
    zone <- getUTMzone(lat = lat_mean, lon = lon_mean)
  }
  if(is.null(south)){
    south <- ifelse(lat_mean > 0, "", "+south")
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
#' @param lon [\code{numeric}] Longitude.
#' @param lat [\code{numeric}] Latitude
#' @return [\code{integer(1)}] The UTM zone.
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
#' 
#' @param xy     [\code{matrix(,2)}] Columns = x and y coordinates.
#' @param CRSobj [\code{character(1)}] Coordinate reference system 
#'               (proj4string)
#' @return a 2-column-matrix (longitude (N), latitude (E))
#' @export
UTMTolonlat <- function(xy, CRSobj = NULL){
  #if(max(xy[,1]) > 834000) stop("y-values (northing) are larger than 834000")
  #if(min(xy[,1]) < 166000) stop("x-values (easting) are smaller than 166000")
  if(is.null(CRSobj)){
    CRSobj <- "+proj=utm +zone=32 +ellps=WGS84"
  } 
  ll <- data.frame(ID = 1:nrow(xy), X = xy[,1], Y = xy[,2])
  sp::coordinates(ll) <- c("X", "Y")
  sp::proj4string(ll) <- sp::CRS(CRSobj)
  xy <- sp::spTransform(ll, sp::CRS("+init=epsg:4326"))
  as.matrix(as.data.frame(xy)[,2:3])
}

# old name ll2dc
# conversion latitude longitude (hh:mm:ss into decimal
lonlatToDeci <- function(x){
  NS <- gsub('[^[:alpha:]]', "", x)
  w <- gsub('[^0-9:.]', "", x)
  V <- matrix(as.numeric(do.call(rbind, strsplit(w, ":"))), ncol = 3)
  pm <- 2* (grepl("N", NS) | grepl("E", NS)) - 1
  dec <- (V[,1] + V[,2] / 60 + V[,3]/3600) * pm
  return(dec)
}  

