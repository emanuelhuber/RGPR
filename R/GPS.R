
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
#' @param a (`data.frame`) GPGGA sentence information 
#' @return (`data.frame`) Columns = latitude, longitude, elevation and
#'         time
#' @export
#' @concept CRS
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

.getLonLatFromGNGGA <- function(a){
  trctime <- strptime(paste(Sys.Date(), a$UTC), '%Y-%m-%d %H%M%OS', tz='UTC')
  
  # # 3 latitude 
  # #  The format for NMEA coordinates is (d)ddmm.mmmm
  # lat <- as.numeric(a$lat) / 100
  # 
  # # 5 longitude
  # #  The format for NMEA coordinates is (d)ddmm.mmmm
  # lon <- as.numeric(a$lon) / 100
  
  # 3 latitude 
  #  The format for NMEA coordinates is (d)ddmm.mmmm
  lat <- sapply(a$lat, stringToLat, NW = a$NS, USE.NAMES = FALSE, nn = 2)
  
  # 5 longitude
  #  The format for NMEA coordinates is (d)ddmm.mmmm
  lon <- sapply(a$lon, stringToLat, NW = a$EW, USE.NAMES = FALSE, nn = 3)
  
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
#' check also https://stackoverflow.com/questions/176137/java-convert-lat-lon-to-utm
#' @param lon (`numeric`) Longitude.
#' @param lat (`numeric`) Latitude
#' @param zone (`integer[1]`) UMT zone (optional).
#' @param south (`logical[1]`) `TRUE` if the coordinates are in the
#'              southern hemisphere, else `FALSE`.
#' @param west  (`logical[1]`) `TRUE` if the longitude measures the 
#' angle west of the Prime Meridian; `FALSE` if the longitude measures the 
#' angle east of the Prime Meridian.
#' @return (`list[2]`) `xy` the coordinates in UTM, 
#'         `crs` the UTM coordinate reference system (proj4string).
#' @export
#' @concept CRS
lonLatToUTM <- function(lon, lat, zone = NULL, south = NULL, west = FALSE){
  # FIXME
  # - convert UTM to EPSG: https://gis.stackexchange.com/questions/365584/convert-utm-zone-into-epsg-code
  # todo: check if lat/long in hh:mm:ss and convert them into
  #       decimal with the function 'lonlatToDeci()' (see below)
  lat_mean <- median(lat)
  lon_mean <- median(lon)
  if(isTRUE(west)){
    if(lon_mean > 0 ) lon_mean <- -lon_mean
    if(lon_mean > 0 ) lon <- -lon
  }
  if(is.null(zone)){
    zone <- getUTMzone(lat = lat_mean, lon = lon_mean)
  }
  
  if(is.null(south) && lat_mean < 0){
    south <- TRUE
    lat <- -lat
    # print("SOUTH")
  }else if(isTRUE(south)){
    south <- TRUE
  }else{
    south <- FALSE
  }
  if(lat_mean < 0) lat <- -lat
  
  xy_crs <-  UTMToEPSG(zone, south)
  
  # ll <- data.frame(ID = 1:length(lat), x = lon, y = lat)
  # xy <- sf::st_as_sf(x      = ll,
  #                     coords = c("x", "y"),
  #                     crs    = 4326)
  # xy <- sf::st_transform(xy, crs = xy_crs)
  # return(list(xy = sf::st_coordinates(xy)[,1:2], crs = xy_crs))
  
  # sp::coordinates(ll) <- c("X", "Y")
  # sp::proj4string(ll) <- sp::CRS("+proj=longlat +datum=WGS84")
  # xy_crs <- paste0("+proj=utm ", south, "+zone=", zone, " +datum=WGS84",
  #                  " +units=m +no_defs", " +ellps=WGS84 +towgs84=0,0,0")
  
  xy <- sf::sf_project(from = "EPSG:4326", 
                       to   = paste0("EPSG:", xy_crs), 
                       pts  = cbind(lon, lat))
  
  return(list(xy = xy, crs = xy_crs))
  
}

#' Get UTM zone from lattidue and longitude
#'
#' @param lon (`numeric`) Longitude.
#' @param lat (`numeric`) Latitude
#' @return (`integer([1]`) The UTM zone.
#' @export
#' @concept CRS
getUTMzone <- function(lat, lon){
  # see https://stackoverflow.com/a/9188972
  # The formula is to simple: it does not work for the both 
  # UTM Zone Exceptions in Norway and Svalbard
  # Special zones for Svalbard
  lat <- median(lat)
  lon <- median(lon)
  if (lat >= 72.0 && lat < 84.0 ){
    if (lon >= 0.0  && lon <  9.0)  return(31) # 31X
    if (lon >= 9.0  && lon < 21.0)  return(33) # 33X
    if (lon >= 21.0 && lon < 33.0)  return(35) # 35X
    if (lon >= 33.0 && lon < 42.0)  return(37) # 37X
  }
  # Special zones for Norway
  if (lat >= 56.0 && lat < 64.0 ) {
    if (lon >= 0.0  && lon <  3.0) return(31) # 31V
    if (lon >= 3.0  && lon < 12.0) return(32) # 32V
  }
  zone <- (floor((lon + 180)/6) %% 60) + 1
  return(unique(zone)[1])
}

#' UTM to latitude-longitude
#' 
#' @param xy     (`matrix[,2]`) Columns = x and y coordinates.
#' @param CRSobj (`character[1]`) Coordinate reference system 
#'               (proj4string)
#' @return a 2-column-matrix (longitude N, latitude (E))
#' @export
#' @concept CRS
#' 
# not yet used!
UTMTolonlat <- function(xy, CRSobj = NULL){
  #if(max(xy[,1]) > 834000) stop("y-values (northing) are larger than 834000")
  #if(min(xy[,1]) < 166000) stop("x-values (easting) are smaller than 166000")
  if(is.null(CRSobj)){
    stop("You must set CRSobj")
  } 
  # ll <- data.frame(ID = 1:nrow(xy), X = xy[,1], Y = xy[,2])
  # sp::coordinates(ll) <- c("X", "Y")
  # sp::proj4string(ll) <- sp::CRS(CRSobj)
  # xy <- sp::spTransform(ll, sp::CRS("+init=epsg:4326"))
  # as.matrix(as.data.frame(xy)[,2:3])
  
  # ll_sf <- sf::st_as_sf(ll, coords = c("X", "Y"), crs = CRSobj)
  # ll_sf <- sf::st_transform(ll, crs = )
  
  xy[, 1:2] <- sf::sf_project(from = CRSobj, to = "EPSG:4326", pts = xy[, 1:2])
  return(xy)
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

#' EPGS code from UTM zone
#'
#' Returns the EPSG code from UTM zone. EPSG code is:
#'   32600+zone for positive latitudes and  32700+zone for negatives latitudes.
#' @param zone (`integer[1]`) the UTM zone.
#' @param south (`integer[1]`) `TRUE` if the UTM is located in 
#'              southern hemisphere.
#' @return (`integer[1]`) The EPSG code.
#' @export
#' @concept CRS
UTMToEPSG <- function(zone, south = FALSE){
  if(isTRUE(south)){
    return(32700 + as.integer(zone))
  }else{
    return(32600 + as.integer(zone))
  }
}


#' EPGS code from UTM zone string
#'
#' Returns the EPSG code from UTM zone string (e.g., '32N'). EPSG code is:
#'   32600+zone for positive latitudes and  32700+zone for negatives latitudes.
#' @param x (`character[1]`) The EPSG code string (e.g. 32N).
#' @return (`integer[1]`) The EPSG code.
#' @export
#' @concept CRS
UMTStringToEPSG <- function(x){
  pat <- extractPattern(x, "(\\d{1,2})(N|S)", start = 0, stop = -1)
  UTMToEPSG(zone = as.numeric(pat[1]),
            south = grepl("S", pat[2], ignore.case = TRUE))
}
