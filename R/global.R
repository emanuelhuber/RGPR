



# check lockBinding  (bibliotheque/documents/R/manuel-S4)


################## GPR PROCESSING ######################3


# TAPER WINDOWS
# tapertype = MinPhase.tapertype;
# tabpZL = nlags;

# switch lower(tapertype)
    # case 'cos'
        # taper = (sin(linspace(0,pi/2,tabpZL)).^2)';
        # ACFoutput(1:tabpZL) = ACFoutput(1:tabpZL).*taper;
        # ACFoutput(end-tabpZL+1:end) = ACFoutput(end-tabpZL+1:end).*
#           flipud(taper);
    # case 'triang'
        # taper = triang(2*tabpZL+1);
        # ACFoutput = ACFoutput.*taper;
    # case 'hamming'
        # taper = hamming(2*tabpZL+1);
        # ACFoutput = ACFoutput.*taper;
    # case 'bartlett'
        # taper = bartlett(2*tabpZL+1);
        # ACFoutput = ACFoutput.*taper;
    # case 'none'
        # % taper = ones(2*tabpZL+1,1);
        # % ACFoutput = ACFoutput-ACFoutput(end);
    # case 'limtaper'
        # % taper = ones(size(ACFoutput));
        # % taper = cs_taper(taper,'hann',round(length(ACFoutput)*0.25));
        # % ACFoutput = cs_taper(ACFoutput,'cos',round(length(ACFoutput)*0.25));
        # ACFoutput = cs_taper(ACFoutput,'cos',round(length(ACFoutput)*0.05));
    # case 'hann'
        # ACFoutput = cs_taper(ACFoutput,'hann');
    # case 'flattop'
        # taper = flattopwin(2*tabpZL+1);
        # ACFoutput = ACFoutput.*taper;
# end


##----------- helper functions -------------------##
# FID <- choose.files(caption = " txt files",filters = c("txt","*.txt"))
# output = list of data frame (one for each file from FID) 
#    with c("E","N","Z","TRACE") structure
#' read fiducial marker files
#' 
#' read fiducial marker files
#' @export
readFID <- function(FID,sep=","){
  myFid <- list() 
  for(i in seq_along(FID)){
    message("read ", FID[[i]], "...")
    A <- read.table(FID[[i]], sep=",", stringsAsFactors=FALSE,header=TRUE)
    colnames(A) <- toupper(colnames(A))
    if(!all(c("E","N","Z","TRACE") %in% colnames(A))){
      stop("The headers should be \"E\",\"N\",\"Z\",\"TRACE\"!\n")
    }
    myFid[[i]] <- A[,c("E","N","Z","TRACE")]
  }
  return(myFid)
}

#' read topo file
#' 
#' read topo file
#' @export
readTopo <- function(TOPO,sep=","){
  myTopo <- list() 
  for(i in seq_along(TOPO)){
    A <- read.table(TOPO[[i]], sep=sep, stringsAsFactors = FALSE, header = TRUE)
    colnames(A) <- toupper(colnames(A))
    if(ncol(A) < 3){
      stop("The headers should be \"E\",\"N\",\"Z\"!\n")
    }
    myTopo[[i]] <- A[,1:3]
  }
  return(myTopo)
}

# If there are more recorded positions than fiducials
# remove duplicates from the recorded positions
# P = Position (topo)
# F = fiducials (from GPR data)
rmDuplicates <- function(xyz, fid, tol = 0.1){
  dn <- nrow(xyz) - nrow(fid)
  if(dn > 0){
    dd <- as.matrix(dist(xyz[, 1:2]))
    diag(dd) <- NA
    dd[upper.tri(dd)] <- NA
    ij <- which(dd <= min(sort(dd)[dn], tol, arr.ind = TRUE))
    # handle "duplicate"
    if(length(ij) > 0){
      A <- list()
      it <- 1
      for(k in 1:nrow(xyz)){
        ik <- apply(ij, 1, function(x, k0 = k) any(x %in% k0))
        if(sum(ik) > 0){
          A[[it]] <- unname(ij[ik,])
          ij <- ij[-ik,]
          it <- it + 1
        }else if( !(k %in% unlist(A, use.names = FALSE))){
          A[[it]] <- k
          it <- it + 1
        }
      }
      ##TODO ##FIXME
      if(length(A) != nrow(fid)) stop("case to be implemented!!")
      ##TODO ##FIXME
      nL <- sapply(A, length)
      B <- matrix(ncol = length(A), nrow = prod(nL))
      for(k in 1:length(A)){
        B[,k] <- rep(A[[k]], prod(nL[-k]))
      }
      regres <- numeric(nrow(B))
      for(k in 1:nrow(B)){
        posTopo <- posLine(xyz[B[k,], 1:2])
        reg <- lm(fid$POSITION ~ posTopo)
        regres[k] <- summary(reg)$r.squared
      }
      iOK <- which.max(regres)
      xyz <- xyz[B[iOK,], ]
    }
  }
  return(xyz)
}

# tt = x@time
# pos = x@pos
# xyz <- TOp
# fid <- fi
# fidx = x@fid
addFid <- function(xyz, fid, tt, pos, fidx){
  dn <- nrow(xyz) - nrow(fid)
  if(dn > 0){
    dtime  <- c(0, diff(tt))
    sel    <- pos %in% fid$POSITION
    idtime <- which(dtime > 1.5 * sd(dtime[!sel]) & !sel)
    dn <- nrow(xyz) - nrow(fid)
    if(length(idtime) > 0){
      # en fait il faudrait regarder au cas par cas pour chaque
      # coordonnée auquelle il manque un fiducial
      if(length(idtime) >= dn){ 
        posTopo <- posLine(xyz[, 1:2])
        vx <- combn(seq_along(idtime), dn)
        regres <- numeric(ncol(vx))
        for(k in seq_len(ncol(vx))){
          dxt <- sort(c(idtime[vx[,k]], which(sel)))
          reg <- lm( pos[dxt] ~ posTopo)
          regres[k] <- summary(reg)$r.squared
        }
        iOK <- which.max(regres)
        dxt <- sort(c(idtime[vx[,iOK]], which(sel)))
        ffid <- fidx[dxt]
        ffid[1] <- "START"
        ffid[length(ffid)] <- "END"
        if(length(ffid) > 2){
          ffid[2:(length(ffid)-1)] <- paste0("FID", seq_len(length(ffid)-2))
        }
      }else{
        stop("TO BE IMPLEMENTED")
      }
      fid <- data.frame(TRACE    = dxt, 
                      POSITION = pos[dxt], 
                      COMMENT  = ffid)
      com <- paste0("  find ", dn," new FIDs!")
    }
  }
  return(fid)
}

rmxyz <- function(xyz, fid){
  dn <- nrow(xyz) - nrow(fid)
  if(dn > 0){
    vx <- combn(1:nrow(xyz), nrow(fid))
    regres <- numeric(ncol(vx))
    coeff <- numeric(ncol(vx))
    for(k in 1:ncol(vx)){
      posTopo <- posLine(xyz[vx[,k], 1:2])
      reg <- lm(fid$POSITION ~ posTopo)
      regres[k] <- summary(reg)$r.squared
      coeff[k] <- reg$coef[2]
    }
    if(nrow(fid) == 2){
      iOK <- which.min(abs(coeff - 1))
    }else{
      iOK <- which.max(regres)
    }
    #com <- "   remove a coord!"
    xyz <- xyz[vx[,iOK],]
  }
  return(xyz)
}

rmfid <- function(xyz, fid){
  dn <- nrow(xyz) - nrow(fid)
  if(dn < 0){
    # remove fiducials!!
    vx <- combn(1:nrow(fid), nrow(xyz))
    regres <- numeric(ncol(vx))
    coeff <- numeric(ncol(vx))
    posTopo <- posLine(xyz[, 1:2])
    for(k in 1:ncol(vx)){
      reg <- lm(fid$POSITION[vx[,k]] ~ posTopo)
      regres[k] <- summary(reg)$r.squared
      coeff[k] <- reg$coef[2]
    }
    if(nrow(fid) == 2){
      iOK <- which.min(abs(coeff - 1))
    }else{
      iOK <- which.max(regres)
    }
    com <- "   remove a fiducial!"
    fid <- fid[vx[,iOK],]
  }
  return(fid)
}

#' Link coordinates to fiducial marker
#'
#' To interpolate topo
#' @param y object of class GPSsurvey
#' @param xyz matrix of coordinates
#' @param pcode character vector (length(pcode) = nrow(xyz)) indicating which
#'              coordinates to which GPR data belongs
#' @param tol Tolerance to detect duplicates from topo data   
#' @export
linkCoordFid <- function(y, xyz, pcode, tol = 0.1 ){
  FIDs <- list()
  sn <- names(y)
  for(i in seq_along(y)){
    tp <- grep(sn[i], pcode)
    x <- y[[i]]
    fi <- exportFid(x)
    xyzp <- xyz[tp,]
    xyzp <- rmDuplicates(xyz = xyzp, fid = fi)
    fi <- addFid(xyz = xyzp, fid = fi, tt = x@time, pos = x@pos, fidx = x@fid)
    xyzp <- rmxyz(xyz = xyzp, fid = fi)
    fi <- rmfid(xyz = xyzp, fid = fi)
    if( nrow(fi) == nrow(xyzp)){
      fi$E <- xyzp[, "E" ]
      fi$N <- xyzp[, "N" ]
      fi$Z <- xyzp[, "Z" ]
      FIDs[[i]] <- fi
    }
  }
  return(FIDs)
}
# # 1. too much coordinates  n(coord) > n(fid)
# #   a. remove duplicates
# #     i. n(coord) = n(fid) -> OK
# #     ii. too much coordinates  n(coord) > n(fid)
# #       > maybe a fiducial is missing
# #         find potential fiducials
# #         make all possible combination fiducials - coords
# #         * n(coord) = n(fid) -> OK
# #         * ii. too much coordinates  n(coord) > n(fid)
# #           - remove a fiducial
 # fids <- linkCoordFid(y = SU, xyz = TO[, c("E", "N", "Z")], pcode = TO$PCODE)
# which(sapply(fids, is.null))

# for(i in seq_along(SU)){
  # posTopo <- posLine(fids[[i]][, c("E", "N")])
  # reg <- lm(fids[[i]]$POSITION ~ posTopo)
  # plot(posTopo, fids[[i]]$POSITION, main = SU@names[i], asp = 1)
  # abline(reg, col = "red")
  # title(sub = summary(reg)$r.squared)
  # Sys.sleep(0.5)
  # if(max(reg$res) < 1.5 | summary(reg)$r.squared > 0.99){
    # title("\n\nOK!")
    # Sys.sleep(1.5)
  # }else{
    # message(SU@names[i])
  # }
# }
  

##------------- FILENAME/FILEPATH/EXTENSION -------------------##
# return filename with correct extension (lower or upper case)
getFName <- function(fPath, ext = c(".rad", ".rd3")){
  fp <- file.path(dirname(fPath), .fNameWExt(fPath))
  ext <- tolower(ext)
  Ext <- toupper(ext)
  mfile <- list()
  for(i in seq_along(ext)){
    if(file.exists(f1 <- paste0(fp, Ext[i]))){
      #mfile[[gsub("^[.]",  "", ext[i])]] <- paste0(fp, Ext[i])
      mfile[[gsub("^[.]",  "", ext[i])]] <- f1
    }else if(file.exists(f2 <- paste0(fp, ext[i]))){
      #mfile[[gsub("^[.]",  "", ext[i])]] <- paste0(fp, ext[i])
      mfile[[gsub("^[.]",  "", ext[i])]] <- f2
    }else{
      stop("Files '", f1, "' and '", f2, "' do not exist!\n",
            "Check the filepath!") 
    }
  }
  return(mfile)
}

# NAME: safe file path
# test if the file already exists
# if yes, add a suffix to the filepath
safeFPath <- function(fPath = NULL){
  dirName   <- dirname(fPath)
  fName <- .fNameWExt(fPath)
  ext <- .fExt(fPath)
  if(dirName == '.'){
    fPath <- fName
  }else{
    fPath <- paste0(dirName, '/' , fName)
  }
  fPath_orgi <- fPath
  k <- 0
  while(file.exists(paste0(fPath, ".", ext)) || 
          file.exists( paste0(fPath, ".HD"))){
    fPath <- paste0(fPath_orgi, "_", k)
    k <- k + 1
  }
  newfPath <- paste0(fPath, ".", ext)
  return(newfPath)
}

#' Trim string
#'
#' returns string w/o leading or trailing whitespace
#' @export
trimStr <- function (x) gsub("^\\s+|\\s+$", "", x)

# return filename without extension
#' @export
.fNameWExt <- function(x){
  unlist(lapply(strsplit(basename(x),"[.]"), head , 1 ), use.names = FALSE)
}

# return the file extension.

#' @export
.fExt <- function(x){
#   cat("with caution... because split \'.\' may not be so good\n")
  unlist(lapply(strsplit(basename(x),"[.]"), tail , 1 ), use.names = FALSE)
}


.saveTempFile <- function(x){
  tmpf <- tempfile(x@name)
  writeGPR(x, type = "rds", overwrite = FALSE, fPath = tmpf)
  return(paste0(tmpf, ".rds"))
}

##------------- COLOR FUNCTIONS -------------------##
#' @name palGPR
#' @rdname palGPR
#' @export
palGPR <- function(colPal="default", n = 101, power = 1, returnNames = FALSE){
  colPal <- gsub("gray", "grey", x= colPal)
  tmp <- structure(list(
    default = grDevices::colorRampPalette(c("#1C007C", "#1B0086", "#1A0091", 
                "#18009C",
                "#1600A7", "#1400B2", "#1100C3", "#0E00CF", "#0A00E0",
                "#0300F5", "#0001FF", "#080FFF", "#1521FF", "#2232FF",
                "#2E42FF", "#3B52FF", "#4862FF", "#5470FF", "#617FFF",
                "#6E8CFF", "#7F9EFF", "#8CAAFF", "#98B5FF", "#A5C1FF",
                "#B2CBFF", "#BFD5FF", "#CBDFFF", "#D8E7FF", "#E5F0FF",
                "#F2F7FF", "#FFFCFB", "#FFF4F0", "#FFECE5", "#FFE3DA",
                "#FFDACE", "#FFCEC0", "#FFC4B5", "#FFB9AA", "#FFAE9E",
                "#FF9F90", "#FF9485", "#FF877A", "#FF766B", "#FF6960",
                "#FF5B55", "#FF4946", "#FF3B4E", "#FF3045", "#FF253D",
                "#FF1632", "#FF0B2A", "#FF0022", "#F70023", "#EE0023",
                "#E50023", "#DC0024", "#D30024", "#CA0024", "#C20024",
                "#B70023", "#AF0023", "#A70023", "#9C0022"))(n),
    hcl_0 = colorspace::diverge_hcl(n,power=1),
    # blue - white - red (fade)
    hcl_1 = colorspace::diverge_hcl(n, c = 100, l = c(50, 90), power = power), 
    # blue - white - orange (fade)
    hcl_2 = colorspace::diverge_hcl(n, h = c(246, 40), c = 96, 
                        l = c(65, 90), power=power),
    #  green - white- orange (fade)
    hcl_3 = colorspace::diverge_hcl(n, h = c(130, 43), c = 100, 
                        l = c(70, 90), power=power), 
    # blue/violet - white - red/violet 
    hcl_4 = colorspace::diverge_hcl(n, h = c(255, 330), 
                        l = c(40, 90), power=power), 
    # rose - white - turquise (fade)
    hcl_5 = colorspace::diverge_hcl(n, h = c(20, 200), c = 90, 
                        l = c(70, 95), power=power),  
    #  blue - white - red (vivid)
    hcl_6 = colorspace::diverge_hcl(n, h = c(246, 10), c = 120, 
                          l = c(30, 90), power=power), 
    # blue - white - red (tern)
    hcl_7 = colorspace::diverge_hcl(n, h = c(220, 10), c = 100, 
                        l = c(20, 90), power=power), 
    # blue - white - red (fade)
    hcl_8 = colorspace::diverge_hcl(n, h = c(250, 10), c = 150, 
                        l = c(30, 90), power=power), 
    grey3 = colorspace::diverge_hcl(n, h = c(300, 1), c = 1, 
            l = c(1, 100), power=power), 
    # too light
    grey1 = colorspace::sequential_hcl(n, h = c(1, 300), c = 0, 
                          l = c(10, 100), power=power), 
    #  too dark  
    grey2 = colorspace::sequential_hcl(n, h = c(300, 100), c = 0, 
              l = c(120, 10), power=power), 
    grey = colorspace::sequential_hcl(n, h = c(190, 1), c = 10, 
                          l = c(1, 110), power=power),
    rainbow = grDevices::colorRampPalette(rainbow(13),interpolate ="spline")(n),
    rainbow_hcl = colorspace::rainbow_hcl(n,c=100,l=60)
  ))
  if(returnNames){
   return( names(tmp) )
  }
  (tmp[[match(colPal, names(tmp))]])
}

#' Plot single colour palette
#' 
#' source: vignette of the R-package "colorspace" (Color Space Manipulation) 
#' @examples
#' plotPal(palGPR("hcl_5"))
#' @name plotPal
#' @rdname palGPR
#' @export
plotPal <- function(col, border = NA){
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, 
        xlab = "",  ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

#' Colour palette
#'
#' @examples
#' displayPalGPR()
#' @name displayPalGPR
#' @rdname palGPR
#' @export
displayPalGPR <- function(){
   op <- par(no.readonly=TRUE)
   par(mai=op$mai + c(0,1,0,0))
  pNames <- palGPR(returnNames=TRUE)
  n <- 101
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, length(pNames)), 
        axes = FALSE, xlab = "", ylab = "")
  for(i in seq_along(pNames)){
    myPal  <- palGPR(colPal=pNames[i], n=n)
    rect(0:(n-1)/n, i-1/3, 1:n/n, i + 1/3, col = myPal, border = NA)
    mtext(pNames[i], side=2, at=i, adj = 1, las = 1)
  }
  title("Colour palettes from RGPR (palGPR)")
  par(op)
}

#' Return color from palette
#'
#' @export
colFromPal <- function(A , col = palGPR(n=101)){
  CCY = (A-min(A,na.rm=TRUE))/(max(A,na.rm=TRUE)-min(A,na.rm=TRUE))
  ClimY <- range(CCY,na.rm=TRUE)
  ClenY <- ClimY[2] - ClimY[1] + 1
  return(col[ (CCY)*(length(col)-1)+1 ] )
}
#--------------------------------#


# Compute the orientation angle of GPR profile
gprAngle <- function(x){
  dEN <- x@coord[1,1:2] - tail(x@coord[,1:2],1)
  return(atan2(dEN[2], dEN[1]))
}

# is angle b between aref - 1/2*atol and aref + 1/2*atol?
inBetAngle <- function(aref, b, atol = pi/10){
  dot <- cos(b)*cos(aref) + sin(b) * sin(aref)
  return(acos(dot) <= atol)
}


  
# select a box on plot(mySurvey) and return list(xlim,ylim)
# that can be used in plot3D:
#   plot(mySurvey,asp=1)
#   bbox <- selectBBox()
#   plot3D(mySurvey, addTopo=TRUE,clip=30, xlim=bbox$xlim,
# ylim=bbox$ylim, add=FALSE)

selectBBox <- function(border="red",lwd=2,...){
  bbox <- locator(type="p",n=2)
  LIM <- sapply(bbox, range)
  rect(LIM[1,"x"], LIM[1,"y"], LIM[2,"x"], LIM[2,"y"], border=border)
  return(list("xlim"=LIM[,"x"], "ylim" =LIM[,"y"]))
}

# NOT USED!
# .fidpos <- function(xyz,fid){
#   return(xyz[trimStr(fid)!="",,drop=FALSE])
# }

.plotLine <- function(xyz,...){
  lines(xyz[,1:2],...)
}

.plotArrows <- function(xyz, ...){
  arrows(xyz[nrow(xyz)-1,1], xyz[nrow(xyz)-1,2], xyz[nrow(xyz),1], 
         xyz[nrow(xyz),2], ...)
}

.whichMin <- function(x,y){
  which.min(abs(x-y))
}

.which <- function(x,y){
  which(x==y)
}

.lengthList <- function(x){
  if(typeof(x)=="list"){
    return(length(x))
    # print(typeof(x))
  }else{
    return(1)
  }
}

#-- not exported!
# Georeferencing
#
# Perform on a set of x,y coordinates
# (1) a translation by \code{-cloc}, then
# (2) a rotation by \code{alpha} (radian), and (3)
# a translation by \code{creg}. If \code{creg}
# is \code{NULL}, then \code{creg} is set equal
# to \code{cloc}.
# @param x A matrix with the first two columns corresponding
#          to coordinates.
# @param alpha A length-one numeric vector corresponding to 
#              the rotation angle in radians. If \code{alpha = NULL},
#              \code{alpha} is estimated from the pairs of points in
#              the local reference system (\code{ploc}) and in the
#              regional reference system (\code{preg}).
# @param cloc A length-two numeric vector corresponding to the coordinate
#             center of the local reference system
# @param creg A length-two numeric vector corresponding to the coordinate
#             center of the regional reference system. Setting 
#             \code{creg = NULL} (default) is equivalent to apply a rotation
#             of angle \code{alpha} and center \code{cloc}.
# @param ploc A matrix with the first two columns corresponding
#            to coordinates in the local reference system.
# @param preg A matrix with the first two columns corresponding
#             to coordinates in the regional reference system.
# @param FUN If \code{alpha = NULL}, a function to estimate the rotation angle
#            from the angles computed for each pairs of coordinates of
#            \code{ploc}-\code{preg}.
.georef <- function(x, alpha = NULL, cloc = c(0,0), creg = NULL,
                   ploc = NULL, preg = NULL, FUN = mean){
  x0 <- as.matrix(unname(x[,1:2, drop = FALSE]))
  if(is.null(alpha)){
    cloc <- as.double(cloc)
    creg <- as.double(creg)
    ploc <- as.matrix(ploc)
    preg <- as.matrix(preg)
    alphaloc <- atan2(ploc[,1] - cloc[1], ploc[,2] - cloc[2])
    alphareg <- atan2(preg[,1] - creg[1], preg[,2] - creg[2])
    alpha <- alphareg - alphaloc
    message(paste0("rotation angles: ", 
                   paste0(round(alpha,4), collapse = ", "), "."))
    alpha <- FUN(alpha)
  }
  ROT <- matrix(c( cos(alpha), sin(alpha),
                  -sin(alpha), cos(alpha)), nrow=2, ncol=2)
  TRL <-  matrix(as.double(cloc[1:2]), nrow = nrow(x0), 
                 ncol = 2, byrow = TRUE)
  if(is.null(creg)){
    TRL2 <- TRL
  }else{
    TRL2 <-  matrix(creg[1:2], nrow = nrow(x0), ncol = 2, byrow = TRUE)
  }
  x[,1:2] <- (x0 - TRL) %*% ROT + TRL2
  return(x)
}

#' Position on a multiline
#'
#' Position on a multiline
#' @export
posLine <- function(loc,last=FALSE){
  loc <- as.matrix(loc)
  all_dist <- cumsum(c(0,sqrt(apply(diff(loc)^2,1,sum))))
  if(last){
        return(tail(all_dist,1))
  }else{
        return(as.numeric(all_dist))
  }
}
  
#' latitude-longitude to UTM
#' 
#' see https://stackoverflow.com/a/30225804
#' @export
latlongToUTM <- function(lat, long, zone = NULL, south = FALSE){
  # todo: check if lat/long in hh:mm:ss and convert them into
  #       decimal with the function 'll2dc()' (see below)
  if(is.null(zone)){
    # see https://stackoverflow.com/a/9188972
    #  The formula is to simple: it does not work for the both 
    # UTM Zone Exceptions in Norway and Svalbard –
    zone <- (floor((long + 180)/6) %% 60) + 1
    zone <- unique(zone)[1]
  }
  ll <- data.frame(ID = 1:length(lat), X = long, Y = lat)
  sp::coordinates(ll) <- c("X", "Y")
  sp::proj4string(ll) <- sp::CRS("+proj=longlat +datum=WGS84")
  if(isTRUE(south)){
    south <- "+south "
  }else{
    south <- ""
  }
  xy <- sp::spTransform(ll, sp::CRS(paste0("+proj=utm ", south, "+zone=", zone,
                                           " ellps=WGS84")))
  return(as.matrix(as.data.frame(xy)[,2:3]))
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
  
.doubleVector <- function(v,n=2L){
  if(n > 1){
    m <- length(v)
    dxpos <- rep( diff(v)/n,n-1)
    vv <- v[-m] + rep(seq(1,n-1),each=m-1)*dxpos
    xvalues  <- sort(c(v,vv ,v[m] + cumsum(rep(dxpos[length(dxpos)],n-1))))
    xvalues <- xvalues[1:(length(xvalues))]
    return(xvalues)
  }else{
    return(v)
  }
}

#--------------------------------
# wapply: A faster (but less functional) "rollapply" for vector setups
# April 23, 2013
# By A.N. Spiess, senior scientist at the Department of Andrology at the 
# University Hospital Hamburg-Eppendorf
# This is what turned out (wapply for "window apply")
wapply <- function(x=NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- length(x)
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
   
  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}

#' Wapply on the row of a matrix (windowed)
#'
#' NOT CURRENTLY USED
#' mod by MANU
#' @export
wapplyRow <- function(x = NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- nrow(x)
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))

  OUT <- lapply(SEQ2, function(a) FUN(x[a,,drop=FALSE], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}

#' Wapply on the row of a matrix (windowed + CENTERED)
#'
#' NOT CURRENTLY USED
#' mod by MANU
#' @export
wapplyRowC <- function(x = NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- nrow(x)
  SEQ1 <- seq(-(width-1)/2 + 1, lenX -(width-1)/2, by = by)
  SEQ2 <- lapply(SEQ1, function(x){ xnew <- x:(x + width - 1)
                                    xnew <- xnew[xnew > 0]
                                    xnew <- xnew[xnew <= lenX]})
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a,,drop=FALSE], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}

# based on wapply and modified by Manu
# centered moving window
# return a matrix of the same dimension than x
# some border effect at a distance < width/2 at the first and last col/row
wapplyMat <- function(x = NULL, width = NULL, by = NULL, FUN = NULL, 
                      MARGIN = 1, ...){
  FUN <- match.fun(FUN)
  width <- ifelse(width %% 2 == 0, width + 1, width)
  if (is.null(by)) by <- width
  lenX <- ifelse(MARGIN == 1, ncol(x), nrow(x))
  SEQ1 <- seq(-(width-1)/2 + 1, lenX -(width-1)/2, by = by)
  SEQ2 <- lapply(SEQ1, function(x){ xnew <- x:(x + width - 1)
                                    xnew <- xnew[xnew > 0]
                                    xnew <- xnew[xnew <= lenX]})
  if(MARGIN == 1){
    OUT <- lapply(SEQ2, function(a) apply(x[, a, drop = FALSE], MARGIN, FUN))
  }else if( MARGIN == 2) {
    OUT <- lapply(SEQ2, function(a) apply(x[a,, drop = FALSE], MARGIN, FUN))
  }
  OUT <- base::simplify2array(OUT, higher = TRUE)
  if(MARGIN == 2){
    return(t(OUT))
  }else{
    return(OUT)
  }
}


# based on wapply and modified by Manu
# not centered moving window!
# return a matrix of with smaller dimension than x (margin - 2*width)
wapplyMat2 <- function(x = NULL, width = NULL, by = NULL, FUN = NULL, 
                      MARGIN = 1, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- ifelse(MARGIN == 1, ncol(x), nrow(x))
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
  if(MARGIN == 1){
    OUT <- lapply(SEQ2, function(a) apply(x[, a, drop = FALSE], MARGIN, FUN))
  }else if( MARGIN == 2) {
    OUT <- lapply(SEQ2, function(a) apply(x[a,, drop = FALSE], MARGIN, FUN))
  }
  OUT <- base::simplify2array(OUT, higher = TRUE)
  if(MARGIN == 2){
    return(t(OUT))
  }else{
    return(OUT)
  }
}

xyToLine <- function(x){
  sp::Line(x[,1:2])
}

LineToLines <- function(i,pp, myNames){
  sp::Lines(pp[i],myNames[i])
}


#--------------------------------------------#
#---------------- SETGENERIC ----------------#
# setGenericVerif <- function(x,y){
#   if(!isGeneric(x)){
#     setGeneric(x,y)
#   }else{
#     cat("setGeneric", x,"already exists!\n")
#   }
# }
setGenericVerif <- function(x,y){setGeneric(x,y)}


#------------------------------

setGenericVerif("as.SpatialPoints", function(x) 
standardGeneric("as.SpatialPoints"))

setGenericVerif("as.SpatialLines", function(x) 
standardGeneric("as.SpatialLines"))


#------------------------------
#' @name coordref
#' @rdname coordref-methods
#' @exportMethod coordref
setGenericVerif("coordref", function(x) standardGeneric("coordref"))


#' @name coordref<-
#' @rdname coordref-methods
#' @exportMethod coordref
setGenericVerif("coordref<-", function(x, value) standardGeneric("coordref<-"))

#' @name intersections
#' @rdname intersections-methods
#' @exportMethod intersections
setGenericVerif("intersections", function(x) standardGeneric("intersections"))

#' @name filepath
#' @rdname filepath-methods
#' @exportMethod filepath
setGenericVerif("filepath", function(x) standardGeneric("filepath"))

#' @name filepath<-
#' @rdname filepath-methods
#' @exportMethod filepath<-
setGenericVerif("filepath<-", function(x, value) standardGeneric("filepath<-"))

#' @name coords
#' @rdname coords-methods
#' @exportMethod coords
setGenericVerif("coords", function(x,i) standardGeneric("coords"))

#' @name coords<-
#' @rdname coords-methods
#' @exportMethod coords<-
setGenericVerif("coords<-",function(x,value){standardGeneric("coords<-")})

#' @name coord
#' @rdname coord-methods
#' @exportMethod coord
setGenericVerif("coord", function(x, i, ...) standardGeneric("coord"))

#' @name coord<-
#' @rdname coord-methods
#' @exportMethod coord<-
setGenericVerif("coord<-",function(x,value){standardGeneric("coord<-")})


#' @name svDate
#' @rdname svDate
#' @export
setGenericVerif("svDate", function(x, i, ...) standardGeneric("svDate"))

#' @name svDate<-
#' @rdname svDate
#' @export
setGenericVerif("svDate<-",function(x,value){standardGeneric("svDate<-")})


#' @name vel
#' @rdname vel
#' @export
setGenericVerif("vel", function(x) standardGeneric("vel"))

#' @name vel<-
#' @rdname vel
#' @export
setGenericVerif("vel<-",function(x,value){standardGeneric("vel<-")})

#' @name ann
#' @rdname ann
#' @export
setGenericVerif("ann", function(x) standardGeneric("ann"))

#' @name ann<-
#' @rdname ann
#' @export
setGenericVerif("ann<-",function(x,value){standardGeneric("ann<-")})

#' @name name
#' @rdname name
#' @export
setGenericVerif("name", function(x) standardGeneric("name"))

#' @name name<-
#' @rdname name
#' @export
setGenericVerif("name<-",function(x,value){standardGeneric("name<-")})

#' @name depthunit
#' @rdname depthunit
#' @export
setGenericVerif("depthunit", function(x) standardGeneric("depthunit"))

#' @name depthunit<-
#' @rdname depthunit
#' @export
setGenericVerif("depthunit<-",function(x,value){standardGeneric("depthunit<-")})

#' @name posunit
#' @rdname posunit
#' @export
setGenericVerif("posunit", function(x) standardGeneric("posunit"))

#' @name posunit<-
#' @rdname posunit
#' @export
setGenericVerif("posunit<-",function(x,value){standardGeneric("posunit<-")})

#' @name crs
#' @rdname crs
#' @export
setGenericVerif("crs", function(x) standardGeneric("crs"))

#' @name crs<-
#' @rdname crs
#' @export
setGenericVerif("crs<-",function(x,value){standardGeneric("crs<-")})

#' @name depth
#' @rdname depth
#' @export
setGenericVerif("depth", function(x) standardGeneric("depth"))

#' @name depth<-
#' @rdname depth
#' @export
setGenericVerif("depth<-", function(x,value) standardGeneric("depth<-"))

#' @name pos
#' @rdname pos
#' @export
setGenericVerif("pos", function(x) standardGeneric("pos"))

#' @name pos<-
#' @rdname pos
#' @export
setGenericVerif("pos<-", function(x,value) standardGeneric("pos<-"))

#' @name time0
#' @rdname time0
#' @export
setGenericVerif("time0", function(x) standardGeneric("time0"))

#' @name time0<-
#' @rdname time0
#' @export
setGenericVerif("time0<-",function(x,value){standardGeneric("time0<-")})

#' Time of data collection for each trace
#'
#' @name trTime
#' @rdname trTime
#' @export
setGenericVerif("trTime", function(x) standardGeneric("trTime"))

#' @name fid
#' @rdname fid
#' @export
setGenericVerif("fid", function(x) standardGeneric("fid"))

#' @name fid<-
#' @rdname fid
#' @export
setGenericVerif("fid<-",function(x,value){standardGeneric("fid<-")})

#' @name values
#' @rdname values
#' @export
setGenericVerif("values", function(x) standardGeneric("values"))

#' @name values<-
#' @rdname values
#' @export
setGenericVerif("values<-", function(x,value) standardGeneric("values<-"))

#' @name processing
#' @rdname processing
#' @export
setGenericVerif("processing", function(x) standardGeneric("processing"))

#' @name proc
#' @rdname proc
#' @export
setGenericVerif("proc", function(x) standardGeneric("proc"))


#' @name proc<-
#' @rdname proc
#' @export
setGenericVerif("proc<-",function(x,value){standardGeneric("proc<-")})

#' @name antsep
#' @rdname antsep
#' @export
setGenericVerif("antsep", function(x) standardGeneric("antsep"))

#' @name antsep<-
#' @rdname antsep
#' @export
setGenericVerif("antsep<-",function(x,value){standardGeneric("antsep<-")})                
                

#' @name surveymode
#' @rdname surveymode
#' @export
setGenericVerif("surveymode", function(x) standardGeneric("surveymode"))

#' @name surveymode<-
#' @rdname surveymode
#' @export
setGenericVerif("surveymode<-",
                function(x, value){standardGeneric("surveymode<-")})    

#' @name isCMP
#' @rdname isCMP
#' @export
setGenericVerif("isCMP", function(x) standardGeneric("isCMP"))



#' @name description
#' @rdname description
#' @export
setGenericVerif("description", function(x) standardGeneric("description"))

#' @name description<-
#' @rdname description
#' @export
setGenericVerif("description<-", function(x, value) 
standardGeneric("description<-"))
#####
setGenericVerif("papply", function(x, prc = NULL) standardGeneric("papply"))
##########
                  
#------------------------------GPR
#' @name gethd
#' @rdname gethd
#' @export
setGenericVerif("gethd", function(x,hd=NULL) standardGeneric("gethd"))

#' @name plotAmpl
#' @rdname plotAmpl
#' @export
setGenericVerif("plotAmpl", function(x, FUN = mean, add = FALSE, 
                all = FALSE,...) standardGeneric("plotAmpl"))
setGenericVerif("ampl", function(x, FUN=mean, ...) standardGeneric("ampl"))

#' @name trRmDuplicates
#' @rdname trRmDuplicates
#' @export
setGenericVerif("trRmDuplicates", function(x, tol = NULL) 
  standardGeneric("trRmDuplicates"))

#' @name interpPos
#' @rdname interpPos
#' @export
setGenericVerif("interpPos", function(x, topo, plot = FALSE, r = NULL, tol = NULL, 
                   method = c("linear", "spline", "pchip"), ...) 
    standardGeneric("interpPos"))

#' @name regInterpPos
#' @rdname regInterpPos
#' @export
setGenericVerif("regInterpPos", function(x, type = c("linear", "cosine"), 
          dx = NULL)  standardGeneric("regInterpPos"))

#' @name relPos
#' @rdname relPos
#' @export
setGenericVerif("relPos", function(x) 
    standardGeneric("relPos"))
    
#' @name readGPR
#' @rdname readGPR
#' @export
setGenericVerif("readGPR", function(fPath, desc = "") standardGeneric("readGPR")
)

#' @name writeGPR
#' @rdname writeGPR
#' @export
setGeneric("writeGPR", function(x, fPath = NULL, 
                type = c("DT1", "rds", "ASCII", "xyzv"),
                overwrite = FALSE, ...){ standardGeneric("writeGPR")})

#' @name exportCoord
#' @rdname exportCoord
#' @export
setGenericVerif("exportCoord",  
          function(x, type = c("SpatialPoints", "SpatialLines", "ASCII"),
  fPath = NULL, driver = "ESRI Shapefile", ...) standardGeneric("exportCoord"))
#' @name exportFid
#' @rdname exportFid
#' @export
setGenericVerif("exportFid", function(x, fPath = NULL) 
                  standardGeneric("exportFid"))

#' @name exportProc
#' @rdname exportProc
#' @export
setGenericVerif("exportProc",  function(x,fPath=NULL,sep="\t", row.names=FALSE,
              col.names=FALSE, ...) standardGeneric("exportProc"))

#' @name reverse
#' @rdname reverse
#' @export
setGenericVerif("reverse", function(x, id = NULL,  tol = 0.3) 
                standardGeneric("reverse"))
  
#' @name shiftEst
#' @rdname shiftEst
#' @export
setGenericVerif("shiftEst", function(x, y = NULL, 
                method=c("phase", "WSSD"), dxy = NULL, ...) 
                standardGeneric("shiftEst"))

#' @name NMOCor
#' @rdname NMOCor-methods
#' @exportMethod NMOCor
setGenericVerif("NMOCor", function(x, v = NULL) 
  standardGeneric("NMOCor"))


#' @name CMPAnalysis
#' @rdname CMPAnalysis-methods
#' @exportMethod CMPAnalysis
setGenericVerif("CMPAnalysis", function(x, method = c("semblance", 
               "winsemblance", "wincoherence", "wincoherence2"), v = NULL, 
               w = NULL) standardGeneric("CMPAnalysis"))

setGenericVerif("migration", function(x,type=c("static","kirchhoff"), ...) 
standardGeneric("migration"))
setGenericVerif("upsample", function(x,n) standardGeneric("upsample"))
setGenericVerif("timeCorOffset", function(x, t0 = NULL, c0 = 0.299) 
  standardGeneric("timeCorOffset"))

#' @name filter1D
#' @rdname filter1D
setGenericVerif("filter1D", function(x, type = c("median", "hampel", 
                "Gaussian"), ...) standardGeneric("filter1D"))

#' @name filter2D
#' @rdname filter2D
#' @export
setGenericVerif("filter2D", function(x, type=c("median3x3", "adimpro"), ...) 
                standardGeneric("filter2D"))
                
setGenericVerif("dewow", function(x, type=c("MAD", "Gaussian"), w ) 
                standardGeneric("dewow"))
                
setGenericVerif("gain", function(x, type=c("power", "exp", "agc"), ...) 
                standardGeneric("gain")) 

setGenericVerif("trAmplCor", 
                function(x, type=c("power", "exp", "agc"),  ...) 
                standardGeneric("trAmplCor"))
                
setGenericVerif("dcshift", function(x, u=1:10, FUN=mean) 
                standardGeneric("dcshift"))
                
setGenericVerif("firstBreak", function(x, method = c("coppens", "coppens2",
                "threshold",  "MER"), thr = 0.12, w = 11, ns = NULL, 
                bet = NULL)
                standardGeneric("firstBreak"))

setGenericVerif("clip", function(x, Amax=NULL,Amin=NULL) 
                standardGeneric("clip"))
                
setGenericVerif("gammaCorrection", function(x, a = 1, b = 1) 
                standardGeneric("gammaCorrection"))
                
setGenericVerif("traceScaling", function(x, 
                  type = c("stat", "min-max", "95", "eq", "sum", "rms", 
                           "mad", "invNormal")) 
                  standardGeneric("traceScaling"))

setGenericVerif("spec", function(x, type = c("f-x", "f-k"), plotSpec = TRUE, 
                unwrapPhase = TRUE, ...) standardGeneric("spec"))
                
setGenericVerif("fFilter", function(x, f = 100, 
                type = c('low', 'high', 'bandpass'),
                L = 257, plotSpec = FALSE) standardGeneric("fFilter"))
                
setGenericVerif("fkFilter", function(x, fk = NULL, L = c(5, 5), npad = 1) 
                standardGeneric("fkFilter"))

setGenericVerif("eigenFilter", function(x, eigenvalue = NA, center = TRUE, 
                                        scale = FALSE) 
standardGeneric("eigenFilter"))

setGenericVerif("traceShift", function(x,  ts, method = c("spline", "linear", 
                "nearest", "pchip", "cubic", "none"), crop = TRUE) 
                standardGeneric("traceShift"))
                
setGenericVerif("traceAverage", function(x, w = NULL, FUN = mean, ...) 
                standardGeneric("traceAverage"))

setGenericVerif("time0Cor",  function(x, t0 = NULL, 
                method = c("spline", "linear", "nearest", "pchip", "cubic", 
                "none"), crop = TRUE, keep = 0) 
                standardGeneric("time0Cor"))

setGenericVerif("deconv", function(x, method=c("spiking", "wavelet",
                "min-phase", "mixed-phase"), ...) standardGeneric("deconv"))
setGenericVerif("conv1D", function(x, w) standardGeneric("conv1D"))
setGenericVerif("conv2D", function(x, w) standardGeneric("conv2D"))
setGenericVerif("rotatePhase", function(x, phi) standardGeneric("rotatePhase"))


#------------------------------GRPsurvey
#' @name getGPR
#' @rdname getGPR
#' @export
setGenericVerif("getGPR", function(x,id) standardGeneric("getGPR"))

#' @name surveyIntersect
#' @rdname surveyIntersect
#' @export
setGenericVerif("surveyIntersect", function(x) 
                standardGeneric("surveyIntersect"))
#' @name writeSurvey
#' @rdname writeSurvey
#' @export
setGenericVerif("writeSurvey", function(x, fPath, overwrite=FALSE){ 
                standardGeneric("writeSurvey")})

#' Georeferencing
#'
#' Perform on a set of x,y coordinates
#' (1) a translation by \code{-cloc}, then
#' (2) a rotation by \code{alpha} (radian), and (3)
#' a translation by \code{creg}. If \code{creg}
#' is \code{NULL}, then \code{creg} is set equal
#' to \code{cloc}.
#' @param x A matrix with the first two columns corresponding
#'          to coordinates.
#' @param alpha A length-one numeric vector corresponding to 
#'              the rotation angle in radians. If \code{alpha = NULL},
#'              \code{alpha} is estimated from the pairs of points in
#'              the local reference system (\code{ploc}) and in the
#'              regional reference system (\code{preg}).
#' @param cloc A length-two numeric vector corresponding to the coordinate
#'             center of the local reference system
#' @param creg A length-two numeric vector corresponding to the coordinate
#'             center of the regional reference system. Setting 
#'             \code{creg = NULL} (default) is equivalent to apply a rotation
#'             of angle \code{alpha} and center \code{cloc}.
#' @param ploc A matrix with the first two columns corresponding
#'             to coordinates in the local reference system.
#' @param preg A matrix with the first two columns corresponding
#'             to coordinates in the regional reference system.
#' @param FUN If \code{alpha = NULL}, a function to estimate the rotation angle
#'            from the angles computed for each pairs of coordinates of
#'            \code{ploc}-\code{preg}.
#' @export
#' @name georef
#' @rdname georef
setGenericVerif("georef", function(x, alpha = NULL, cloc = c(0,0), creg = NULL,
                   ploc = NULL, preg = NULL, FUN = mean){ 
                standardGeneric("georef")})
                
#------------------------------BOTH
setGenericVerif("plot3DRGL", 
          function(x, addTopo = FALSE, clip = NULL, normalize = NULL, 
                  nupspl = NULL, add = TRUE, xlim = NULL, ylim = NULL, 
                  zlim = NULL,...) 
standardGeneric("plot3DRGL"))

setGenericVerif("exportPDF", function(x, fPath = NULL, addTopo = FALSE, 
                clip = NULL, normalize = NULL, nupspl = NULL, ...) 
standardGeneric("exportPDF"))

#setGenericVerif("adimproSmooth", function(x,hmax=2,...) standardGeneric("
# adimproSmooth"))

#---------------------- DELINEATIONS ---------------------#
#' @name delineate
#' @rdname delineation
#' @export
setGenericVerif("delineate", function(x, name = NULL,
                type = c("raster", "wiggles"),
                addTopo = FALSE, nupspl = NULL, n = 10000, ...) 
                  standardGeneric("delineate"))
#' @name rmDelineations<-
#' @rdname delineation
#' @export
setGenericVerif("rmDelineations<-", function(x,value=NULL) 
                  standardGeneric("rmDelineations<-"))
#' @name delineations
#' @rdname delineation
#' @export
setGenericVerif("delineations", function(x,sel=NULL,...) 
                  standardGeneric("delineations"))
#' @name addDelineation
#' @rdname delineation
#' @export
setGenericVerif("addDelineation", function(x,...) 
                  standardGeneric("addDelineation"))
setGenericVerif("showDelineations", function(x,sel=NULL,...) 
                  standardGeneric("showDelineations"))
#' @name exportDelineations
#' @rdname delineation
#' @export
setGenericVerif("exportDelineations", function(x, dirpath="") 
                  standardGeneric("exportDelineations"))
#' @name plotDelineations3D
#' @rdname delineation
#' @export
setGenericVerif("plotDelineations3D", 
                function(x,sel=NULL,col=NULL,add=TRUE,...)
                standardGeneric("plotDelineations3D"))
#' @name plotDelineations
#' @rdname delineation
#' @export
setGenericVerif("plotDelineations", function(x,sel=NULL,col=NULL,...) 
                  standardGeneric("plotDelineations"))
#' @name identifyDelineation
#' @rdname delineation
#' @export
setGenericVerif("identifyDelineation", function(x,sel=NULL,...) 
                  standardGeneric("identifyDelineation"))

#' Structure tensor field of GPR data 
#'
#' @name strTensor
#' @rdname strTensor
#' @export
setGenericVerif("strTensor", function(x,  blksze = c(2, 4),
                        kBlur   = list(n = 1, m = 1, sd = 1), 
                        kEdge   = list(n = 5, m = 5, sd = 1), 
                        kTensor = list(n = 5, m = 5, sd = 1),
                        thresh = 0.02, what = c("tensor", "mask"), ...)
                        standardGeneric("strTensor"))
                  
                  
                  
                  
                  
# http://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
extrema <- function(x, type=c("max","min")){
  type <- match.arg(type, c("max", "min"))
  if( type == "min" ){
    x <- -x
  }
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  return(y)
}

#--- see trTime in "ClassGPR.R"
# #' @export                  
# trRecTime <- function(x, origin = "1970-01-01"){
#   return(as.POSIXct(x@time, origin = origin))
# }

#' time to depth conversion
#'
#' time to depth conversion
#' @export                  
timeToDepth <- function(tt, time_0, v=0.1, antsep=1, c0 = 0.299){
  t0 <- time_0 - antsep/c0
  sqrt(v^2*(tt-t0)- antsep^2)/2
}

#' Depth to time conversion
#' 
#' Depth to time conversion
#' @export
depthToTime <- function(z, time_0, v=0.1, antsep=1, c0 = 0.299){
  t0 <- time_0 - antsep/c0
  sqrt((4*z^2 + antsep^2)/(v^2)) + t0
}

#' Depth zero 
#' 
#' Depth zero 
#' @export
depth0 <- function(time_0, v=0.1, antsep=1, c0 = 0.299){
  time_0 - antsep/c0 + antsep/v
}

#' Convert first wave break to time-zero
#'
#' Account for the delay time between time of wave emission and time of first
#' wave break recording due to the antenna separation (offset).
#' @rdname timeCorOffset
#' @export
firstBreakToTime0 <- function(fb, x, c0 = 0.299){
  if(length(x@antsep) == 0 || (!is.numeric(x@antsep))){
  stop("You must first define the antenna separation",
        "with `antsep(x)<-...`!")
  }
  fb - x@antsep/c0
}

.plot3DRGL <- function(A,x,y,z,z0,col=palGPR(n=101),back="fill", 
                  smooth = TRUE, lit=FALSE, lwd=0,empty=FALSE,...){
  nr = nrow(A)
  nc = ncol(A)
  if(empty==TRUE){
    X <- matrix(x, ncol=nc, nrow=2, byrow=TRUE)
    Y <- matrix(y, ncol=nc, nrow=2, byrow=TRUE)
    Z <-  matrix(z0, ncol=nc, nrow=2, byrow=TRUE) - 
          matrix(z[c(1,nr)], ncol=nc, nrow=2, byrow=FALSE)
    colA <- col[1]
    if(!is.null(list(...)$alpha) && (list(...)$alpha==0 || is.null(col))){

    }else{
      rgl::rgl.surface(Y, X, Z, color=colA, back=back, smooth = smooth, 
                  lit=lit, lwd= lwd,...) 
    }
    lines3d(y,z0,x, col="black",alpha=1,lwd=lwd)   
    lines3d(y,(z0-z[length(z)]),x, col="black",alpha=1,lwd=lwd)   
    lines3d(rep(y[1],2),(z0[1]-z),rep(x[1],2), col="black",alpha=1,lwd=lwd)
    lines3d(rep(y[length(y)],2),(z0[length(z0)]-z),rep(x[length(x)],2),
            col="black",alpha=1,lwd=lwd)   

  }else{
    X <- matrix(x, ncol=nc, nrow=nr, byrow=TRUE)
    Y <- matrix(y, ncol=nc, nrow=nr, byrow=TRUE)
    Z <-  matrix(z0, ncol=nc, nrow=nr, byrow=TRUE) - 
          matrix(z, ncol=nc, nrow=nr, byrow=FALSE)
    A = (A-min(A))/(max(A)-min(A))
    colA <- col[ (A)*100+1 ] # assign colors to heights for each point 
    rgl::rgl.surface(Y, X, Z, color=colA, back=back, smooth = smooth, 
                lit=lit, lwd= lwd,...) 
  }
}

.plot3DSlice <- function(XYZ,slice=c("x","y","z"),section=1,col=palGPR(n=101), 
                          sampling = c(0.25,0.25,0.04),rmStripes = TRUE){
  # k=100
  # j=25
  # i=40
  # col <- tim.colors(101) # height color lookup table
  slice = match.arg(slice)
  if(length(slice)>1){
    slice = slice[1]
  }
  
  dimXYZ = dim(XYZ)
  vz = seq(0,dimXYZ[3]-1,by=1)*sampling[3]  # dtime / 2 * v
  vx = seq(0,dimXYZ[1]-1,by=1)*sampling[1]
  vy = seq(0,dimXYZ[2]-1,by=1)*sampling[2]
  if(rgl::rgl.cur()==0){  # si la fenêtre rgl est ouverte, on plot dedans...
    rgl::rgl.open()
    rgl::rgl.bg( color=c("white"))
  }
  i = section
  j=i
  k=i
  if(slice=="x"){
    if(rmStripes == TRUE){ 
      Xside = normalizeGPR(removeStripes(t(XYZ[,j,])))
    }else{  
      Xside = normalizeGPR((t(XYZ[,j,])))  
    }
    
    Xside_x = matrix(vx,nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=TRUE)
    Xside_y = matrix( vy[j],nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=TRUE)
    Xside_z = matrix( max(vz)-vz,nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=FALSE)

    CCX = (Xside-min(Xside))/(max(Xside)-min(Xside))
    ClimX <- range(CCX)
    ClenX <- ClimX[2] - ClimX[1] + 1
    # col <- tim.colors(101) # height color lookup table
    #col = palette(gray(0:101 / 101))
    colCX <- col[ (CCX)*100+1 ] 
    
    surface3d(Xside_x, Xside_z, Xside_y, col= setCol(Xside), lit=FALSE,
            front="fill",back="fill")#, alpha=0.5)
  }else if(slice=="z"){
    if(rmStripes == TRUE){ Zside = (removeStripes(t(XYZ[,,k])))
    }else{  Zside = ((t(XYZ[,,k])))  }
    
    Zside_x = matrix(vx,nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=TRUE)
    Zside_y = matrix( vy,nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=FALSE)
    Zside_z = matrix(max(vz) - vz[k],nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=FALSE)

    CCZ = (Zside-min(Zside))/(max(Zside)-min(Zside))
    ClimZ <- range(CCZ)
    ClenZ <- ClimZ[2] - ClimZ[1] + 1
    #col = palette(gray(0:101 / 101))
    colCZ <- col[ (CCZ)*100+1 ]
    
    surface3d(Zside_x, Zside_z, Zside_y, col= setCol(Zside), lit=FALSE,
              front="fill",back="fill")#, alpha=0.5)
  }else if(slice=="y"){
    if(rmStripes == TRUE){ Yside = normalizeGPR(removeStripes(t(XYZ[i,,])))
    }else{  Yside = normalizeGPR((t(XYZ[i,,])))  }
    
    Yside_x = matrix(vx[i],nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=TRUE)
    Yside_y = matrix( vy,nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=TRUE)
    Yside_z = matrix( max(vz)-vz,nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=FALSE)
    
#     CCY = (Yside-min(Yside))/(max(Yside)-min(Yside))
#     ClimY <- range(CCY)
#     ClenY <- ClimY[2] - ClimY[1] + 1
#     colCY <- col[ (CCY)*100+1 ] 
    colCY <- colFromPal(Yside , col = col )

    surface3d(Yside_x, Yside_z, Yside_y, col= setCol(Yside), lit=FALSE,
              front="fill",back="fill")#, alpha=0.5)
  }
}

# shift the topography of the GPR profile (to display its topography)
.topoShift <- function(x, topo, dz){
  zShift <- (max(topo) - topo)
  # in fact >> old_t = x@depth
  old_t <- seq(0, length.out = nrow(x), by = dz)
  xShifted <- matrix(0, nrow = nrow(x) + floor(max(zShift)/dz), ncol = ncol(x))
  n <- 1:(nrow(x)-2)
  for(i in 1:ncol(x)){
    # compute new t-vector for each trace
    new_t <- old_t + zShift[i]
    xit <- seq(ceiling(new_t[1]/dz), ceiling(new_t[nrow(x)-2]/dz))
    # interpolate
    # FIX ME! does not work well (not nice interpolation)
    xShifted[xit + 1, i] = signal::interp1(new_t, x[,i], xi = xit * dz, 
                                           method = "spline",extrap = TRUE)  
  }
  return(xShifted)
}

.traceShift <- function(A, ts=0, tt=NULL, dz=0.4, method = "linear"){
  ps <- ts/dz
  Anew <- matrix(NA, nrow=nrow(A), ncol=ncol(A))
  v0 <- 1:nrow(A)
  for(i in seq_len(ncol(A))){
    relts <- floor(ps[i])*dz - ts[i]
    if(method == "none"){
      ynew <- A[,i]
    }else{
      ynew <- signal::interp1(tt, A[,i], tt + relts, 
                              method = method, extrap = NA)
    }
    vs <- v0 + floor(ps[i])
    test <- vs > 0 & vs <= nrow(A)
    vs <- vs[test]
    Anew[vs,i] <- ynew[test]
  }
  return(Anew)
}

# x = data matrix (col = traces)
# topoGPR = z-coordinate of each trace
# dx = spatial sampling (trace spacing)
# dts = temporal sampling
# v = GPR wave velocity (ns)
# max_depth = to which depth should the migration be performed
# dz = vertical resolution of the migrated data
# fdo = dominant frequency of the GPR signal
.kirMig <- function(x, topoGPR, dx, dts, v, max_depth = 8, 
                 dz = 0.025, fdo = 80, FUN = sum){
  n <- nrow(x)
  m <- ncol(x)
  z <- max(topoGPR) - topoGPR
  fdo <- fdo*10^6   # from MHz to Hz
  lambda <- fdo / v * 10^-9
  v2 <- v^2
  kirTopoGPR <- matrix(0, nrow = max_depth/dz + 1, ncol = m)
  for( i in seq_len(m)){
    x_d <- (i-1)*dx   # diffraction
    z_d <- seq(z[i],max_depth,by=dz)
#     mt <- (i - migTpl):(i + migTpl) # migration template
#     mt <- mt[mt > 0 & mt <= m]
#     l_migtpl <- length(mt)
    
    for(k in seq_along(z_d)){
      t_0 <- 2*(z_d[k] - z[i])/v    # = k * dts in reality
      z_idx <- round(z_d[k] /dz + 1)
      # Fresnel zone
      # Pérez-Gracia et al. (2008) Horizontal resolution in a non-destructive
      # shallow GPR survey: An experimental evaluation. NDT & E International,
      # 41(8): 611–620.
      # doi:10.1016/j.ndteint.2008.06.002
      rf <- 0.5 * sqrt(lambda * 2 * (z_d[k] - z[i]))
      rf_tr <- round(rf/dx)
      mt <- (i - rf_tr):(i + rf_tr)
      mt <- mt[mt > 0 & mt <= m]
      
      lmt <- length(mt)
      Ampl <- numeric(lmt)
      for(j in mt){
        x_a <- (j-1)*dx
        t_top <-  t_0 - 2*(z[j] - z[i])/v
        t_x <- sqrt( t_top^2 +   4*(x_a - x_d)^2 /v2)
        t1 <- floor(t_x/dts) + 1 # the largest integers not greater
        t2 <- ceiling(t_x/dts) + 1 # smallest integers not less
        if(t2 <= n && t1 > 0 && t_x != 0){
          w <- ifelse(t1 != t2, abs((t1 - t_x)/(t1 - t2)), 0)
          # Dujardin & Bano amplitude factor weight: cos(alpha) = t_top/t_x
          # Ampl[j- mt[1] + 1] <- (t_top/t_x) * 
          # ((1-w)*A[t1,j] + w*A[t2,j])
          # http://sepwww.stanford.edu/public/docs/sep87/SEP087.Bevc.pdf
          Ampl[j- mt[1] + 1] <- (dx/sqrt(2*pi*t_x*v))*
                                (t_top/t_x) * 
                                ((1-w)*x[t1,j] + w*x[t2,j])
        }
      }
      kirTopoGPR[z_idx,i] <- FUN(Ampl)
    }
  }
  kirTopoGPR <- kirTopoGPR/max(kirTopoGPR, na.rm=TRUE) * 50
#   kirTopoGPR2 <- kirTopoGPR
  
  return(kirTopoGPR)
}

plotWig <- function(z, x = NULL, y = NULL, main ="", note=NULL,
          time_0 = 0, antsep = 1, v = 0.1, surveymode = NULL,
          addFid = TRUE, fid = NULL,
          addAnn = TRUE, annotations = NULL, 
          depthunit="ns", posunit="m",
          topo=NULL,
          pdfName=NULL, 
          yaxt = "s", bty = "o",
          col = "black", lwd = 0.5, ws = 1, side = 1, ratio = 1, 
          dx = 0.25, dz = 0.4, xlim = NULL, ylim = NULL, relTime0 = TRUE, ...){
  
  op <- par(no.readonly = TRUE) 
  dx <- mean(diff(x)) # estimated x-step
  z[is.na(z)] = 0
  z <- z/max(abs(z)) * dx
  # A =  as.matrix(A)/max(abs(A))
  # v <- ifelse(is.null(v),1,v/2)
  nr <- nrow(z)
  nc <- ncol(z)
  z <- z[nr:1,]
  time_0 <- mean(time_0)
  if(is.null(y)){
    y <- -(ncol(GPR):1)
  }
  y0 <- 0
  if(is.null(topo)){
    topo <- rep(0L,nc)
  }else{
    # conversion ns to m!
    if(grepl("[s]$", depthunit)){
      # timeToDepth <- function(tt, time0, v=0.1, antsep=1){
        # t0 <- time0 - antsep/0.299
        # sqrt(v^2*(tt-t0)- antsep^2)/2
      # }
      y <-  y * v/ 2
      depthunit <- "m"
    }
    topo <- topo - max(topo)
  }
  if(grepl("[s]$", depthunit) && relTime0){
    y <- y + time_0
    if(ylim[2] > -time_0){
      ylim[1] <- max(c(min(y), ylim[1]))
      ylim[2] <- 0
    }
  }else if(grepl("[m]$", depthunit)){
    depth_0 <- depthToTime(z=0, time_0 , v=v, antsep=antsep) * v/ 2
    y <- y + depth_0
  }
  if(is.null(xlim) ){
       # xlim <- range(x) 
       xlim <- range(x)  + c(-1,1)*dx
     test <- rep(TRUE,length(x))
  }else{
    test <- ( x >= xlim[1] & x <= xlim[2] )
    xlim <- xlim + c(-1,1)*dx
  }

  
  omi=c(0,0,0.6,0)
  mgp=c(2.5, 0.75, 0)
  fac <- 0.2
  # if the depthunit are "meters"
  if(grepl("[m]$",depthunit)){
    mai=c(1,0.8,0.6,0.8)+0.02
    heightPDF <- fac*diff(ylim) + sum(omi[c(1,3)] + mai[c(1,3)])
    widthPDF <- fac*diff(xlim)*ratio +  sum(omi[c(2,4)]+ mai[c(2,4)])
  }else{
    mai=c(1,0.8,0.6,1.0)+0.02 
    heightPDF <- fac*(ylim[2] - ylim[1])*v/ 2 + sum(omi[c(1,3)] + mai[c(1,3)])
    widthPDF <- fac*(xlim[2] - xlim[1])*ratio + sum(omi[c(2,4)] + mai[c(2,4)])
  }
  if(!is.null(pdfName)){
    Cairo::CairoPDF(file = paste0(pdfName, ".pdf"),
        # pointsize=10,
        width = widthPDF, 
        height = heightPDF,
        # dpi=75,  # 75
        bg = "white",
        pointsize=10,
        # units = "in",
        title = pdfName)  
  }
  
  par(mai=mai,omi=omi,mgp=mgp)
      
  plot(0,0, type="n", xaxs="i", yaxs="i", yaxt="n",
      xlim=xlim, ylim=ylim, bty = "n", ...)
  usr <- par("usr")
  if(side==1){
    for(i in rev(seq_along(x))){
      y2 <- y + topo[i]
      wig = cbind(ws*z[,i]+x[i],y2)
      wig1 = rbind(c(x[i],y2[1]),wig,c(x[i],y2[nr]))
      polygon(wig1, col = col, border=NA)
      rect(min(wig1[,1]), ylim[1], x[i], ylim[2],col="white",border=NA)
      # lines(x[i]+ws*z[,i],y2,lwd=lwd)
    }
  }else{
    for(i in (seq_along(x))){
      y2 <- y + topo[i]
      wig = cbind(ws*z[,i]+x[i],y2)
      wig1 = rbind(c(x[i],y2[1]),wig,c(x[i],y2[nr]))
      polygon(wig1, col = col, border=NA)
      rect(max(wig1[,1]), ylim[1], x[i], ylim[2],col="white",border=NA)
    }
  }
  for(i in (seq_along(x))){
    y2 <- y + topo[i]
    lines(x[i]+ws*z[,i],y2,lwd=lwd)  
  }

  # plot fiducial markers
  if(!is.null(fid) && length(fid) > 0){
    .plotFid(fid[test], x[test])
  }
  
  # plot annotations
  testAnn <- FALSE
  if(addAnn && !is.null(annotations) && length(annotations) > 0){
    testAnn <- .plotAnn(annotations[test],x[test])
  }
  
  # plot title
  if(addAnn && testAnn){
    title(main,outer=TRUE,line=1)
  }else{
    title(main)  
  }
  
  # plot axis
#   axis(side=2, at=pretty_y + dusr/2, labels= -pretty_y)
#   dusr <- dylim/length(y)
  if( yaxt != "n"){
    pretty_y <- pretty(y ,10)
    axis(side=2, at=pretty_y, labels= -pretty_y)
    if( grepl("CMP", toupper(surveymode))){
      axis(side = 4, at = pretty_y, labels = -pretty_y)
    }else{
      #.depthAxis(ylim, pretty_y, time_0, v, antsep, depthunit, posunit )
      .depthAxis(y, pretty_y, time_0, v, antsep, depthunit, posunit )
    }
  }
  
  if( bty != "n"){
    box(bty = bty)
  }
  if(!is.null(note) && length(note) > 0){
     mtext(note, side = 1, line = 4, cex=0.6)
  }
  
  if(!is.null(pdfName)){
    dev.off()
  }else{
    par(op)
    par("usr" = usr)
  }
}


# @relTime0 > boolean, y scale relative to time0? 0 <-> time0
# col, main, xlab, ylab, mar, barscale
#' Plot GPR as image (raster)
#' 
#' Plot GPR as image (raster)
#' @export
plotRaster <- function(z, x = NULL, y = NULL, main = "", xlim = NULL,
                      note = NULL, ratio = 1,
             time_0 = 0, antsep = 1, v = 0.1,surveymode = NULL,
             addFid = TRUE, fid=NULL, ylim = NULL,
             addAnn = TRUE, annotations = NULL, 
             depthunit = "ns", posunit = "m",
             rasterImage = FALSE, resfac = 1, clab = "mV",
             add = FALSE, barscale = TRUE, addGrid = FALSE, 
             col = palGPR(n = 101), yaxt = "s", bty = "o",
             relTime0 = TRUE, clim = NULL, pdfName = NULL,...){
  op <- par(no.readonly=TRUE)
  time_0 <- median(time_0)
  mai <- op$mai
  if(barscale == FALSE){
    mai <- c(1.2, 1.2, 1.2, 1.2)
  }else{
    mai <- c(1.2, 1.2, 1.2, 1.8)
  }
  z <-  as.matrix(z)
  z <- t(z[nrow(z):1,])
  if(is.null(x)){
    x <- (1:nrow(z))
  }  
  if(is.null(y)){
    y <- -(ncol(z):1)
  }
  if(is.null(clim)){
    clim <- range(z[is.finite(z)], na.rm = TRUE)
    if( min(z) > 0 ){
      # to plot amplitudes for example...
      clim <- c(0, max(z, na.rm = TRUE))
      # clim <- range(z, na.rm = TRUE)
    } else if(!is.null(surveymode) && 
                tolower(surveymode) %in% c("cmp", "reflection")){
      clim <- c(-1, 1) * max(abs(z), na.rm = TRUE)
    }
  }
  # Note that y and ylim are negative and time_0 is positive...
  if(relTime0 && ylim[2] > -time_0){
    # truncate the data -> start at time-zero!!
    y <- y + time_0
    ylim[1] <- max(c(min(y), ylim[1]))
    ylim[2] <- 0
  }
  if( length(unique(diff(x))) > 1){
    rasterImage <- FALSE
  }
  omi <- c(0,0,0.6,0)
  mgp <- c(2.5, 0.75, 0)
  fac <- 0.2
  # if the depthunit are "meters"
  if(grepl("[m]$",depthunit)){
    heightPDF <- fac*diff(ylim) + sum(omi[c(1,3)] + mai[c(1,3)])
    widthPDF <- fac*diff(xlim)*ratio +  sum(omi[c(2,4)]+ mai[c(2,4)])
  }else{
    heightPDF <- fac*(ylim[2] - ylim[1])*v/ 2 + sum(omi[c(1,3)] + mai[c(1,3)])
    widthPDF <- fac*(xlim[2] - xlim[1])*ratio + sum(omi[c(2,4)] + mai[c(2,4)])
  }
  if(!is.null(pdfName)){
    Cairo::CairoPDF(file = paste0(pdfName, ".pdf"),
        # pointsize=10,
        width = widthPDF, 
        height = heightPDF,
        # dpi=75,  # 75
        bg = "white",
        pointsize=10,
        # units = "in",
        title = pdfName)  
  }
  if(add == TRUE){ 
    par(new = TRUE)
  }else{
    par( mai = mai,omi=omi,mgp=mgp)
  }
  if(isTRUE(rasterImage)){
    dy <- diff(y)
    tol <- sqrt(.Machine$double.eps)
    # all not equal
    if(abs(max(dy) - min(dy)) > tol){
      rasterImage <- FALSE
    }
  }
  #image(x,y,z,col=col,zlim=clim,xaxs="i", yaxs="i", yaxt="n",...)
  plot3D::image2D(x = x, y = y, z = z, col = col, 
        xlim = xlim, ylim = ylim, zlim = clim,
        xaxs = "i", yaxs = "i", yaxt = "n", rasterImage = rasterImage, 
        resfac = resfac, main = "", bty = "n", colkey = FALSE, ...)
 
  usr <- par("usr")
  if(is.null(xlim) ){
     test <- rep(TRUE,length(x))
  }else{
    test <- ( x >= xlim[1] & x <= xlim[2] )
  }
  # plot fiducial markers
  if(!is.null(fid) && length(fid) > 0){
    .plotFid(fid[test], x[test])
  }
  # plot annotations
  testAnn <- FALSE
  if(addAnn && !is.null(annotations) && length(annotations) > 0){
    testAnn <- .plotAnn(annotations[test],x[test])
  }
  # plot title
  if(addAnn && testAnn){
    title(main,outer=TRUE,line=1)
  }else{
    title(main)  
  }
  # plot axis
  pretty_y <- pretty(ylim, 10)
  axis(side = 2, at = pretty_y, labels = -pretty_y)
  if( grepl("CMP", toupper(surveymode))){
    axis(side = 4, at = pretty_y, labels = -pretty_y)
  }else{
    .depthAxis(ylim, pretty_y, time_0, v, antsep, depthunit, posunit )
  }
  # plot time0
  abline(h=0,col="red",lwd=0.5)
  # plot note
  if(!is.null(note) && length(note) > 0){
    mtext(note, side = 1, line = 4, cex=0.6)
  }
  # add grid
  if(addGrid){
    grid()
  }
  if( bty != "n"){
    box(bty = bty)
  }
  if(barscale){
    op2 <- par(no.readonly=TRUE)
    .barScale(clim = clim, y = y, col = col, clab = clab, 
              clabcex = 0.8)
    par(op2)
  }
  if(!is.null(pdfName)){
    dev.off()
  }
}
#---

.plotAnn <- function(ann, x, line=1.7){
  if(length(ann)>0){
    testann <- (ann != "")
    if(sum(testann)>0){
      posann <- x
      ann <- gsub("#","\n",ann)
      abline(v=posann[testann],col="red",lwd=1)
      mtext(ann[testann], side = 3, line = line, at=posann[testann], 
            col="red", cex=0.9)
    }
    return(TRUE)
  }else{
    return(FALSE)
  }
}

.plotFid <- function(fid, x){
  usr <- par()$usr
  pin <- par()$pin  # inch
  if(!is.null(fid) && length(fid)>0 && any(fid!="")){
    cin <- par()$cin[2]
    posfid <- x
    testfid <- (fid != "")
    yr <- diff(usr[3:4])/(pin[2])
    if(sum(testfid)>0){  
      par(xpd=TRUE)
      cst <- yr*cin
      points(posfid[testfid],cst/2*0.75+rep(usr[4],sum(testfid)),pch=25,
            col="red",bg="yellow",cex=1)
      text(posfid[testfid],cst+rep(usr[4],sum(testfid)),fid[testfid],cex=0.6)
          #,pos=3,offset =0)
      par(xpd=FALSE)
    }
  }
}


.barScale <- function(clim, y, col, clab = "mV", clabcex = 0.8){
  usr <- par()$usr
  pin <- par()$pin  # inch
  mai <- par()$mai
  dxin <- diff(usr[1:2])/(pin[1])
  dylim <- diff(usr[3:4])
  fin <- par()$fin
  mai2 <- c(par("mai")[1], par("mai")[1] + pin[1] + 1, par("mai")[3], 0.6)
  par(mai=mai2)
  fin2 <- par()$fin
  wstrip <- dxin*(fin2[1] - mai2[2] - mai2[4])/2
  xpos <- usr[1] + dxin*(mai2[2] - mai[2])
  zstrip <- matrix(seq(clim[1], clim[2], length.out = length(col)), nrow = 1)
  xstrip <- c( xpos - 20*wstrip,  xpos + 20*wstrip)#*c(0.9, 1.1)
  ystrip <- seq(min(y),max(y),length.out=length(col))
  ystrip <- seq(usr[3],usr[4],length.out=length(col))
  pretty_z <- pretty(as.vector(zstrip))
  dclim <- clim[2]-clim[1] 
  pretty_at <- usr[3] - dylim * (clim[1] - pretty_z)/dclim
  axis(side=4,las=2, at=pretty_at, labels=pretty_z)
 #  print(par("usr"))
  # print(range(xstrip))
  image(x = xstrip, y = ystrip, z = zstrip,
        add = TRUE, col = col, 
        axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  # axis(side=4, las=2)
  title(main=clab, line =1, cex.main = clabcex)
  box()
}

# we use the Sensors & Software method to plot the depth axis
# when the data are in time domain: because of the offset between
# transmitter and receiver, there is an offset between time zero and depth,
# the depth axes is squished.
.depthAxis <- function(y, pretty_y, time_0, v, antsep, depthunit, posunit ){
  if(grepl("[s]$",depthunit)){
    maxDepth <- max( abs(y + 2*time_0) ) * v
    depthAll <- pretty(c(0, maxDepth), 10)
    depthAllPos <- depthToTime(depthAll, 0, v, antsep)
    axis(side = 4, at = -depthAllPos, labels = depthAll, tck = -0.01)
    mtext(paste0("depth (", posunit, "),   v = ",v, " ", posunit, "/", 
                  depthunit), side = 4, line = 3)
                  
#     depth2 <- seq(0.1, by = 0.1, 0.9)
#     depthat2 <- depthToTime(depth2, 0, v, antsep)
#     
#     maxDepth <- max( abs(y + 2*time_0) ) * v
#     if(maxDepth > 1.1){
#       # depth <- pretty(seq(1.1, by = 0.1 , max( abs(y + 2*time_0) ) * v), 10)
#       depth <- pretty(c(1.1, max( abs(y + 2*time_0) ) * v), 10)
#       depthat <- depthToTime(depth, 0, v, antsep)
#       axis(side = 4, at = -depthat, labels = depth, tck = -0.02)
#       labelsTop <- FALSE
#     }else{
#       labelsTop <- depth2
#     }
#     axis(side = 4, at = -depthat2, labels = labelsTop, tck = -0.01)
#     axis(side = 4, at = -1*depthToTime(1, 0, v, antsep), 
#          labels = "1", tck = -0.02)
#     mtext(paste0("depth (", posunit, "),   v = ",v, " ", posunit, "/", 
#                   depthunit), side = 4, line = 3)
  }else{
    axis(side = 4, at = pretty_y, labels = -pretty_y)
    mtext(paste0("depth (", depthunit, ")") ,side = 4, line = 3)
  }
}

# Modified Energy ratio method
.firstBreakMER <- function(x, w){
  E <- wapplyMat2(x, width = w, by = 1, FUN = function(x) sum(x^2), 
                  MARGIN = 2)
  v1 <- 1:(nrow(x) - 2*(w-1))
  v2 <- v1 + (w-1)
  E1 <- E[v1,]
  E2 <- E[v2,]
  ER <- E2/E1
  MER <- (ER * abs( x[v1 + w-1,]) )^3
  fb <- apply(MER, 2, function(x) which.max(x)) + (w-1)
  return(fb)
}

# Threshold method for first breack picking
.firstBreakThres <- function(x, thr = 0.12, tt){
#   first_breacks <- rep(NA, ncol(x))
#   thres <- thr * max(x)
#   for(j in seq_len(ncol(x))){
    if( max(x) > thr){
      fb <- which(x > thr)
      if(length(fb) > 0){
        i <- fb[1]
        w <- (x[i] - thr) / (x[i] - x[i-1])
        return( w * tt[i-1] + (1- w) * tt[i] )
      } else{
        return(NA)
      }
    } else{
      return(NA)
    }
#   }
  
#   return(first_breacks)
}

# Jaun I. Sabbione and Danilo Velis (2010). Automatic first-breaks picking: 
# New strategies and algorithms. Geophysics, 75 (4): v67-v76
# -> modified Coppens's Method
# w = length leading window: about one period of the first-arrival waveform
# ns = length eps (edge preserving smoothing) window: good results with ns 
# between one and two signal periods
#        -> default values ns= 1.5*w
# bet = stabilisation constant, not critical, set to 0.2*max(amplitude) 
.firstBreakModCoppens2 <- function(x, w = 11, ns = NULL, bet = 0.2){
  if(is.null(ns)){
    ns <- 1.5 * w
  }
  E1all <- matrix(0, nrow=nrow(x), ncol= ncol(x))
  E1all[1:(nrow(E1all) - w +1),] <- wapplyMat2(x, width = w, by = 1, 
                                              FUN = sum, MARGIN=2)
  E2all <- apply(x, 2, cumsum)
  Erall <- E1all/(E2all + bet)

  xmeanall <- wapplyMat(Erall, width = ns, by = 1, FUN = mean, MARGIN=2)
  xsdall <- wapplyMat(Erall, width = ns, by = 1, FUN = sd, MARGIN=2)
  xtestall <- wapplyMat2(xsdall, width = ns, by = 1, FUN = which.min, MARGIN=2)
  xtestall <- xtestall + seq_len(nrow(xtestall))
  meantstall <- matrix(xmeanall[xtestall],nrow=nrow(xtestall), 
                    ncol=ncol(xmeanall), byrow=FALSE)
  meantstall2 <- matrix(0, nrow=nrow(x), ncol= ncol(x))
  meantstall2[seq_len(nrow(meantstall)) + (ns-1)/2,] <- meantstall
  fb <- apply(meantstall2, 2, function(x) which.max(abs(diff(x))))
  return(fb)
}

.firstBreakModCoppens <- function(x, w = 11, ns = NULL, bet = 0.2){
  if(is.null(ns)){
    ns <- 1.5 * w
  }
  E1 <- c(wapply(x, width = w, by = 1, FUN = sum), rep(0, 2*floor(w/2)))
  E2 <- cumsum(x)
  Er <- E1/(E2 + bet)
  Er_fil <- .eps(Er, ns = ns)
  fb <- which.max(abs(diff(Er_fil)))
  return(fb)
}

# edge preserving smoothing
# luo et al. (2002): Edge preserving smoothing and applications: 
# The Leading edge, 21: 136-158
.eps <- function(x, ns){
  xmean <-  c(rep(0, floor(ns/2)), 
              wapply(x, width = ns, by = 1, FUN = mean),
              rep(0, floor(ns/2)))
  xsd <- c(rep(0, floor(ns/2)), 
           wapply(x, width = ns, by = 1, FUN = sd),
           rep(0, floor(ns/2)))
  xtest <- wapply(xsd, width = ns, by = 1, FUN = which.min) + 
            (0):(length(xmean)- 2*floor(ns/2)-1)
  return(c(rep(0, floor(ns/2)), xmean[xtest], rep(0, floor(ns/2))))
}

#==============================#
#========== SPATIAL FILTER =========#
.medianFilter3x3 <- function(A){
  B <- A  # <- matrix(0, 364,364)
  for(i in 1:(nrow(A)-2)) {
       for(j in 1:(ncol(A)-2) ) {
          xm <- A[i+0:2, j+0:2]
             B[i+1, j+1] <- xm[order(xm)[5]]
    }
  }  
  return(B)
}

.medianFilter1D <- function(a,w){
  b <- a  # <- matrix(0, 364,364)
  for(i in (w+1):(length(a)-w-1)){
    xm <- a[i+(-w:w)]
    b[i] <- xm[order(xm)[w+1]]
  }
  return(b)
}

#==============================#
#======= GAIN FUNCTIONS ========#
# dts = sampling time (e.g., 0.8 ns)
# t0 = starting time to apply the gain scaling
# te = ending time to apply the gain scaling
# tcst
# CF Yilmaz, p85
.gainPower <- function(A, alpha, dts, t0 = NULL, te = NULL,
                       tcst = NULL){
  g <- .gainPower0(A[,1], alpha, dts, t0, te, tcst)
  Anew <- A * g
  #s1 = ((max(A))-(min(A)));  # scale factor
  #s2 = ((max(Anew))-(min(Anew)));  # scale factor
  #return(Anew/s2*s1 )
  return( Anew / sd(Anew) * sd(A))
}

.gainPower0 <- function(d, alpha, dts, t0 = NULL, te = NULL, tcst = NULL){
  if(is.null(t0)) t0 <- 0
  if(is.null(te)) te <- (length(d) - 1) * dts
  if(!is.null(tcst) && !(tcst > t0 && tcst < te)){
    stop("you need tcst > t0 && tcst < te\n")
  }
  ## FIX ME > instead of "(seq_along(d) - 1) *dts" use directly
  ## x <- gpr@depth !!!  
  x <- (seq_along(d) - 1) *dts
  test <- x >= t0 & x <= te
  g <- rep(1, length(d))
  g[test] <- 1 + (seq_along(d[test])*dts )^alpha
  g[x > te] <- max(g)
  if(!is.null(tcst) && any(x < tcst)){
    g[x < tcst] <- g[1 + floor(tcst/dts)]
  }
  return( g)
}

.gainExp <- function(A, alpha, dts, t0 = NULL, te = NULL){
  g <- .gainExp0(A[,1], alpha, dts, t0, te)
  Anew <- A * g
  return( Anew / sd(Anew) * sd(A))
}

.gainExp0 <- function(d, alpha, dts, t0 = NULL, te = NULL){
  if(is.null(t0) || t0==0) t0 <-0
  if(is.null(te)) te <-(length(d)-1)*dts
  ## FIX ME > instead of "(seq_along(d) - 1) *dts" use directly
  ## x <- gpr@depth !!! 
  x <- (seq_along(d) - 1) * dts
  test <- (x >= t0 & x <= te)
  test_max <- x > te
  g <- rep(1L,length(d))
  g[test] <-  exp((x[test] - t0) * dts * alpha)
  g[test_max] <-  exp(max(x[test] - t0)*dts*alpha)
  return( g)
}


.gainAgc <- function(A, dts, w = 10, p = 2, r = 0.5){
  w <- w/dts
  Anew <- apply(A, 2, .gainAgc0, w, p, r)
  #s1 = ((max(A))-(min(A)));  # scale factor
  #s2 = ((max(Anew))-(min(Anew)));  # scale factor
  #return(Anew * s1/s2)
  return(Anew)
}

.gainAgc0 <- function(d, w = 10, p = 2 , r = 0.5){
  # convert NA into 0
  d[is.na(d)] <- 0
  # Get local mean by smoothing the image with a Gaussian filter
  dAmp   <- mmand::gaussianSmooth(d, w)
  # Subtract image from local mean, raise to power 'p' then apply Gaussian
  # smoothing filter to obtain a local weighted sum. 
  # Finally raise the result
  # to power 'r' to obtain the 'gain'.  Typically p = 2 and r = 0.5 which will
  # make gain equal to the local RMS.  The abs() function is used to allow
  # for arbitrary 'p' and 'r'.
  dGain <- (mmand::gaussianSmooth(abs(d - dAmp)^p, w))^r
  # Apply inverse gain to the difference between the image and the local
  # mean to obtain the final AGC image.
  sel <- dGain > 0
  d[sel ] <- d[sel ]/dGain[sel ]
  return(d)
}


# histogram transformation to normal distributed data with same mean and sd
# https://msu.edu/~ashton/temp/nscore.R
.nScoreTrans <- function(x, inverse = FALSE, tbl = NULL){
  if(isTRUE(inverse)){
    if(is.null(tbl)){
      stop("tbl must be provided")
    }
    min_x <- min(tbl[,1])
    max_x <- max(tbl[,1])
    min_sc <- min(x)
    max_sc <- max(x)
    x1 <- c(min_x, tbl[,1], max_x)
    nsc <- c(min_sc, tbl[,2], max_sc)
    
    back.xf <- approxfun(nsc,x1) # Develop the back transform function
    val <- back.xf(x)
    return(val)
  }else{
    #     fx <- ecdf(x)
    #     x1 <- head(knots(fx), -1)
    #     xCDF <- fx(x1)
    #     y <- qnorm(xCDF, mean(x), sd = sd(x))
    if(!is.null(tbl)){
      x1 <- tbl[,1]
      y  <- tbl[,2]
    }else{
      x1 <- sort(x)
      y <- sort(qqnorm(x, plot.it = FALSE)$x)
      tbl <- data.frame(x = x1, nscore = y)
    }
    y_n <- approx(x1, y, xout = x, rule = 2)$y
    #return(list(x = y_n, table = tbl))
    return(y_n)
  }
}

#=============================================#
#======== CLIP/GAMMA/NORMALIZE ================#


.gammaCorrection <- function(A,a,b){
  return(a*sign(A)*abs(A)^b)
}

.clip <- function(A, Amax = NULL, Amin = NULL){
  if(!is.null(Amin)){
          A[(A)< Amin] <- Amin
  }
  if(!is.null(Amax)){
          A[(A) > Amax] <- Amax
  }
  if(!is.null(Amax) && is.null(Amin)){
    A[(A)< -Amax] <- -Amax
  }
  return(A)
}

#.rms <- function(num) sqrt(sum(num^2)/length(num))

scaleCol <- function(A, type = c("stat", "min-max", "95",
                                 "eq", "sum", "rms", "mad", "invNormal")){
  A <-  as.matrix(A)
  test <- suppressWarnings(as.numeric(type))
  if(!is.na(test) && test >0 && test < 100){
    A_q95 <- apply(A, 2, quantile, test/100, na.rm = TRUE)
    A_q05 <- apply(A, 2, quantile, 1 - test/100, na.rm = TRUE)
    Ascl <- scale(A, center = FALSE, scale = A_q95 - A_q05)
    # matrix(A_q95 - A_q05, nrow = nrow(A), ncol = ncol(A), byrow=TRUE)
    #A <- A/Ascl
  }else{
    type <- match.arg(type)
    if( type == "invNormal"){
      Ascl <- apply( A, 2, .nScoreTrans)
      return(Ascl)
    }
    else if(type == "stat"){
      # A <- scale(A, center=.colMeans(A, nrow(A), ncol(A)), 
      #            scale = apply(A, 2, sd, na.rm = TRUE))
      Ascl <- scale(A)
    }else if(type == "sum"){
      Ascl <- scale(A, center=FALSE, scale = colSums(abs(A)))
    }else if(type == "eq"){
      # equalize line such each trace has same value for 
      # sqrt(\int  (A(t))^2 dt)
      Aamp <- matrix(apply((A)^2,2,sum), nrow = nrow(A), 
                     ncol = ncol(A), byrow=TRUE)
      Ascl <- A*sqrt(Aamp)/sum(sqrt(Aamp))
    }else if(type == "rms"){
      Ascl <- scale(A, center = FALSE)
      # Ascl <- matrix(apply(A ,2, .rms), nrow = nrow(A), 
      #                ncol = ncol(A), byrow=TRUE)
    }else if(type == "min-max"){  # min-max
      Ascl <- scale(A, center = FALSE, 
                    scale = apply(A, 2, max, na.rm = TRUE) - 
                            apply(A, 2, min, na.rm = TRUE))
    }else if(type == "mad"){  # mad
      Ascl <- scale(A, center = apply(A, 2, median), 
                    scale = apply(A, 2, mad))
    }
  }
  test <- (!is.na(Ascl[1,] ) & abs(Ascl[1,]) > .Machine$double.eps^0.75) 
  A[,test] <- Ascl[,test]
  A[, !test] <- 0
  return(A)
}

rmsScaling <- function(...){
  stop("Deprecated! Use instead 'traceScaling(x,type=\"rms\")'\n")
}

#=============================================#
#======== SPECTRAL FUNCTIONS ================#


# @param [matrix]/[vector]   A     (each column represent a trace / a trace)
# @param [double]       dT     (sampling time in nanoseconde)  
# @param [double]      fac    (result multiplied by a factor, e.g. to get MHz 
# instead of Hz)

# @return power spectrum (frequency, power, phase)
# -------------------------------------------
powSpec <- function(A, dT = 0.8, fac = 1000000, plotSpec = TRUE, 
                   titleSpec = NULL, unwrapPhase = TRUE){
  A   <- as.matrix(A)
  nr  <- nrow(A)
  nc  <- ncol(A)
  N   <- 2^(ceiling(log2(nr)))
  A   <- rbind(A,matrix(0,nrow=N-nr,ncol=nc))

  # samping interval GPR = 0.8 ns
  Ts    <- dT*(10^(-9))     # [s] Sample time
  Fs    <- 1/Ts             # [Hz] Sampling frequency
  Fc    <- 1/(2*Ts)         # Nyquist frequency
  nfreq <- N/2 + 1
  
  # if y <- fft(z), then z is 
  # fft(y, inverse = TRUE) / length(y).
  # each column = discrete Fourier transform. 
  fft_A <- stats::mvfft(A)
  # extract the power spectrum (sometimes referred to as "magnitude")
  pow <- as.matrix(Mod(fft_A))
  pow <- pow[1:nfreq,,drop=FALSE]   # select only first half
  # extract the phase which is atan(Im(fourier)/Re(fourier))
  pha <- as.matrix(Arg(fft_A))
  pha <- pha[1:nfreq,,drop=FALSE]    # # select only first half

  pow_mean <- apply(pow, 1, mean, na.rm = TRUE)
  if(unwrapPhase){
    pha <- apply(pha, 2, signal::unwrap)
  }
  pha_mean <- apply(pha, 1, mean, na.rm = TRUE)

  # frequenceS
  fre = Fs*seq(0, N/2)/N/fac
  
  # plot the power spectrum
  if(plotSpec){
    op <- par(no.readonly=TRUE)
    m = seq(0, 10000, by = 50)
    par(mfrow=c(2,1))
    par(mar=c(0, 4, 4, 2) + 0.1, oma=c(1,1,1,1) )
    plot(fre,pow_mean, type="n",
#           xaxt = "n",
          ylim=c(0,max(pow)),
          ylab="amplitude",xlab="")
      if(!is.null(dim(A))){
        invisible( apply(pow, 2, lines, x = fre, 
                col=rgb(0.2,0.2,0.2,7/max(ncol(A),7))) )
      }
      lines(fre,pow_mean,col="red")
#       Axis(side = 1, tcl = +0.3,  labels=FALSE ,at=m)
      if(!is.null(titleSpec)){
        title(titleSpec)
      }
    par(mar=c(4, 4, 0.3, 2))
    plot(fre,pha_mean, type="n", 
#           xaxt = "n",
          ylim=range(pha), 
          xlab = "frequency MHz", ylab="phase") 
      if(!is.null(dim(A))){
        invisible(  apply(pha, 2, lines, x = fre, 
                  col = rgb(0.2,0.2,0.2,7/max(ncol(A), 7))) )
      }
      lines(fre,pha_mean,col="red")
#       Axis(side = 1, tcl = +0.3,  labels=m ,at=m)
    par(op)
  }
  return(list(freq = fre, pow = pow, pha = pha))
}

# @param [matrix]/[vector]   A     (each column represent a trace / a trace)
# @param [double]       dT     (sampling time in nanoseconde)  


# @return power spectrum (frequency, power, phase)
# -------------------------------------------

.fFilter1D <- function(A, f = c(100), type = c('low', 'high', 'bandpass'), 
                        L = 257, dT = 0.8, plotSpec = FALSE, fac = 1000000){
  type <- match.arg(type)
  A <- as.matrix(A)
  nr <- nrow(A)      # signal length

  # samping interval GPR = 0.8 ns
  Ts    <- dT*(10^(-9))     # [s] Sample time
  Fs    <- 1/Ts             # [Hz] Sampling frequency
  
  # cut-off frequency/ies fc in (MHz)
  f <- sort(f) * 10^6    # cut-off frequency in Hz
  
  # FIXME > write a function setFFilter(f, type=..., Fs)
  if(type == "low" || type == "high"){
    # Design the filter using the window method:
    if(length(f)>1) {
        BW <- (f[2] - f[1])/Fs          # filter bandwidth
        fc <- f[1] + (f[2] - f[1])/2    # cut frequency
        L <- round(4 / BW)
        if(L %% 2 == 0){
          L <- L + 1  
        }
    }else if(length(f)==1){
      fc <- f[1]
    }
    h <- winSincKernel(L, fc/Fs, type)
  }else if(type == "bandpass"){
    if(length(f)==2 ) {
      h1 <- winSincKernel(L, f[1]/Fs, "low")
      h2 <- winSincKernel(L, f[2]/Fs, "high")
    }else if(length(f) == 4 ){
      BW <- (f[2] - f[1])/Fs
      fc <- f[1] + (f[2] - f[1])/2
      L <- round(4 / BW)
      if(L %% 2 == 0){
        L <- L + 1
      }
      h1 <- winSincKernel(L, fc/Fs, "low")
      BW <- (f[4] - f[3])/Fs
      fc <- f[3] + (f[4] - f[3])/2
      L <- round(4 / BW)
      if(L %% 2 == 0){
        L <- L + 1
      }
      h2 <- winSincKernel(L, fc/Fs, "high")
    }
    L = max(length(h1),length(h2))
    if(length(h2) < L ){
      h2 = c(rep(0,(L-length(h2))/2),h2,rep(0,(L-length(h2))/2))
    }
    if(length(h1) < L ){
      h1 = c(rep(0,(L-length(h1))/2),h1,rep(0,(L-length(h1))/2))
    }
    # change the band-reject filter kernel into a band-pass 
    h = -h1 - h2
    h[(L+1)/2] = h[(L+1)/2] + 1
  }
  
  # L <- length(h)
  
  # Choose the next power of 2 greater than L+nr-1 
  Nfft = 2^(ceiling(log2(L + nr-1)))
  # Zero pad the signal and impulse response:
  h_long = c( h, rep(0, Nfft - L) )
  A = rbind(as.matrix(A) , matrix(0,nrow=Nfft-nr,ncol=ncol(A)) )

  fft_A = stats::mvfft(A)    # signal
  fft_h = stats::fft(h_long)        # filter

  # Now we perform cyclic convolution in the time domain using 
  # pointwise multiplication in the frequency domain:
  Y = fft_A * fft_h

  pow_A = Mod(fft_A)
  pow_h = Mod(fft_h)
  pow_y = Mod(Y)
  # si matrix -> moyenne sur les colonnes
  if(!is.null(dim(A))){
    pow_A = apply(pow_A,1, mean, na.rm=TRUE)
    pow_y = apply(pow_y,1, mean, na.rm=TRUE)
  }
  # select only first half of vectors
  pow_A = pow_A[1:(Nfft/2+1)] 
  pow_y = pow_y[1:(Nfft/2+1)] 
  pow_h = pow_h[1:(Nfft/2+1)] 

  fre = Fs*(0:(Nfft/2))/Nfft/fac  #[MHz]
  
  if(plotSpec == TRUE){
    op <- par(no.readonly=TRUE)
    # plot the power spectrum
    m = seq(0,900,by=50)
    #par(mfrow=c(2,1), mar=c(5, 4, 4, 6) + 0.1 )
    par( mar=c(0, 4, 0.3, 2) + 0.1, oma=c(3,2,1,2) )
    plot(fre,pow_A, type="l",
#         xaxt = "n",
        #  yaxt = "n", 
          ylim=c(0,max(pow_A,pow_y)),
          ylab="power",lwd=2)
      lines(fre,pow_y, type="l",col="blue",lwd=2)
#       Axis(side = 1, tcl = +0.3,  labels=m ,at=m)
      par(new=TRUE)
      plot(fre,pow_h,type="l", col="red",
        yaxt = "n",
        ylab="")
        legend("topright",c("input signal","filter","filtered signal"),
              col = c("black", "red", "blue"), lwd=c(2,1,2),bg = "white")
      abline(v=f/1000000, col="grey",lty=2)
     par(op)
  }
  a = (L-1)/2
  y = stats::mvfft(Y, inverse = TRUE)
  y = y[a:(a+nr-1),]/nrow(y)
  return(Re(y))
}

winSincKernel <- function(L,f,type=c("low","high")){
  type = match.arg(type)  # if L is even (because L - filter length - 
#                             must be odd)
  x = (-(L-1)/2):((L-1)/2)
  # low-pass
  h = hammingWindow(L) * sincMod(x,2*pi*f)  # h is our filter
  h = h/sum(h)

  # high-pass
  if(type == "high"){
    h = -h
    h[(L+1)/2] = h[(L+1)/2] + 1
  }
  return(h)
}

sincMod <- function(x,ff){
  r = length(x)
  n0 = which(x == 0)
  v = rep(0,r)
  ww <- c(1:(n0-1),(n0+1):r)
  #x = x[c(1:(n0-1),(n0+1):r)]
  v[ww] = sin(ff*x[ww])/(x[ww])
  v[n0] = ff
  return(v)
}
hammingWindow <- function(L){
  N = L-1
  n <- 0:N
  return(0.54 - 0.46*cos(2*pi*n/N))
}

# Choose the next power of 2 greater than L+M-1 
  # Nfft = 2^(ceiling(log2(L+M-1)))  # -1)))    # or 2^nextpow2(L+M-1)
nextpower2 <- function(x){
  return(2^(ceiling(log2(x))))
}

#' Phase rotation
#'       
#' shift the phase of signal by phi (in radian)
#' @export
phaseRotation <- function(x,phi){
  nf <- length(x)
  X <- stats::fft(x)
  phi2 <- numeric(nf)
  phi2[2:(nf/2)] <- phi
  phi2[(nf/2+1):(nf)] <- -phi
  Phase <- exp(-complex(imaginary=-1)*phi2)
  xcor <- stats::fft(X*Phase, inverse=TRUE)/nf
  return(Re(xcor))
}



# -------------------------------------------
# ------------addProfile3D--------------------------
# -------------------------------------------
# @name  addProfile3D (plot/add a 2D profile in rgl)
# @description read one or several DT1 file and plot/add 
# the 2D profiles in a rgl plot

# # @date 14.10.2013 15:15
# # @auteur Emanuel Huber
# require(rgl)
# # @require function load_install_package()
# source('load_install_package.R')
# cat('> Function(s) loaded: "load_install_package.R" ')
# # @require function normalizeGPR
# source('GPR_normalize.R')
# source('GPR_readDT1.R')
# source('GPR_gain.R')
# cat('"GPR_normalize.R"')
# cat('"GPR_readDT1.R" \n')
# cat('"GPR_gain.R" \n')

# @param [list]   LINE       (list containing several fPath of the DT1 file)
# @param [c(1)]   col=NULL   (palette of color)  
# @param [boolean]   plotNew=FALSE  (if true, open a new rgl window)
# @return void
# -------------------------------------------



# plot/add a 2D profile in rgl

# addProfile3D <- function(LINES, col=palGPR(n=101),plotNew=FALSE, normalize=TRU
# E, v=1, zlim=NULL, AGC=FALSE, sig=10){
  # if(plotNew){
    # # rgl.open()
    # open3d()
  # }
  # for(i in seq_along(LINES)){
    # #------------- READ DATA ------------------#
    # lineName2   <- strsplit(LINES,split="[.]")
    # lineName   <- lineName2[[i]][1]
    # fileNameHD   <- paste(lineName,".HD",sep="")
    # fileNameDT1 <- paste(lineName,".DT1",sep="")
    # cat(basename(lineName),"\n")
    # GPR <- readDT1(LINES[[i]])
    # #------------- read data ------------------#

    # myGPRdZ <- as.numeric(as.character(GPR$hd[7,2]))/as.numeric(as.character(
# GPR$hd[5,2]))
    # HD <- GPR$dt1hd
    
    # A <- GPR$data
    # A[is.na(A)] <- 0
    # if(!is.null(zlim)){
      # sel <- seq(1, zlim/myGPRdZ/v,by=myGPRdZ)
      # A <- A[sel,]
    # }
    # if(normalize){
      # A <- normalizeGPR(A)
    # }
    # if(AGC){
      # A <- apply(A,2,gain,sig=sig)
    # }
     # # example with GPR profile A 
    # nr = nrow(A)
    # nc = ncol(A)
    # X <- matrix(HD$recx, ncol=nc, nrow=nr, byrow=TRUE)
    # Y <- matrix(HD$recy, ncol=nc, nrow=nr, byrow=TRUE)
    # Z <-  matrix(HD$topo, ncol=nc, nrow=nr, byrow=TRUE) - 
# matrix(myGPRdZ*v*(0:(
# nr-1)), ncol=nc, nrow=nr, byrow=FALSE)
    # if(all(HD$topo==0)){
      # warning("No topography \n")
    # }
    # if(all(HD$recx==0)){
      # warning("No x-coordinates \n")
    # }
    # if(all(HD$recy==0)){
      # warning("No y-coordinates \n")
    # }
    # A = (A-min(A))/(max(A)-min(A))
    # Alim <- range(A)
    # Alen <- Alim[2] - Alim[1] + 1
    
    # # if(is.null(col))     col <- tim.colors(101)  # height color lookup table

    # colA <- col[ (A)*100+1 ] # assign colors to heights for each point 
    # rgl.surface(X, Y, Z, color=colA, back="fill", smooth = TRUE, lit=FALSE, 
# lwd=0) 
    # # surface3d(X, Y, Z, color=colA, back="fill", smooth = FALSE, lit=FALSE, 
# lwd=0) 
  # }
# }




#-------------------------------------
#------- PRIVAT FUNCTION --------#
byte2volt <- function ( V=c(-50,50), nBytes = 16) {
  abs(diff(V))/(2^nBytes)
}




.minCommon10 <- function(xmin,xmax){
  xmin <- as.numeric(xmin)
  xmax <- as.numeric(xmax)
  D <- xmax-xmin
  n <- nchar(D)
  if( as.numeric(substr(xmin,nchar(xmin)-n+1,nchar(xmin))) + D < 10^(n)){
    return(as.numeric(substr(xmin,1,n+1))*10^(nchar(xmin)-n-1))
  }else{
    return(xmin)
  }
}



 
.upsample <- function(A, n=c(2,1), type=c("DFT","bicubic")){
  # bi cubic---
  # library(fields)
  # interp2d <- function(old, newx, newy) {
    # interp.surface.grid(list(x=seq(nrow(old)),y=seq(ncol(old)),z=old),
              # list(x=seq(1,nrow(old),length=newx),
                 # y=seq(1,ncol(old),length=newy)))$z
  # }
  # A_bi <- interp2d(A,nrow(A)*2,ncol(A)*2)
  # or akima::bicubic.grid(x,y,z,xlim,ylim,dx,dy)
  if(is.matrix(A)){
    nr <- nrow(A)  # time  
    nc <- ncol(A)  # x  

    nk <- (nextpower2(nc))
    nf <- (nextpower2(nr))
    A1 <- matrix(0,nrow=nf,ncol=nk)
    A1[1:nr,1:nc] <- A
    A1_fft <- stats::fft(A1)
    
    A_fftint <- matrix(0,nrow=n[1]*nf,ncol=n[2]*nk)
    A_fftint[1:(nf/2),1:(nk/2)] <- A1_fft[1:(nf/2),1:(nk/2)]
    A_fftint[((n[1]-1)*nf + nf/2+1):(n[1]*nf),
             ((n[2]-1)*nk + nk/2 + 1):(n[2]*nk)] <- 
                  A1_fft[(nf/2+1):(nf),(nk/2 + 1):nk]
    A_fftint[1:(nf/2),
             ((n[2]-1)*nk + nk/2 + 1):(n[2]*nk)] <- 
                  A1_fft[1:(nf/2),(nk/2 + 1):nk]
    A_fftint[((n[1]-1)*nf + nf/2+1):(n[1]*nf),
              1:(nk/2)] <- 
          A1_fft[(nf/2+1):(nf),1:(nk/2)]
    
    A_int <- stats::fft(A_fftint, inverse = TRUE)
    A_int <- A_int[1:(n[1]*nr),1:(n[2]*nc)]/(nk*nf)
  }else if(is.vector(A)){
    # FTA = fft(A);
    n_A = length(A)
    # Choose the next power of 2 greater than L+M-1 
    Nfft = 2^(ceiling(log2(n_A)))    # 
    # Zero pad the signal and impulse response:
    A0 = c( A, rep(0,Nfft-n_A) )
    n_A0 <- length(A0)
    
    FTA <- stats::fft(A0)
    
    # % now insert enough zeros into the dft to match the desired density 'n'
    FTA = c(FTA[1:(n_A0/2)], rep.int(0,floor((n[1]-1)*n_A0)), 
                                    FTA[(n_A0/2+1):n_A0])

    A_int = stats::fft(FTA, inverse = TRUE)
    A_int <- A_int[1:(n_A * (n[1]))]/n_A0
  }
  return(Re(A_int))
}

#========================================================#
#================= LOCAL ORIENTATION ====================#
#========================================================#

# -------------------------------------------
# ------------localOrientation--------------------------
# -------------------------------------------
# @name  localOrientation (Compute the local orientation)
# @description Compute the local orientation based on the tensor of the
# gradient field and on SVD decomposition.

# @date 04.09.2012 12:45
# @auteur Emanuel Huber

# @require source("convolution2D.R") -> tim.colors + image.plot
  # source('convolution2D.R')
  # cat('> Function(s) loaded: "convolution2D.R" \n')

# @return void
# -------------------------------------------

# return inv(A) %*% B.
# assume that
#      | a1  c1 |
#  A = |        |
#      | c1  b1 |
#
#      | a2  c2 |
#  B = |        |
#      | c2  b2 |
invAxB <- function(a1,b1,c1,a2,b2,c2){
  D <- a1*b1 - c1*c1
  return(list( a11 = (a2*b1 - c1*c2)/D,
               a22 = (a1*b2 - c1*c2)/D,
               a12 = (b1*c2 - b2*c1)/D,
               a21 = (a1*c2 - a2*c1)/D))
}

# eigendecomposition tensor field.
# assume that
#      | a   d |
#  J = |       |
#      | d   b |
eigenDecomp2x2SymMatrix <- function(a,b,d){
  D <- sqrt((a-b)^2 + 4*d^2)
#   l1 <- 0.5*(a+b + D)
#   l2 <- 0.5*(a+b - D)
#   u1x <- 0.5 *(a-b + D)
#   u1y <- d
#   u2x <- 0.5 *(a-b - D)
#   u2y <- d
  return(list(l1 = 0.5*(a+b + D),
              l2 = 0.5*(a+b - D),
              u1x = 0.5 *(a-b + D),
              u1y = d,
              u2x = 0.5 *(a-b - D),
              u2y = d))
}

# return eigenvalues of A.
# assume that
#      | a11  a12 |
#  A = |          |
#      | a21  a22 |
eigenValue2x2Mat <- function(a11,a12,a21,a22){
  D <- a11*a22 - a21*a12
  tr <- a11 + a22
  tr24d <- tr^2 - 4*D
  tr24d[abs(tr24d ) < .Machine$double.eps^0.75] <- 0
  return(list(l1 = 0.5 * (tr + sqrt(tr24d)),
              l2 = 0.5 * (tr - sqrt(tr24d))))
  
}

# return A^n
# assume that
#      | a   d |
#  A = |       |
#      | d   b |
matPow <- function(a, b, d, n){
  eg <- eigenDecomp2x2SymMatrix(a, b, d)
  l1 <- eg$l1^n
  l2 <- eg$l2^n
  return(list(a11 = eg$u1x^2 * l1 + eg$u2x^2 * l2,
              a22 = eg$u1y^2 * l1 + eg$u2y^2 * l2,
              a12 = eg$u1x*eg$u1y * l1 + eg$u2x*eg$u2y * l2,
              a21 = eg$u1y*eg$u1x * l1 + eg$u2y*eg$u2x * l2))
}

matProd2x2 <- function(a11, a12, a21, a22, b11, b12, b21, b22){
  return(list(a11 = a11*b11 + a12*b21,
              a12 = a11*b12 + a12*b22,
              a21 = a21*b11 + a22*b21,
              a22 = a21*b12 + a22*b22))
}

#' Distance between structure tensors
#'
#' @name distTensors
#' @rdname distTensors
#' @export
distTensors <- function(J1, J2, method=c("geodesic", "log-Euclidean",
                        "angular", "L2"), normalise = FALSE){
  method <- match.arg(method, c("geodesic", "log-Euclidean", 
                                "angular", "L2"))
  if(normalise == TRUE){
    # J1[[1]] = J_xx
    # J1[[2]] = J_yy
    # J1[[3]] = J_xy
    J1 <- normTensor(J1[[1]], J1[[2]], J1[[3]])
    J2 <- normTensor(J2[[1]], J2[[2]], J2[[3]])
  }
  if(method == "geodesic"){
    return(distTensorGeod(J1[[1]], J1[[2]], J1[[3]], 
                          J2[[1]], J2[[2]], J2[[3]]))
  }else if(method == "log-Euclidean"){
    return(distTensorLogE(J1[[1]], J1[[2]], J1[[3]], 
                          J2[[1]], J2[[2]], J2[[3]]))
  }else if(method == "angular"){
    angle1 <- 1/2*atan2(2*J1[[3]], (J1[[1]] - J1[[2]]) ) + pi/2  
    angle2 <- 1/2*atan2(2*J2[[3]], (J2[[1]] - J2[[2]]) ) + pi/2
    return( (angle1 - angle2) )# %% pi )
  }else if(method == "L2"){
    return(  (J1[[1]] - J2[[1]])^2 + 
           2*(J1[[3]] - J2[[3]])^2 + 
             (J1[[2]] - J2[[2]])^2   )
  }
}
#
normTensor <- function(a1,b1,c1){
  val_1 <- eigenValue2x2Mat(a1, c1, c1, b1)
#   l1 <- (val_1$l1 + val_1$l2)
  l1 <- sqrt(val_1$l1^2 + val_1$l2^2)
  return(list(a1/l1, b1/l1,  c1/l1))
}

# geometric-based distance d g that measures the distance between two tensors
# J1 and J2 in the space of positive definite tensors 
# (based on Riemannian geometry)
# assume that
#       | a1   c1 |
#  J1 = |         |
#       | c1   b1 |
# 
#       | a2   c2 |
#  J2 = |         |
#       | c2   b2 |
distTensorGeod <- function(a1,b1,c1,a2,b2,c2){
  ABA <- invAxB(a1,b1,c1,a2,b2,c2)
#   A <- matPow(a1, b1, c1, -0.5)
#   AB <- matProd2x2(A$a11, A$a12, 
#                              A$a21, A$a22,
#                              a2,c2,c2,b2)
#   ABA <- matProd2x2(AB$a11, AB$a12, 
#                     AB$a21, AB$a22,
#                     A$a11, A$a12, 
#                     A$a21, A$a22)
  val <- eigenValue2x2Mat(ABA$a11, ABA$a12, ABA$a21, ABA$a22)
  val$l1[val$l1 < 10^-100] <- 10^-100
  val$l2[val$l2 < 10^-100] <- 10^-100
#   val$l1 <- 1
#   val$l2 <- val$l2/val$l1
  return(sqrt(log(val$l1)^2 + log(val$l2)^2))
}

# log-Euclidean-based distance between two tensors
# J1 and J2 in the space of positive definite tensors 
# (based on Riemannian geometry)
# assume that
#       | a1   c1 |
#  J1 = |         |
#       | c1   b1 |
# 
#       | a2   c2 |
#  J2 = |         |
#       | c2   b2 |
distTensorLogE <- function(a1,b1,c1,a2,b2,c2){
  a1[a1 < 10^-100] <- 10^-100
  a2[a2 < 10^-100] <- 10^-100
  b1[b1 < 10^-100] <- 10^-100
  b2[b2 < 10^-100] <- 10^-100
  c1[c1 < 10^-100] <- 10^-100
  c2[c2 < 10^-100] <- 10^-100
  a11 <- log(a1) - log(a2)
  a12 <- log(c1) - log(c2)
#   a21 <- log(c1) - log(c2)
  a22 <- log(b1) - log(b2)
  tr <- a11^2 + 2*a12^2 + a22^2
  return(sqrt( tr))

}

# return structure tensor
#------------------------------
# Structure tensor field
# 
# name strucTensor
# rdname strucTensor
# export
.strucTensor <- function(P, dxy = c(1, 1), mask = c(2, 2),
                        kBlur   = list(n = 3, m =  3, sd = 1), 
                        kEdge   = list(n = 7, m =  7, sd = 1), 
                        kTensor = list(n = 5, m = 10, sd = 2),
                        thresh=0.1, ...){
  n <- nrow(P)
  m <- ncol(P)
 
 #-------------------------------------------------
  #- Identify ridge-like regions and normalise image
  #-------------------------------------------------
  # normalization (mean = 0, sd = 1)

  # apply standard deviation block-wise
  if(!is.null(mask)){
    if(length(mask)==2 && is.null(dim(mask))){
      blksze2 <- round(mask/2)
      Pn <- (P - mean(P, na.rm = TRUE))/sd(as.vector(P), na.rm = TRUE)
      nseq <- c(seq(1, n, mask[1]),n)
  #     nseq <- seq(1, n/blksze[1])
      mseq <- c(seq(1, m, mask[2]),m)
  #     mseq <- seq(1, m/blksze[2])
      P_sd <- matrix(NA, nrow = n, ncol = m)
      for(i in seq_len(n)){
        for(j in seq_len(m)){
          nv <- (i - blksze2[1]):(i + blksze2[1])
          mv <- (j - blksze2[2]):(j + blksze2[2])
          nv <- nv[nv <= n & nv > 0]
          mv <- mv[mv <= m & mv > 0]
          std <- sd(array(Pn[nv,mv]),na.rm=TRUE)
          P_sd[nv,mv] <- std
        }
      }
      mask <- P_sd < thresh
    }else{
#       P <- (P - mean(P[!mask], na.rm = TRUE))/
#           sd(as.vector(P[!mask]), na.rm = TRUE)
    }
  }else{
    mask <- matrix(FALSE, nrow = n, ncol = m)
#     P <- (P - mean(P, na.rm = TRUE))/sd(as.vector(P), na.rm = TRUE)
  }

  #------------------------------
  #- Determine ridge orientations
  #------------------------------
  #  BLURING
  if(!is.null(kBlur)){
    k <- do.call( gkernel, kBlur)
    P_f  <- convolution2D(P, k)
  }else{
    P_f <- P
  }

  # GRADIENT FIELD
  # window size for edge dectection
  kdx <- do.call( dx_gkernel, kEdge)
  kdy <- do.call( dy_gkernel, kEdge)
  vx <- convolution2D(P_f, kdx)/dxy[1]
  vy <- convolution2D(P_f, kdy)/dxy[2]
#   image2D(vy)
#   image2D(vx)
  # local TENSOR
  Gxx <- vx^2
  Gyy <- vy^2
  Gxy <- vx*vy 
    
  # LOCAL AVERAGED TENSOR
  #sze = 5 *2
  kg <- do.call(gkernel, kTensor)
  Jxx  <- convolution2D(Gxx, kg)
  Jyy  <- convolution2D(Gyy, kg)
  Jxy  <- convolution2D(Gxy, kg)
  Jxx[Jxx < .Machine$double.eps^0.75] <- 0
  Jyy[Jyy < .Machine$double.eps^0.75] <- 0
  
#   image2D(Jxx)
#   image2D(Jyy)
#   image2D(Jxy)
  
  
  # polar parametrisation
  energy <- Jxx + Jyy                               # energy
  anisot  <- sqrt((Jxx-Jyy)^2 + 4*(Jxy)^2)/energy   # anisotropy
  orient <- 1/2*atan2(2*Jxy, (Jxx - Jyy) ) + pi/2     # orientation
  mask2 <- mask | is.infinite(energy) | is.infinite(anisot)
  anisot[mask2] <- 0
  energy[mask2] <- 0
  orient[mask2] <- 0
#   anisot[is.infinite(orient)] <- 0

  # ANALYTIC SOLUTION BASED ON SVD DECOMPOSITION
  Jeig <- eigenDecomp2x2SymMatrix(Jxx, Jyy, Jxy)
  Jeig$u1x[mask2] <- 0
  Jeig$u1y[mask2] <- 0
  Jeig$u2x[mask2] <- 0
  Jeig$u2y[mask2] <- 0
  Jeig$l1[mask2] <- 0
  Jeig$l2[mask2] <- 0
  mode(mask2) <- "integer"
  
  return(list(tensor  = list("xx" = Jxx,
                             "yy" = Jyy,
                             "xy" = Jxy),
              vectors = list("u1x" = Jeig$u1x,
                             "u1y" = Jeig$u1y,
                             "u2x" = Jeig$u2x,
                             "u2y" = Jeig$u2y),
              values  = list("l1" = Jeig$l1,
                             "l2" = Jeig$l2),
              polar   = list("energy" = energy,
                             "anisotropy" = anisot,
                             "orientation" = orient),
              mask = mask2))
}

#' Plot structure tensor on GPR data
#' 
#' @name plotTensor
#' @rdname plotTensor
#' @export
plotTensor <- function(x, O, type=c("vectors", "ellipses"), normalise=FALSE,
                      spacing=c(6,4), len=1.9, n=10, ratio=1,...){
  type <- match.arg(type, c("vectors", "ellipses"))
  n <- nrow(x)
  m <- ncol(x)

  len1 <- len*max(spacing*c(x@dx,x@dz));  # length of orientation lines
  
  # Subsample the orientation data according to the specified spacing
  v_y = seq(spacing[1],(m-spacing[1]),by=spacing[1])
  v_x = seq(spacing[2],(n-spacing[2]), by=spacing[2])
  
  # Determine placement of orientation vectors
  if(length(x@coord)>0){
    xvalues <- posLine(x@coord)
  }else{
    xvalues <- x@pos
  }
  X = matrix(xvalues[v_y],nrow=length(v_x),ncol=length(v_y),byrow=TRUE)
  Y = matrix(-x@depth[v_x],nrow=length(v_x),ncol=length(v_y),byrow=FALSE)
  
  angle = O$polar$orientation[v_x, v_y];
  l1 <- O$values[[1]][v_x, v_y]
  l2 <- O$values[[2]][v_x, v_y]

  if(type == "vectors"){
    #Orientation vectors
    dx0 <- cos(angle)
    dy0 <- sin(angle)
    normdxdy <- 1
    if(isTRUE(normalise)){
      normdxdy <- sqrt(dx0^2 + dy0^2)
    }
    dx <- len1*dx0/normdxdy/2
    dy <- len1*dy0/normdxdy/2*ratio
    segments(X - dx, (Y - dy), X + dx , (Y + dy))#,...)
  }else if(type == "ellipses"){
  l1[l1 < 0] <- 0
    l2[l2 < 0] <- 0
    a <- 1/sqrt(l2)
    b <- 1/sqrt(l1)
#     normdxdy <- matrix(max(a[is.finite(a)],b[is.finite(b)]),
    normdxdy <- matrix(max(a[is.finite(a)]),
                nrow=length(v_x),ncol=length(v_y))
    if(isTRUE(normalise)){
      normdxdy <- b
    }
    for(i in seq_along(v_x)){
      for(j in seq_along(v_y)){
        aij <- ifelse(is.infinite(a[i,j]), 0, a[i,j])
        bij <- ifelse(is.infinite(b[i,j]), 0, b[i,j])
        if(isTRUE(normalise)){
          maxab <- max(aij, bij)
          if(maxab != 0){
            aij <- aij/maxab
            bij <- bij/maxab
#             cat(i,".",j,"  > a =",aij, "  b =", bij, "\n")
#           normdxdy <- b
          }
        }
        if(!(aij == 0 & bij == 0)){
          E <- ellipse(saxes = c(aij, bij*ratio)*len1,
#           /normdxdy[i,j], 
                      loc   = c(X[i,j], Y[i,j]), 
                      theta = -angle[i,j], n=n)
          polygon(E, ...)
        }
      }
    }
    #     a <- 1/sqrt(l1)
    #     b <- 1/sqrt(l2)
    #     normdxdy <- matrix(max(a,b),nrow=length(v_x),ncol=length(v_y))
    #     if(isTRUE(normalise)){
    #       normdxdy <- a
    #     }
    #     for(i in seq_along(v_x)){
    #       for(j in seq_along(v_y)){
    #         E <- ellipse(saxes = c(a[i,j], b[i,j]*ratio)*len1/normdxdy[i,j], 
    #                     loc   = c(X[i,j], -Y[i,j]), 
    #                     theta = -angle[i,j], n=n)
    #         polygon(E,...)
    #       }
    #     }
  }
}

#' Plot structure tensor on GPR data
#' 
#' @name plotTensor0
#' @rdname plotTensor0
#' @export
plotTensor0 <- function(alpha, l1, l2,  x, y, col = NULL ,
                type=c("vectors", "ellipses"), normalise=FALSE,
                spacing=c(6,4), len=1.9, n=10, ratio=1,...){
  type <- match.arg(type, c("vectors", "ellipses"))
  alpha <- t(alpha[nrow(alpha):1, ])
  l1 <- t(l1[nrow(l1):1, ])
  l2 <- t(l2[nrow(l2):1, ])
  
  n <- nrow(alpha)
  m <- ncol(alpha)
  
  dxy <- c(mean(diff(x)), mean(diff(y)))
  
  len1 <- len*max(spacing * dxy);  # length of orientation lines
  
  # Subsample the orientation data according to the specified spacing
  v_x = seq(spacing[1],(n-spacing[1]),by=spacing[1])
  v_y = seq(spacing[2],(m-spacing[2]), by=spacing[2])
  
  # Determine placement of orientation vectors
#   xpos <- seq(0, by = dxy[1], length.out = n)
#   ypos <- seq(0, by = dxy[2], length.out = m)
  xsub <- x[v_x]
  ysub <- y[v_y]
  X = matrix(xsub,nrow=length(v_x),ncol=length(v_y),byrow=FALSE)
  Y = matrix(ysub,nrow=length(v_x),ncol=length(v_y),byrow=TRUE)
  
#   angle <- O$polar$orientation[v_x, v_y]
  angle <- alpha[v_x, v_y]
#   l1 <- O$values[[1]][v_x, v_y]
  l1 <- l1[v_x, v_y]
#   l2 <- O$values[[2]][v_x, v_y]
  l2 <- l2[v_x, v_y]
  
  if(type == "vectors"){
    #Orientation vectors
    dx0 <- sin(angle)
    dy0 <- cos(angle)
    normdxdy <- 1
    if(isTRUE(normalise)){
      normdxdy <- sqrt(dx0^2 + dy0^2)
    }
    dx <- len1*dx0/normdxdy/2
    dy <- len1*dy0/normdxdy/2*ratio
    segments(X - dx, (Y - dy), X + dx , (Y + dy),...)
  }else if(type == "ellipses"){
    if(is.null(col)){
      col <- colorspace::heat_hcl(101, c = c(80, 30), l = c(30, 90), 
                                power = c(1/5, 2))
    }
    
#     l1[l1 < .Machine$double.eps] <- .Machine$double.eps
#     l2[l2 < .Machine$double.eps] <- .Machine$double.eps
    l1[l1 < 0] <- 0
    l2[l2 < 0] <- 0
    b <- 1/sqrt(l2)
    a <- 1/sqrt(l1)
    lcol <- l1 + l2
    lcol <- 1+(lcol - min(lcol))/(max(lcol)-min(lcol)) * 100
#     normdxdy <- matrix(max(a[is.finite(a)],b[is.finite(b)]),
    normdxdy <- matrix(max(a[is.finite(a)]),
                nrow=length(v_x),ncol=length(v_y))
    if(isTRUE(normalise)){
      normdxdy <- b
    }
    for(i in seq_along(v_x)){
      for(j in seq_along(v_y)){
        aij <- ifelse(is.infinite(a[i,j]), 0, a[i,j])
        bij <- ifelse(is.infinite(b[i,j]), 0, b[i,j])
        if(isTRUE(normalise)){
          maxab <- max(aij, bij)
          if(maxab != 0){
            aij <- aij/maxab
            bij <- bij/maxab
            cat(i,".",j,"  > a =",aij, "  b =", bij, "\n")
#           normdxdy <- b
          }
        }
        if(!(aij == 0 & bij == 0)){
          E <- ellipse(saxes = c(aij, bij*ratio)*len1,
#           /normdxdy[i,j], 
                      loc   = c(X[i,j], Y[i,j]), 
                      theta = pi/2 - angle[i,j], n=n)
          polygon(E, col=col[lcol[i,j]])
        }
      }
    }
  }
}


#   image2DN(x@data)
#    X = matrix(v_y,nrow=length(v_x),ncol=length(v_y),byrow=TRUE)
#   Y = matrix(v_x,nrow=length(v_x),ncol=length(v_y),byrow=FALSE)
#   for(i in seq_along(v_x)){
#     for(j in seq_along(v_y)){
#       E <- ellipse(saxes = c(l1[i,j], l2[i,j])/max(m,n)/5, 
#                    loc   = c(X[i,j]/m, 1-Y[i,j]/n), 
#                    theta = -angle[i,j], n=10)
#       lines(E, lwd=0.5)
#     }
#   }
#   for(i in seq_along(v_x)){
#     for(j in seq_along(v_y)){
#       E <- ellipse(saxes = c(l1[i,j], l2[i,j])/max(l1,l2)*len1, 
#                    loc   = c(X[i,j], Y[i,j]), 
#                    theta = -angle[i,j], n=10)
#       lines(E, lwd=0.5)
#     }
#   }


#-----------------
#-----------------


# n = nrow
# m = mrow
#' Gaussian 2d-kernel
#'
# sigma = sd
#' @name gkernel
#' @rdname kernels
#' @export
gkernel <- function(n, m, sd=1){
  n <- ifelse(n %% 2 == 0, n + 1, n)
  m <- ifelse(m %% 2 == 0, m + 1, m)
  siz <- (n - 1)/2;
  y <- matrix(-siz:siz, nrow = n, ncol = m)
  siz <- (m - 1)/2;
  x <- matrix(-siz:siz, nrow = n, ncol = m, byrow = TRUE)
  g <- exp(-(x^2+y^2)/(2*sd^2))
  sumg <- sum(g)
  if(sumg != 0){
    return( g/sumg )
  }else{
    return( g )
  }
}

#' Gaussian x-derivative kernel (edge detector)
#'
#' @name dx_gkernel
#' @rdname kernels
#' @export
dx_gkernel <- function(n, m, sd=1){
  n <- ifelse(n %% 2 == 0, n + 1, n)
  m <- ifelse(m %% 2 == 0, m + 1, m)
  siz <- (n - 1)/2;
  y <- matrix(-siz:siz, nrow = n, ncol = m)
  siz <- (m - 1)/2;
  x <- matrix(-siz:siz, nrow = n, ncol = m, byrow = TRUE)
  g <- x*exp(-(x^2+y^2)/(2*sd^2))
  return(g)

}

#' Gaussian y-derivative kernel (edge detector)
#'
#' @name dy_gkernel
#' @rdname kernels
#' @export
dy_gkernel <- function(n, m, sd=1){
  n <- ifelse(n %% 2 == 0, n + 1, n)
  m <- ifelse(m %% 2 == 0, m + 1, m)
  siz <- (n - 1)/2;
  y <- matrix(-siz:siz, nrow = n, ncol = m)
  siz <- (m - 1)/2;
  x <- matrix(-siz:siz, nrow = n, ncol = m, byrow = TRUE)
  g <- y*exp(-(x^2+y^2)/(2*sd^2))
  return(g)
}

#' Two-dimensional convolution
#' 
#' The convolution is performed with 2D FFT
#' @name convolution2D
#' @rdname convolution2D
#' @export
convolution2D <- function(A,k){
  nA = nrow(A)
  mA = ncol(A)
  nk = nrow(k)
  mk = ncol(k)
  if(nk > nA || mk > mA){
    stop("Kernel 'k' should be smaller than the matrix 'A'\n")
  }
  A0 <- paddMatrix(A, nk, mk)
  nL <- nrow(A0)
  mL <- ncol(A0)
  k0 <- matrix(0, nrow=nL, ncol=mL)
  # h0[(nk-1) + 1:nh, (mk-1) + 1:mh] <- A
#   A0[1:nA,  1:mA] <- A
  k0[1:nk, 1:mk] <- k
  g <- Re(stats::fft(stats::fft(k0)*stats::fft(A0),inverse=TRUE))/(nL * mL)
  g2 <- g[nk + nk/2  + (1:nA), mk +mk/2 + (1:mA)]
  # g2 <- g[nk + 1:nh, mk + 1:mh]
  return(g2)
}

#' Displacement to align two matrix
#'
#' Displacement to align two matrix
#' @export
displacement <- function(x, y, method=c("phase", "WSSD"), dxy = NULL){
  nm <- c(max(nrow(x), nrow(y)), max(ncol(x), ncol(y)))
  #--- weighted sum of squared differences
  if(method == "WSSD"){
    nmx <- dim(x)
    nmy <- (dim(x) + 2*nm - dim(y))/2
    x0 <- paddMatrix(x, nm[1], nm[2], zero=TRUE)
    y0 <- paddMatrix(y, nmy[1], nmy[2], zero=TRUE)
    # weights (zero outside the image range)
    wx <- matrix(1,nrow=nrow(x),ncol=ncol(x))
    wx <- paddMatrix(wx, nm[1], nm[2], zero=TRUE)
    wy <- matrix(1,nrow=nrow(y),ncol=ncol(y))
    wy <- paddMatrix(wy, nmy[1], nmy[2], zero=TRUE)
    WX <- fft(wx)
    WY <- fft(wy)
    X  <- fft(wx * x0)
    Y  <- fft(wy * y0)
    X2  <- fft(wx * x0^2)
    Y2  <- fft(wy * y0^2)
    WSSD <- Mod(fft(WX * Conj(Y2) + X2 * Conj(WY) - 2 * X * Conj(Y), 
                    inverse = TRUE))
    WSSD <- WSSD[1:nm[1] , 1:nm[2]]
    d <- which(WSSD == max(WSSD), arr.ind = TRUE)[1,]
  #--- phase correlation
  }else if(method == "phase"){
#     n <- min(nrow(x), nrow(y))
#     m <- min(ncol(x), ncol(y))
#     X <- fft(x[1:n, 1:m])
#     Y <- fft(y[1:n, 1:m])
    x0 <- padmat(x, n = nm[1], m = nm[2])
    y0 <- padmat(y, n = nm[1], m = nm[2])
    X <- fft(x0)
    Y <- fft(y0)
    R <- Mod(fft( X * Conj(Y) / (Mod(X*Y)), inverse = TRUE))
#     R <- R[1:nm[1] , 1:nm[2]]
    if(!is.null(dxy)){
      R <- R[c(1:(dxy[1] + 1), (nm[1] - dxy[1]+1):nm[1]), 
            c(1:(dxy[2] + 1), (nm[2] - dxy[2]+1):nm[2]), drop = FALSE]
      nm <- 2*dxy
    }
    d <- which(R == max(R), arr.ind = TRUE)[1,] - 1
    if(d[1] > nm[1]/2) d[1] <- d[1] - nm[1]
    if(d[2] > nm[2]/2) d[2] <- d[2] - nm[2]
  }
  return(d)
}


#' shift a matrix by n and m
#'     
#' shift a matrix by n and m
#' @export
shiftmat <- function(x, n, m){
  xs <- matrix(0,nrow=nrow(x), ncol=ncol(x))
  vx0 <- 1:nrow(xs) + n
  vx0 <- vx0[vx0 > 1 & vx0 <= nrow(xs)]
  vx1 <- vx0 - n
  vy0 <- 1:ncol(xs) + m
  vy0 <- vy0[vy0 > 1 & vy0 <= ncol(xs)]
  vy1 <- vy0 - m
  xs[vx0, vy0] <- x[vx1, vy1]
  return(xs)
}

#' pad a matrix
#' 
#' pad a matrix
#' @export
padmat <- function(x, n, m, what = 0){
  x0 <- matrix(what, ncol=m, nrow=n)
  x0[1:nrow(x),1:ncol(x)] <- x
  return(x0)
}

# pads the edges of an image to minimize edge effects 
# %during convolutions and Fourier transforms. 
# %Inputs %I - image to pad 
# %p - size of padding around image 
# %Output %Ipad - padded image 
# SOURCE: http://matlabgeeks.com/tips-tutorials/how-to-blur-an-image-with-a-
# fourier-transform-in-matlab-part-i/
 # service@matlabgeeks.com i
paddMatrix <- function(I, p1, p2=NULL, zero = FALSE){
  if(is.null(p2)){
    p2 <- p1
  }
  nI <- nrow(I)
  mI <- ncol(I)
  Ipad <- matrix(0, nrow = nI + 2*p1, ncol = mI + 2*p2)
  # middle
  Ipad[(p1+1):(p1+nI),(p2+1):(p2+mI)] <- I
  # top and bottom
  if(zero == FALSE){
    Ipad[1:p1,(p2+1):(p2+mI)] <- repmat(I[1,,drop=FALSE], p1, 1)
    Ipad[(p1+nI+1):(nI+2*p1), (p2+1):(p2+mI)] <- repmat(I[nI,,drop=FALSE], 
                                                        p1, 1)
  }
  if(p2 > 0 && zero == FALSE){
    # left and right
    Ipad[(p1+1):(p1+nI), 1:p2] <- repmat(I[,1,drop=FALSE], 1, p2)
    Ipad[(p1+1):(p1+nI), (p2+mI+1):(mI + 2*p2)] <- 
                            repmat(I[,mI, drop=FALSE],1,p2)
    # corner
    Ipad[1:p1, 1:p2] <- I[1,1]
    Ipad[(p1+nI+1):(nI+2*p1), (p2+mI+1):(mI+2*p2)] <- I[nI,mI]
    Ipad[1:p1,(p2+mI+1):(mI + 2*p2)] <- I[1,mI]
    Ipad[(p1+nI+1):(nI+2*p1), 1:p2] <- I[nI,1]
  }
  return(Ipad)
}



#' Repeat matrix
#'
#' Repeat a matrix row-wise n times and column-wise m times.
#' 
#' Source
#' A replication of MatLab repmat function!
#' R FOR OCTAVE USERS
#' version 0.4, Copyright (C) 2001 Robin Hankin
#' http://cran.r-project.org/doc/contrib/R-and-octave.txt
#' @name repmat
#' @rdname repmat
#' @export
repmat <- function(A,n,m) {
  kronecker(matrix(1,n,m),A)
}



#---------------- CONVOLUTION --------------------#

# # linear convolution with fft
# # a = vector
# # b = vector
# convolution <- function(a,b){
#   na <- length(a)
#   nb <- length(b)
#   L <- na + nb - 1
#   a0 <- c(a,rep(0,nb-1))
#   b0 <- c(b, rep(0,na-1))
#   y <- Re(fft(fft(a0)*fft(b0),inverse=TRUE))/L
#   return(y[1:(max(na,nb))])
# }


#' Linear convolution based on FFT
#'
#' If A (or B) is a numeric vector, it is converted into a one-column 
#' matrix. Then if A and B do not have the same number of column, then the 
#' first column of the matrix with the smallest number of column is repeated to
#' match the dimension of the other matrix.
#' match the dimension of the other matrix.
#' @param A A numeric vector or matrix.
#' @param A B numeric vector or matrix.
#' @name convolution
#' @rdname convolution
#' @export
convolution <- function(A,k){
  if(is.null(dim(A))){
    dim(A) <- c(length(A),1)
  }
  if(is.null(dim(k))){
    dim(k) <- c(length(k),1)
  }
  if(ncol(k) < ncol(A)){
    k <- repmat(k[,1, drop = FALSE], 1, ncol(A)) 
  }
#   else if(ncol(B) > ncol(A)){
#     A <- repmat(A[,1, drop = FALSE], 1, ncol(B)) 
#   }
  nA <- nrow(A)
  nk <- nrow(k)
  Apad <- paddMatrix(A, nk, 0)
  k0 <- matrix(0, nrow = nrow(Apad), ncol= ncol(Apad))
  k0[1:nk, ] <- k
#   B0 <- rbind(B0, B, B0)
  Y <- Re(stats::mvfft(stats::mvfft(Apad) * stats::mvfft(k0), 
          inverse=TRUE))/nrow(Apad)
  return(Y[1:nA + nk + nk/2 + 1, ])
}


# cf. matlab
# A convolution matrix is a matrix, formed from a vector, 
# whose product with another vector 
# is the convolution of the two vectors.

# A = convmtx(y,nf) returns the convolution matrix, A, 
# such that the product of A and a vector, x, 
# is the convolution of y and x. 
# If y is a column vector of length m, A is (m+nf-1)-by-nf and the 
# product of A and a column vector, x, of length n is the 
# convolution of y and x. 
convmtx <- function(y, nf){
  ny <- length(y)
  L <- nf + ny -1
  # convolution matrix Y
  yext <- rep(c(y,rep(0,L-ny+1)),nf)
  yext <- yext[1:(L*nf)]
  return( matrix(yext,nrow=L,ncol=nf))
}



#---------------- DECONVOLUTION --------------------#
# spectral deconvolution with known wavelet
# convolution model: y = h*x 
# h and y are known, x is unknown
# x ~ H^h * Y / (H^h * H + mu)
deconvolve <- function(y,h,mu=0.0001){
  ny <- length(y)
  nh <- length(h)
  L  <- ny + ny - 1
  H  <- stats::fft(c(h,rep(0,ny-1)))
  Y  <- stats::fft(c(y, rep(0,nh-1)))
  Re(stats::fft( t(Conj(H))*Y/(t(Conj(H))*H + mu) ,inverse=TRUE))[1:ny]/L
  # Re(fft( Y/(H + mu) ,inverse=TRUE))[1:ny]/L
}


# TO CHECK!!!!
# deconvolution with known wavelet
# convolution model: y = h*x 
# h and y are known, x is unknown
# x ~ H^h * Y / (H^h * H + mu)
deconvolutionMtx <- function(y,h,nf,mu=0.0001){
  # ny <- length(y)
  # nh <- length(h)
  # L  <- ny + ny - 1
  H  <- convmtx(h,nf)
  y_acf <- as.numeric(acf(y,lag=nf-1,plot=FALSE)[[1]])
  y_acf[1] <- y_acf[1] + mu
  HtH <- toeplitz(y_acf)
  x <-  solve(HtH) %*% (t(H[1:nf,1:nf]) %*% y)
  return(x)
}



# setGenericVerif("rmsScaling", function(x) standardGeneric("rmsScaling"))
#' Optimum Phase Rotation
#'
#' @param x any data that can be converted into a numeric vector with 
#'          as.vector.
#' @param rot The phase rotation increment.
#' @param plot A lenth-one boolean vector. If TRUE, the kurtosis as a function
#'             of phase angle is plotet.
#' @name optPhaseRotation
#' @rdname optPhaseRotation
#' @export
optPhaseRotation <- function(x,rot=0.01,plot=TRUE){
  # x_dec <- as.vector(gpr/apply(as.matrix(gpr),2,RMS))
  x_dec <- as.vector(x)
  pi_seq <- seq(0,pi,by=rot)
  kurt <- numeric(length(pi_seq))
  nx <- length(x_dec)
  for(i in seq_along(pi_seq)){
    xrot <- phaseRotation(x_dec, pi_seq[i])
    # xrot_scaled2 <- (xrot -   mean(xrot))^2
    # kurt[i] <- ((1/nx) * sum( xrot_scaled2^2)) / 
    # ( (1/nx) *sum( xrot_scaled2))^2 
    kurt[i] <- e1071::kurtosis( xrot)
  }
  phi_max <- pi_seq[which.max(kurt)]
  message("rotation angle = ", phi_max/pi*180, " degree")
  # dev.off(); windows()
  if(plot==TRUE){
    plot(pi_seq/pi*180,kurt,type="l")
    abline(v=phi_max/pi*180,col="red")
  }
  return(phi_max)
  # x_dec <- phaseRotation(x_dec, phi_max)
}

# y is the wavelet and we want 
# a filter f s.t. f*y = d 
# with d = [0 ... 0 1 0 ...0]
# 1 at the postion i = shft
# if shft = NULL, the shift is chosen by the
#     algorithm and also returned
# if shift is not NULL, case of wavelet estimation
#     from the trace via the autocorrelation matrix.
# mu = percent of pre-whitening
.spikingFilter <- function(y,nf=32,mu=0.1,shft=1){
  # R = t(Y)%*%Y = Toepliz matrix of ACF
  y_acf <- as.numeric(acf(y,lag=nf-1,plot=FALSE)[[1]])
  taper <- hammingWindow(2*nf)
  y_acf <- y_acf*taper[(nf+1):(2*nf)] 
  y_acf[1] <- y_acf[1] + mu
  YtY <- toeplitz(y_acf)
  # all the spiking filters
  if(is.null(shft)){
    ny <- length(y)
    L <- nf + ny -1
    # convolution matrix Y
    Y <- convmtx(y,nf)
    H <- solve(YtY) %*% t(Y) 
    v <- numeric(L)
    # performance matrix: all spiking filter outputs
    P <- Y %*% H
    # optimal delay (smallest error)
    i <- which.max(diag(P))
    v[i] <- 1
    h <- H%*%v
    return(list("h"=h,"delay"=i))
  }else{
    v <- numeric(nf)
    v[shft] <- 1
    h <- solve(YtY) %*% v 
    return(h)
  }
}







# version vectoriel!!!!
#' Return points that are within a polygon
#' 
#' Return points that are within a polygon
#' @export
inPoly <- function(x, y, vertx, verty){
  inPo <- rep(0L, length(x))
  nvert <- length(vertx)
  for(i in 1:nvert){
    j <- ifelse(i==1, nvert,i-1)
    myTest <- ((verty[i] > y) != (verty[j]>y)) &
                (x < (vertx[j]-vertx[i]) * (y-verty[i]) / 
                (verty[j]-verty[i]) + vertx[i])
    inPo[myTest] <- !inPo[myTest]
  }
  return(inPo)
}

.FKSpectrum <- function(A, dx = 0.25, dz = 0.8, npad = 1, 
                        p = 0.01, plotSpec = TRUE){
  # A <- GPR$data    #[90:1000,]
  nr <- nrow(A)  # time  
  nc <- ncol(A)  # x  

  #============== PLOT F-K SPECTRUM ===============#
  # padding (try also 2*(2^nextpow2(nc))
  nk <- npad*(nextpower2(nc))
  nf <- npad*(nextpower2(nr))
  A1 <- matrix(0,nrow=nf,ncol=nk)
  A1[1:nr,1:nc] <- A

  # function to center the spectrum!! (no need of fttshift!)
  #centres spectrum: Gonzalez & Wintz (1977) Digital Image Processing p.53
  A1  <- A1 * (-1)^(row(A1) + col(A1))
  A1_fft <- stats::fft(A1)
  A1_fft_pow <- Mod(A1_fft)
  A1_fft_phase <- Arg(A1_fft)
  # plotGPR((A1_fft_phase[1:(nf/2),])^0.05)

  # Sampling frequency [Hz] = 1 / Sample time [s]
  Fs = 1/(dz*10^(-9))
  fac = 1000000
  fre = Fs*seq(0,nf/2)/nf/fac
  
  # wavenumber
  Ks <- 1/dx      # [1/m] Sampling frequency
  knu <- 1:(nk/2)/(2*(nk/2)) * Ks  #[1/m]
  knutot <- c(-rev(knu),knu)

  # labels: find a function between "xat" and "xLabels" and use "pretty()"
  xat   <- c(0,nk/2,nk)/nk
  xLabels <- c(min(knutot), 0, max(knutot))
  yat    <- c(0,nf/2,nf)/nf
  yLabels  <- c(0, max(fre)/2, max(fre))

  # Note: when plotting spectra (S)  use log(S) or S.^alpha (alpha=0.1-0.3) to
  #       increase the visibility of small events 
  # p = 0.05
  if(plotSpec){
    plot3D::image2D(x = knutot, y = fre, z = (t(A1_fft_pow[1:(nf/2),])^p), 
                xlab="wavenumber (1/m)",
                ylab="frequency MHz")
#      axis(side=4, labels=TRUE)

  }
   return(list(pow=A1_fft_pow[1:(nf/2),], 
               pha=A1_fft_phase[1:(nf/2),],
               fre = fre,
               wnb = knutot))
}




.FKFilter <- function(A, fk, L = c(5, 5), npad=1){
  nr <- nrow(A)  # time  
  nc <- ncol(A)  # x  

  #============== PLOT F-K SPECTRUM ===============#
  # padding (try also 2*(2^nextpow2(nc))
  nk <- npad*(nextpower2(nc))
  nf <- npad*(nextpower2(nr))
  A1 <- matrix(0,nrow=nf,ncol=nk)
  A1[1:nr,1:nc] <- A

  # function to center the spectrum!! (no need of fttshift!)
  #centres spectrum: Gonzalez & Wintz (1977) Digital Image Processing p.53
  # A1  <- A1 * (-1)^(row(A1) + col(A1))
  A1_fft <- stats::fft(A1)
  
  # plotGPR(Mod(A1_fft)^0.05)
  # plotGPR(Re(fft(A1_fft,inv=TRUE))[1:nr,1:nc])
  # plotGPR(A)
  
  #============== FILTER F-K SPECTRUM ===============#
  myFlong <- matrix(0,nrow=nf,ncol=nk)
  myFlong[1:(nf/2),1:(nk/2)] <- fk[(nf/2):1,(nk/2):1]
  # myFlong  <- myFlong * (-1)^(row(myFlong) + col(myFlong))
  myFlong[(nf/2+1):(nf),(nk/2 + 1):nk] <- fk[1:(nf/2),1:(nk/2)]
  myFlong[1:(nf/2),(nk/2 + 1):nk] <- fk[(nf/2):1,(nk):(nk/2 + 1)]
  # myFlong[(nf/2+1):(nf),1:(nk/2)] <- fk[1:(nf/2),(nk/2 + 1):(nk)]
  myFlong[(nf/2+1):(nf),1:(nk/2)] <- fk[1:(nf/2),(nk/2 + 1):nk]
  # plotGPR(myFlong)


  # hamming window
  if(length(L)==1) L <- c(L,L)
  if(all(L!=0)){
    ham2D <- hammingWindow(L[1])%*%t(hammingWindow(L[2]))
    ham2Dlong <- matrix(0,nrow=nf,ncol=nk)
    ham2Dlong[1:L[1],1:L[2]] <- ham2D
    # plotGPR(ham2Dlong)
    FF <-  Re(stats::fft(stats::fft(myFlong) * stats::fft(ham2Dlong),inv=TRUE))
  }else{
    FF <- myFlong
  }
  FF <- FF/sum(FF)
  
  # plotGPR(Re(fft(fft(myFlong) * fft(ham2Dlong),inv=TRUE))[1:nr,1:nc])
  
  A_back <- Re(stats::fft(A1_fft * FF,inv=TRUE))[1:nr,1:nc]
  # plotGPR(A_back)
  # plotGPR(A_back)
  # scaling
  return(A_back/(max(A_back)-min(A_back))*(max(A)-min(A)))
}


# -------------------------------------------
# ------------readDT1--------------------------
# -------------------------------------------
# @name  readDT1 (plot Ground Penetrating Radar image)
# @description This function read *.HD and associated *.DT1
# files from Sensors & Software.

# @date 30.04.2014 08:33
# @auteur Emanuel Huber
# @param [text]    fPath       (file path of *.hd or *.dt1 file)
# @require source("trimStr.R")
  # source('trimStr.R')
  # cat('> Function(s) loaded: "trimStr.R" \n')
# @return list((hd = headerHD, dt1hd = headerDT1, data=myData))
# -------------------------------------------

readDT1 <- function( fPath){
  fName <- getFName(fPath, ext = c(".HD", ".DT1"))
  #dirName     <- dirname(fPath)
  #baseName    <- .fNameWExt(fPath)
  #fileNameHD  <- file.path(dirName, paste0(baseName,".HD"))
  #fileNameDT1 <- file.path(dirName, paste0(baseName,".DT1"))
  #--- read header file
  headHD <- scan( fName$hd, what = character(), strip.white = TRUE,
                  quiet = TRUE, fill = TRUE, blank.lines.skip = TRUE, 
                  flush = TRUE, sep = "\n")
  nHD <- length(headHD)
  hHD <- data.frame( tag = character(), val = character(), 
                          stringsAsFactors = FALSE)
  for(i in seq_along(headHD)){
    hdline <- strsplit(headHD[i], "=")[[1]]
    if(length(hdline) < 2){
      hHD[i,1] <- ""
      hHD[i,2] <- trimStr(hdline[1])
    }else{
      hHD[i,1:2] <-  as.character(sapply(hdline[1:2],trimStr))
    }
  }
  nTr <- .getHD(hHD, "NUMBER OF TRACES")
  nPt <- .getHD(hHD, "NUMBER OF PTS/TRC")
  #--- READ DT1
  tags <- c("traces", "position", "samples","topo", "NA1", "bytes",
            "tracenb", "stack","window","NA2", "NA3", "NA4",
            "NA5", "NA6", "recx","recy","recz","transx","transy",
            "transz","time0","zeroflag", "NA7", "time","x8","com")  
  hDT1 <- list()
  dataDT1 <- matrix(NA, nrow = nPt, ncol = nTr)
  con <- file(fName$dt1 , "rb")
  for(i in 1:nTr){
    for(j in 1:25){
      hDT1[[tags[j]]][i] <- readBin(con, what = numeric(), n = 1L, size = 4)
    }
    # read the 28 characters long comment
    hDT1[[tags[26]]][i] <- readChar(con, 28)
    # read the nPt * 2 bytes trace data
    dataDT1[,i] <- readBin(con, what=integer(), n = nPt, size = 2)
  }
  close(con)
  return( list(hd = hHD, dt1hd = hDT1, data = dataDT1) )
}
#-----------------
#-----------------

# A = GPR$hd
# if position = TRUE, return the row number
# if number = TRUE, try to convert
.getHD <- function(A,string,number=TRUE,position=FALSE){
  if(number){
    value <- as.numeric(A[trimStr(A[,1]) == string, 2])
  }else{
    value <- A[trimStr(A[,1]) == string,2]
  }
  if(length(value)>0){
    if(position){
      pos <- which((trimStr(A[,1]) == string ) == TRUE)[1]
      return(c(value,pos))
    }else{
      return(value)
    }
  }else{
    return(NULL)
  }
}

  
#--------------- read MALA files -------------------#
readRD3 <- function(fPath){
  fName <- getFName(fPath, ext = c(".rad", ".rd3"))
  #dirName     <- dirname(fPath)
  #baseName    <- .fNameWExt(fPath)
  #fNameRAD    <- file.path(dirName, paste0(baseName, ".rad"))
  #fNameRD3    <- file.path(dirName, paste0(baseName, ".rd3"))
  #fNameCOR    <- file.path(dirName, paste0(baseName, ".cor"))

  ##---- RAD file
  headRAD <- scan(fName$rad, what = character(), strip.white = TRUE,
                  quiet = TRUE, fill = TRUE, blank.lines.skip = TRUE, 
                  flush = TRUE, sep = "\n")
                  
  nRAD <- length(headRAD)
  hRAD <- data.frame( tag = character(), val = character(), 
                          stringsAsFactors = FALSE)
  for(i in seq_along(headRAD)){
    hdline <- strsplit(headRAD[i], ":")[[1]]
    hRAD[i,1:2] <-  as.character(sapply(hdline[1:2],trimStr))
  }
  nTr <- .getHD(hRAD, "LAST TRACE")
  nPt <- .getHD(hRAD, "SAMPLES")
  
  ##---- RD3 file
  dataRD3 <- matrix(NA, nrow = nPt, ncol = nTr)
  con <- file(fName$rd3 , "rb")
  for(i in seq_len(nTr)){
    dataRD3[, i] <- readBin(con, what = integer(), n = nPt, size = 2)
  }
  close(con)
  
  ##---- COR file
  #if(file.exists(fNameCOR)){
    #hCOR <- read.table(fNameCOR, sep = "\t", dec = ".", header = FALSE,
    #                   stringsAsFactors = FALSE)
    #colnames(hCOR) <- c("traces", "date", "time", "latitude", "longitude",
    #                "height", "accuracy")
    #hCOR <- read.table(textConnection(gsub(",", "\t", readLines(fNameCOR))), 
    #                   dec = ".", header = FALSE, stringsAsFactors = FALSE)

    # colnames(hCOR) <- c("traces", "date", "time", "latitude", 
    #                     "lat", "longitude",
    #                "long", "height", "unit", "accuracy")
    # return(list(hd = hRAD, data = dataRD3, coords = hCOR))
  #}else{
    return(list(hd = hRAD, data = dataRD3))
}


# number of bytes in connection
# file.info(filename)$size
.flen <- function(con){
  pos0 <- seek(con)
  seek(con,0,"end")
  pos <- seek(con)
  seek(con,where=pos0,"start")
  return(pos)
}


# Prism2 ” software
#--------------- read RadSys Zond GPR device files -------------------#
readSEGY <- function(fPath){
  #dirName     <- dirname(fPath)
  #baseName    <- .fNameWExt(fPath)
  #fName    <- file.path(dirName, paste0(baseName, ".sgy"))
  fName <- getFName(fPath, ext = c(".sgy"))
  hd <- c()
  con <- file(fName$sgy , "rb")
  ##---- SEGY file
  uu <- readBin(con, what = character(), n = 1, size = 1)
  vv <- strsplit(uu, split ="\r\n")
  hd$EBCDIC <- sub("\\s+$", "", vv[[1]])
  invisible(seek(con, where = 3200, origin = "start"))
  # Job identification number
  hd$JOB_ID <- readBin(con, what = integer(), n = 1, size = 4)
  # Line number
  hd$LINE_NUMBER <-  readBin(con, what = integer(), n = 1, size = 4)
  # Reel number
  hd$REEL_NUMBER <- readBin(con, what = integer(), n = 1, size = 4)
  # Number of data traces per record
  hd$NB_DATA_TRACES <- readBin(con, what = integer(), n = 1, size = 2)
  # Number of auxiliary traces per record
  hd$NB_AUX_TRACES <- readBin(con, what = integer(), n = 1, size = 2)
  # tspl > Sample interval of this reel's data in PICOseconds
  # hd$TIME_SAMPLING in nanoseconds
  hd$TIME_SAMPLING <-  readBin(con, what = integer(), n = 1, size = 2) * 1e-3
  # Number of samples per trace for this reel's data
  invisible(readBin(con, what = integer(), n = 1, size = 2))
  # samples per trace for this reel's data
  # Nspl
  hd$NB_SAMPLES <- readBin(con, what = integer(), n = 1, size = 2)
  #unused
  invisible(readBin(con, what = integer(), n = 1, size = 2))
  # data sample format code
  # 1 = 32-bit IBM floating point;
  # 2 = 32-bit fixed-point (integer);
  # 3 = 16-bit fixed-point (integer);
  # 4 = 16-bit fixed-point with gain code 
  dsfc <- readBin(con, what = integer(), n = 1, size = 2)
  hd$DATA_FORMAT <- switch(dsfc,
                           "1" = "32-bit IBM floating point",
                           "2" = "32-bit fixed-point",
                           "3" = "16-bit fixed-point",
                           "4" = "16-bit fixed-point with gain code")
  #number of traces per ensemble
  invisible(readBin(con, what = integer(), n = 1, size = 2))
  # not used
  invisible(readBin(con, what = integer(), n = 13, size = 2))
  # mesuring system
  # 1 = meters
  # 2 = feet
  pos_unit <- readBin(con, what = integer(), n = 1, size = 2)
  hd$POS_UNIT <- switch(pos_unit,
                        "1" = "meter",
                        "2" = "feet")
  # not used
  invisible(readBin(con, what = integer(), n = 172, size = 2))
  # 240-byte binary tracer header + trace data
  hd$NB_TRACES <- (.flen(con) - seek(con))/(240 + hd$NB_SAMPLES*2)
  dataSGY <- matrix(nrow = hd$NB_SAMPLES, ncol = hd$NB_TRACES)
  hdt <- matrix(nrow = 7, ncol = hd$NB_TRACES)
  xyfac <- numeric(hd$NB_TRACES)
  for(i in 1:hd$NB_TRACES){
    #--------------------------#
    #--------- header ---------#
    # trace sequence number within line
    invisible(readBin(con, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # not used
    invisible(readBin(con, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # Original field record number
    invisible(readBin(con, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # Trace sequence number within original field record
    invisible(readBin(con, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # not used
    invisible(readBin(con, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # CDP ensemble number || CDP = CMP
    invisible(readBin(con, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # Trace sequence number within CDP ensemble
    invisible(readBin(con, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # Trace identification code:
    # 1 = seismic data;
    # 2 = dead;
    # 3 = dummy;
    # 4 = time break;
    # 5 = uphole;
    # 6 = sweep;
    # 7 = timing;
    # 8 = water break;
    # 9 = optional use
    invisible(readBin(con, what = integer(), n = 1, size = 2, 
                      endian = "little"))
    # Number of vertically summed traces yielding this trace
    invisible(readBin(con, what = integer(), n = 1, size = 2, 
                      endian = "little"))
    # Number of horizontally summed traces yielding this trace
    invisible(readBin(con, what = integer(), n = 1, size = 2, 
                      endian = "little"))
    # data use:
    # 1 = production;
    # 2 = test.
    invisible(readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # not used
    invisible(readBin(con, what = integer(), n = 1L, size = 4, 
                      endian = "little"))
    # Altitude (mean-sea-level)
    invisible(readBin(con, what = numeric(), n = 1L, size = 4, 
                      endian = "little"))
    # Height of geoid above WGS84 ellipsoid
    invisible(readBin(con, what = numeric(), n = 1L, size = 4, 
                      endian = "little"))
    # Backward/toward direction (if negative -backward)
    invisible(readBin(con, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # Datum elevation at source in m (topography offset)
    invisible(readBin(con, what = numeric(), n = 1L, size = 4, 
                      endian = "little"))
    # not used
    invisible(readBin(con, what = integer(), n = 7L, size = 2, 
                      endian = "little"))
    # Scalar for coordinates:
    # + = multiplier; 
    # –= divisor.
    xyfac[i] <- readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little")
    # X source coordinate (Longitude in 32-bit float accuracy for arc seconds)
    invisible(readBin(con, what = integer(), n = 1L, size = 4, 
                      endian = "little"))
    # Y source coordinate (Longitude in 32-bit float accuracy for arc seconds)
    invisible(readBin(con, what = integer(), n = 1L, size = 4, 
                      endian = "little"))
    # X receiver group coordinate
    hdt[4,i] <- readBin(con, what = integer(), n = 1L, size = 4, 
                      endian = "little")
    # Y receiver group coordinate
    hdt[5,i] <- readBin(con, what = integer(), n = 1L, size = 4, 
                      endian = "little")
    # Coordinate units:
    # 1 = length in meters or feets; 
    # 2 = arc seconds (DDMM.SSSS).
    invisible(readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # GPS signal quality
    invisible(readBin(con, what = numeric(), n = 1L, size = 4, 
                      endian = "little"))
    # not used
    invisible(readBin(con, what = integer(), n = 7L, size = 2, 
                      endian = "little"))
    # Lag time between shot and recording start in PICOseconds
    invisible(readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # not used
    invisible(readBin(con, what = numeric(), n = 1L, size = 4, 
                      endian = "little"))
    # Number of samples in this trace = hd$NB_SAMPLES
    invisible(readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # Sample interval of this reel's data in PICOseconds 
    # = hd$TIME_SAMPLING *1e3
    invisible(readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # not used
    invisible(readBin(con, what = integer(), n = 21L, size = 2, 
                      endian = "little"))
    # Hour of day (24 hour clock)
    hdt[1,i] <- readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little")
    # Minute of hour
    hdt[2,i] <- readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little")
    # Second of minute
    hdt[3,i] <- readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little")
    # Time basis code (1 –Local, 2 -GMT)
    invisible(readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # not used
    invisible(readBin(con, what = integer(), n = 7L, size = 2, 
                      endian = "little"))
    # Longitude in 64-bit double accuracy
    invisible(readBin(con, what = numeric(), n = 1L, size = 8, 
                      endian = "little"))
    # Latitude in 64-bit double accuracy
    invisible(readBin(con, what = numeric(), n = 1L, size = 8, 
                      endian = "little"))
    # not used
    invisible(readBin(con, what = integer(), n = 8L, size = 2, 
                      endian = "little"))
    # Time scalar. If positive, scalar is used as a 
    # multiplier. If negative –divisor.
    invisible(readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # not used
    invisible(readBin(con, what = integer(), n = 10L, size = 2, 
                      endian = "little"))
    # Marks indicator. If equal to 0x5555, trace is marked.
    hdt[6,i] <- readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little")
    # Mark number.
    hdt[7,i] <- readBin(con, what = integer(), n = 1L, size = 2, 
                      endian = "little") 
    #---------- trace ---------------#
    if( dsfc == 1){
      # "32-bit IBM floating point"
      dataSGY[,i] <- readBin(con, what = numeric(), n = hd$NB_SAMPLES, 
                             size = 2, endian = "little")
    }else{
      dataSGY[,i] <- readBin(con, what = integer(), n = hd$NB_SAMPLES, 
                             size = 2, endian = "little")
    }
  }
  hdt[4,] <- hdt[4,] * abs(xyfac)^sign(xyfac)
  close(con)

  # byte to volt conversion assuming
  # recording range: [-50mV, 50mV]
  # 16 bytes
  #V <- c(-50,50)
  #nBytes <- 16
  #A2 <- A*abs(diff(V))/(2^nBytes)

  # time sample in nanosecond
  #tt <- seq(0, by = hd$TIME_SAMPLING, length.out = hd$NB_SAMPLES)
  
  # hdt matrix
  # row 1 = hour
  # row 2 = minute
  # row 3 = seconde
  # row 4 = x-pos
  # row 5 = y-pos
  # row 6 = mark indicator
  # row 7 = mark number
  return(list(hd = hd, data = dataSGY, hdt = hdt))
}

  

  
  
#--------------------------------------
# http://stackoverflow.com/questions/17256834/getting-the-arguments-of-a-parent-
# function-in-r-with-names
# Ryan Grannell
# website   twitter.com/RyanGrannell
# location   Galway, Ireland
getArgs <- function (returnCharacter=TRUE, addArgs = NULL) {
  arg <- as.list(match.call(definition = sys.function( -1 ),
           call = sys.call(-1),
           expand.dots = TRUE )
           )
  narg <- length(arg)
  if(returnCharacter){
    if(narg >=3){
      eval_arg <- sapply(arg[3:narg],eval)
      argChar <- paste0(arg[[1]],"//", paste(names(arg[3:narg]),
          sapply(eval_arg,pasteArgs,arg[3:narg]),sep="=",collapse="+"))
    }else{
      argChar <- paste0(arg[[1]],"//")
    }
    if(!is.null(addArgs)){
      argChar <- addArg(argChar, addArgs)
    }
    return(argChar)
  }else{
    return(arg)
  }
}

pasteArgs <- function(eval_arg, arg){
  if(is.numeric(eval_arg) || is.character(eval_arg)){
    return( paste0(eval_arg, collapse = ",") )
  }else if(is.list(eval_arg)){
    return( paste0(names(eval_arg), "<-", (eval_arg), collapse = "," ) )
  }else if(is.matrix(eval_arg)){
    return(paste(arg))
  }else if(any(is.null(eval_arg))){
    return("")
  }
}

addArg <- function(proc, arg){
# paste(names(arg[3:narg]),sapply(eval_arg,paste_args,arg[3:narg]),sep="=",
# collapse="+")
  proc_add <- paste(names(arg), sapply(arg,pasteArgs, arg),
                  sep = "=", collapse = "+")
  if(substr(proc,nchar(proc),nchar(proc)) == "//"){
    proc <- paste(proc, proc_add, sep = "")
  }else{
    proc <- paste(proc, "+", proc_add, sep = "")
  }
  return(proc)
}

# return a character vector containing the name of the FUN function
getFunName <- function(FUN){
  if(class(FUN)=="function"){
    funName <- "FUN"
  }else{
    #  if(isGeneric("FUN")){
    funName0 <- selectMethod(FUN, "numeric")
    funName <-funName0@generic[1]
  }
  return(funName)
}


# -------------------------------------------
# ------------writeDT1--------------------------
# -------------------------------------------
# @name  writeDT1 
# @description This function writes *.HD and associated *.DT1
# files  (Sensors & Software)

# @date 07.11.2012 08:33
# @auteur Emanuel Huber
# @param [text]    fileNameHD       (file path of *.hd file)
# @param [text]    fileNameDT1       (file path of *.dt1 file)


# @return list((hd = headerHD, dt1hd = headerDT1, data=myData))
# -------------------------------------------
.writeDT1 <- function(x, fPath){
  #-------------------------
  # DT1 FILE: traces
  traceData <- x@data  # should ranges between -32768 and 32767
  if(max(traceData)/max(abs(traceData))*32768 <= 32767){
    traceData <- round(traceData/max(abs(traceData))*32768)
  }else{
    traceData <- round(traceData/max(abs(traceData))*32767)
  }
#   cat(range(traceData),"\n")
  if(min(traceData)< -32768 || max(traceData) > 32768){
    stop("problem real > integer conversion")
  }
#   A
#   traceData <- A$data
  storage.mode(traceData) <- "integer"
  
  # DT1 FILE: header
  indexDT1Header=c("traces", "position", "samples","topo", "NA0", "bytes","dz", 
          "stacks","NA1","NA2", "NA3", "NA4", "NA5", "NA6", "recx","recy",
          "recz","transx","transy","transz","time0","zeroflag", "NA7", "time",
          "x8", "com","com1","com2","com3","com4","com5","com6")
  traces_hd <- list()
  traces_hd$traces <- x@traces
  traces_hd$position <- x@pos
  traces_hd$samples <- rep(nrow(x@data),ncol(x@data))
  if(length(x@coord) > 0 && sum(is.na(x@coord)) > 0){
    traces_hd$topo <- x@coord[,3]
  }else{
    traces_hd$topo <- rep.int(0L,ncol(x@data))
  }
  traces_hd$NA0 <- rep.int(0L,ncol(x@data))
#   traces_hd$NA0 <- A$dt1hd$NA1
  traces_hd$bytes <- rep.int(2L,ncol(x@data))
  traces_hd$dz <- rep(x@dz*1000,ncol(x@data)) # time window 
  traces_hd$stacks <- rep(as.numeric(x@hd$NUMBER_OF_STACKS),ncol(x@data))
  traces_hd$NA1 <- rep.int(0L,ncol(x@data))
  traces_hd$NA2 <- traces_hd$NA1 
  traces_hd$NA3 <- traces_hd$NA1 
  traces_hd$NA4 <- traces_hd$NA1 
  traces_hd$NA5 <- traces_hd$NA1 
  traces_hd$NA6 <- traces_hd$NA1 
  if(length(x@rec) > 0 && sum(is.na(x@rec)) > 0){
    traces_hd$recx <- x@rec[,1]
    traces_hd$recy <- x@rec[,2]
    traces_hd$recz <- x@rec[,3]
  }else{
    traces_hd$recx <- rep.int(0L,ncol(x@data))
    traces_hd$recy <- rep.int(0L,ncol(x@data))
    traces_hd$recz <- rep.int(0L,ncol(x@data))
  }
  if(length(x@trans) > 0 && sum(is.na(x@trans)) > 0){
    traces_hd$transx <- x@trans[,1]
    traces_hd$transy <- x@trans[,2]
    traces_hd$transz <- x@trans[,3]
  }else{
    traces_hd$transx <- rep.int(0L,ncol(x@data))
    traces_hd$transy <- rep.int(0L,ncol(x@data))
    traces_hd$transz <- rep.int(0L,ncol(x@data))
  }
  # traces_hd$time0 <- x@time0 
  traces_hd$time0 <- 1L + round(x@time0/x@dz,2)
  traces_hd$time0 <- traces_hd$NA1 
  traces_hd$zeroflag <- rep.int(0L,ncol(x@data)) 
  traces_hd$NA7 <- traces_hd$NA1 
  aa <-as.POSIXct(x@time[1], origin = "1970-01-01")
  bb <- format(aa, format = "%Y-%m-%d")
  myDay <- as.double(as.POSIXct(as.Date(bb), origin="1970-01-01"))
  traces_hd$time <- x@time - myDay
  traces_hd$x8 <- rep.int(0L,ncol(x@data)) 
  traces_hd$x8[trimStr(x@fid)!=""] <- 1L
  traces_hd$com <- x@fid 
  
  # FILE NAMES
  dirName   <- dirname(fPath)
  splitBaseName <- unlist(strsplit(basename(fPath), '[.]'), use.names = FALSE)
  baseName   <- paste0(splitBaseName[1:(length(splitBaseName)-1)])
  if(dirName == '.'){
    fPath <- baseName
  }else{
    fPath <- paste0(dirName, '/', baseName)
  }
  # if(isTRUE(overwrite)){
    # cat("file may be overwritten\n")
  # }else{
    # fPath_orgi <- fPath
    # k <- 0
    # while(file.exists(paste(fPath,".DT1",sep="")) || 
            # file.exists(paste(fPath,".HD",sep=""))){
      # fPath <- paste(fPath_orgi,"_",k,sep="")
      # k <- k+1
    # }
  # }
  
  
  # WRITE DT1 FILE
  dt1_file <- file(paste0(fPath, ".DT1") , "wb")
  for(i in 1:ncol(x@data)){
    for(j in 1:25){
      realData4 <- traces_hd[[j]][i]
#       realData4 <- A$dt1hd[[j]][i]
      storage.mode(realData4) <- "double"
      writeBin(realData4, dt1_file, size = 4, endian = "little")
    }
    comment28 <- as.character(traces_hd$com[i])
    # nnchar <- 28-nchar(comment28)
    com_add <- paste(c(rep(" ", 28-nchar(comment28)), comment28), 
                      sep = "", collapse = "")
    writeChar(com_add, dt1_file,nchars =28,eos = NULL)
    # suppressWarnings( writeChar(comment28, dt1_file,nchars = 28,eos = NULL))
    # for(k in 1:nnchar){
      # writeChar("^@", dt1_file)
    # }
    # two-byte integers
    writeBin(traceData[,i], dt1_file, size = 2, endian = "little")
  }
  close(dt1_file)
  
  # j<-0
  
  # j<-j+1
  # traces_hd[[indexDT1Header[j]]][i]
  
  #-------------------------
  # HD FILE: traces
  hd_file <- file(paste0(fPath, ".HD") , "w+")
  writeLines("1234", con = hd_file, sep = "\r\n")
  if(!is.null(x@hd$gprdevice)){
    writeLines(as.character(x@hd$gprdevice), con = hd_file, sep = "\r\n")
  }else{
    writeLines("Data from RGPR", con = hd_file, sep = "\r\n")
  }
  writeLines(as.character(x@date), con = hd_file, sep = "\r\n")
  writeLines(paste0("NUMBER OF TRACES"," = ", as.character(ncol(x@data))), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("NUMBER OF PTS/TRC"," = ", as.character(nrow(x@data))), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("TIMEZERO AT POINT", " = ",
                   as.character(1+round(mean(x@time0)/x@dz,2))), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("TOTAL TIME WINDOW", " = ", 
                    as.character(x@dz*(nrow(x@data)))), 
             con = hd_file, sep = "\r\n")
  startpos <- 0
  if(!is.null(x@hd$startpos)){
    startpos <- x@hd$startpos
  }
  writeLines(paste0("STARTING POSITION", " = ", as.character(startpos)), 
                  con = hd_file, sep = "\r\n")
  endpos <- (ncol(x@data)-1)*x@dx
  if(!is.null(x@hd$endpos)){
    endpos <- x@hd$endpos
  }
  writeLines(paste0("FINAL POSITION"," = ", as.character(endpos)), 
              con = hd_file, sep = "\r\n")
  writeLines(paste0("STEP SIZE USED"," = ",as.character(x@dx)),
              con = hd_file, sep = "\r\n")
  writeLines(paste0("POSITION UNITS", " = ", "m"), 
              con = hd_file, sep = "\r\n")
  if(x@posunit != "m"){
    warning('Position units were defined as "metres"!\n')
  }
  writeLines(paste0("NOMINAL FREQUENCY"," = ", as.character(x@freq)), 
              con = hd_file, sep = "\r\n")
  writeLines(paste0("ANTENNA SEPARATION"," = ", as.character(x@antsep)), 
              con = hd_file, sep = "\r\n")
  pulservoltage <- 0
  if(!is.null(x@hd$PULSER_VOLTAGE_V)){
    pulservoltage <- x@hd$PULSER_VOLTAGE_V
  }
  writeLines(paste0("PULSER VOLTAGE (V)", "=", as.character(pulservoltage)), 
             con = hd_file, sep = "\r\n")
  nstacks <- 1
  if(!is.null(x@hd$NUMBER_OF_STACKS)){
    nstacks <- x@hd$NUMBER_OF_STACKS
  }
  writeLines(paste0("NUMBER OF STACKS", " = ", as.character(nstacks)), 
              con = hd_file, sep = "\r\n")
  writeLines(paste0("SURVEY MODE", " = ", as.character(x@surveymode)), 
              con = hd_file, sep = "\r\n")
  
  if(length(x@hd) > 0){
    hdNames <- names(x@hd)
    hdNames <- hdNames[!(hdNames %in% c("startpos", "endpos",
                                        "NUMBER_OF_STACKS",
                                        "PULSER_VOLTAGE_V", "gprdevice"))]
    for(i in seq_along(hdNames)){
      hdName <- gsub("_", replacement = " ", as.character(hdNames[i]))
      hdName <- gsub("Serial", replacement = "Serial#", hdName)
      hdName <- gsub("CAL tm", replacement = "CAL (t/m)", hdName)
      
writeLines(paste0(as.character(hdName), " = ", 
                  as.character(x@hd[[hdNames[i]]])), 
           con = hd_file, sep = "\r\n")
    }
  }
  close(hd_file)
  return(fPath)
}

