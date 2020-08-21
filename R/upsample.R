setGenericVerif("upsample", function(x,n) standardGeneric("upsample"))

#---------------------- INTERPOLATION ---------------------#  
#' Up-sample the GPR data (1D and 2D sinc-interpolation)
#'
#' When upsampling is performed on a GPR data, it produces an approximation of 
#' the data that would have been collected by sampling them at a higher rate in
#' time/depth domain and/or in spatial domain (trace sampling). 
#' 
#' @param x [\code{GPR class}]
#' @param n [\code{integer(1|2)}] Two positive integer values. The GPR data are
#'          upsampled in the time/depth domain by a factor of \code{n[1]} and 
#'          in the spatial domain (trace sampling) by a factor of \code{n[2]}. 
#'          If only one value is passed, then \code{n[2] <- n[1]}.
#' @name upsample
#' @rdname upsample
#' @export
setMethod("upsample", "GPR", function(x,n){
  n <- abs(round(n))
  if(length(n) == 1){
    n <- rep(n, 2)
  }
  x@data <- .upsample(x@data, n = n, type = c("DFT"))
  x@data <- x@data[, 1:(ncol(x@data))]
  yvalues <- (seq(0, by=x@dz,length.out=nrow(x@data)))
  
  xvalues  <- .doubleVector(x@pos, n = n[2])
  yvalues  <- .doubleVector(x@depth, n = n[1])
  #  
  # image(xvalues,yvalues,t(x@data))
  
  ntr <- ncol(x@data)  # number of traces
  if(ntr != length(xvalues)) stop("ntr!=length(xvalues)")
  if(length(x@coord)>0){
    coord_new <- matrix(ncol=3,nrow=ntr)
    coord_new[,1] <- signal::interp1(x@pos, x@coord[,1], xi = xvalues,   
                                     method = c( "linear"), extrap = TRUE)
    coord_new[,2] <- signal::interp1(x@pos, x@coord[,2], xi = xvalues,   
                                     method = c( "linear"), extrap = TRUE)
    coord_new[,3] <- signal::interp1(x@pos, x@coord[,3], xi = xvalues,   
                                     method = c( "linear"), extrap = TRUE)
    x@coord <- coord_new
  }
  if(length(x@rec)>0){
    rec_new <- matrix(ncol=3,nrow=ntr)
    rec_new[,1] <- signal::interp1(x@pos, x@rec[,1], xi = xvalues,   
                                   method = c( "linear"), extrap = TRUE)
    rec_new[,2] <- signal::interp1(x@pos, x@rec[,2], xi = xvalues,
                                   method = c( "linear"), extrap = TRUE)
    rec_new[,3] <- signal::interp1(x@pos, x@rec[,3], xi = xvalues,   
                                   method = c( "linear"), extrap = TRUE)
    x@rec <- rec_new
  }
  if(length(x@trans)>0){
    trans_new <- matrix(ncol=3,nrow=ntr)
    trans_new[,1] <- signal::interp1(x@pos, x@trans[,1], xi = xvalues,   
                                     method = c( "linear"), extrap = TRUE)
    trans_new[,2] <- signal::interp1(x@pos, x@trans[,2], xi = xvalues,   
                                     method = c( "linear"), extrap = TRUE)
    trans_new[,3] <- signal::interp1(x@pos, x@trans[,3], xi = xvalues,   
                                     method = c( "linear"), extrap = TRUE)
    x@trans <- trans_new
  }
  
  x@traces <- seq.int(1L,by=1L,length.out=ntr)
  #fiducial markers (fid, comments)
  if(length(x@fid) >0){ #&& sum(x@fid != "")>0){
    newfid <- character(length(x@fid)*n[2])
    newfidPos <- which(x@fid!="")
    newfid[newfidPos*n[2]] <- x@fid[newfidPos]
    x@fid <- newfid[1:ntr]
  }
  #annotations
  if(length(x@ann) >0){ # && sum(x@ann != "")>0){
    newAnn <- character(length(x@ann)*n[2])
    newAnnPos <- which(x@ann!="")
    newAnn[newAnnPos*n[2]] <- x@ann[newAnnPos]
    x@ann <- newAnn[1:ntr]
  }
  # trace positions
  x@pos <- xvalues
  # depth/time
  x@depth <- .doubleVector(x@depth, n = n[1])
  
  x@dx <- x@dx / n[2]
  x@dz <- x@dz / n[1]
  #     x@ntr <- ntr
  
  proc <- getArgs()
  x@proc <- c(x@proc, proc)
  return(x)
} 
)




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


