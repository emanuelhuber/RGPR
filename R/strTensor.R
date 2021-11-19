#---------------------- STRUCTURE TENSOR ---------------------#

#' Structure tensor field of GPR data 
#'
#' @name strTensor
#' @rdname strTensor
#' @export
setGenericVerif("strTensor", function(x,  blksze = c(2, 4),
                                      kBlur   = list(n = 1, m = 1, sd = 1), 
                                      kEdge   = list(n = 5, m = 5, sd = 1), 
                                      kTensor = list(n = 5, m = 5, sd = 1),
                                      thresh = 0.02, 
                                      what = c("tensor", "mask", 
                                               "orientation", "values"), ...)
  standardGeneric("strTensor"))



setMethod("strTensor", "GPR", function(x,  blksze = c(2, 4),
                                       kBlur   = list(n = 1, m = 1, sd = 1), 
                                       kEdge   = list(n = 5, m = 5, sd = 1), 
                                       kTensor = list(n = 5, m = 5, sd = 1),
                                       thresh = 0.02, 
                                       what = c("tensor", "mask", 
                                                "orientation", "values"), ...){
  O <- .strucTensor(P = x@data, dxy = c(x@dx, x@dz), 
                    blksze = blksze,
                    kBlur   = kBlur, 
                    kEdge   = kEdge, 
                    kTensor = kTensor,
                    thresh = thresh)  
  output <- list()
  whatref <- c("tensor", "mask", "orientation")
  what <- what[what %in% whatref]
  if(length(what) == 0){
    stop(paste0("argument 'what' only accepts a character vector composed of",
                " at least one of the following words:\n",
                "'tensor', 'mask', 'orientation'"))
  }
  if( "orientation" %in% what){ 
    xOrient <- x
    xAni    <- x
    xEnergy <- x
    xOrient@data       <- O$polar$orientation
    xOrient@surveymode <- "orientation"
    xEnergy@data       <- O$polar$energy
    xEnergy@surveymode <- "energy"
    xAni@data          <- O$polar$anisotropy
    xAni@surveymode    <- "anisotropy"
    output[["orientation"]] <- list("energy"      = xEnergy,
                                    "anisotropy"  = xAni,
                                    "orientation" = xOrient)
  }
  if( "tensor" %in% what){
    xJxx <- x
    xJyy <- x
    xJxy <- x
    xJxx@data <- O$tensor$xx
    xJyy@data <- O$tensor$yy
    xJxy@data <- O$tensor$xy
    xJxx@surveymode <- "tensorxx"
    xJyy@surveymode <- "tensoryy"
    xJxy@surveymode <- "tensorxy"
    output[["tensor"]] <- list("xx" = xJxx,
                               "yy" = xJyy,
                               "xy" = xJxy)
  }
  if( "mask" %in% what){
    mask <- x
    mask@data <- O$mask
    mask@surveymode <- "mask"
    output[["mask"]] <- mask
  }
  return(output)
}
)


#' @export                      
setMethod("strTensor", "matrix", 
          function(x, dxy = c(1, 1), mask = c(2, 2),
                   kBlur   = list(n = 3, m =  3, sd = 1), 
                   kEdge   = list(n = 7, m =  7, sd = 1), 
                   kTensor = list(n = 5, m = 10, sd = 2),
                   thresh=0.1, what = c("tensor", "mask"), ...){
            y <- .strucTensor(x, dxy = dxy, mask = mask,
                              kBlur   = kBlur, 
                              kEdge   = kEdge, 
                              kTensor = kTensor,
                              thresh=thresh, what = what, ...)
            return(y)
          })





# return inv(A) %*% B.
# assume that
#      | a1  c1 |
#  A = |        |
#      | c1  b1 |
#
#      | a2  c2 |
#  B = |        |
#      | c2  b2 |

#' @export
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

#' Eigendecomposition of 2x2 matrices
#'
#' @name eigenDecomp2x2SymMatrix 
#' @export
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

#' @export
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

#' @export
matPow <- function(a, b, d, n){
  eg <- eigenDecomp2x2SymMatrix(a, b, d)
  l1 <- eg$l1^n
  l2 <- eg$l2^n
  return(list(a11 = eg$u1x^2 * l1 + eg$u2x^2 * l2,
              a22 = eg$u1y^2 * l1 + eg$u2y^2 * l2,
              a12 = eg$u1x*eg$u1y * l1 + eg$u2x*eg$u2y * l2,
              a21 = eg$u1y*eg$u1x * l1 + eg$u2y*eg$u2x * l2))
}

#' @export
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

#' @export
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

#' @export
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

#' @export
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

#' structure tensor for matrices
#'
#' @name strucTensor
#' @export
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
      mseq <- c(seq(1, m, mask[2]),m)
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
      # P <- (P - mean(P[!mask], na.rm = TRUE))/
      #        sd(as.vector(P[!mask]), na.rm = TRUE)
    }
  }else{
    mask <- matrix(FALSE, nrow = n, ncol = m)
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
  
  # local TENSOR
  Gxx <- vx^2
  Gyy <- vy^2
  Gxy <- vx*vy 
  
  # LOCAL AVERAGED TENSOR
  kg <- do.call(gkernel, kTensor)
  Jxx  <- convolution2D(Gxx, kg)
  Jyy  <- convolution2D(Gyy, kg)
  Jxy  <- convolution2D(Gxy, kg)
  Jxx[Jxx < .Machine$double.eps^0.75] <- 0
  Jyy[Jyy < .Machine$double.eps^0.75] <- 0
  
  
  # polar parametrisation
  energy <- Jxx + Jyy                               # energy
  anisot  <- sqrt((Jxx-Jyy)^2 + 4*(Jxy)^2)/energy   # anisotropy
  orient <- 1/2*atan2(2*Jxy, (Jxx - Jyy) ) + pi/2   # orientation
  mask2 <- mask | is.infinite(energy) | is.infinite(anisot)
  anisot[mask2] <- 0
  energy[mask2] <- 0
  orient[mask2] <- 0
  
  # ANALYTIC SOLUTION BASED ON SVD DECOMPOSITION
  Jeig <- eigenDecomp2x2SymMatrix(Jxx, Jyy, Jxy)
  Jeig$u1x[mask2] <- 0
  Jeig$u1y[mask2] <- 0
  Jeig$u2x[mask2] <- 0
  Jeig$u2y[mask2] <- 0
  Jeig$l1[mask2] <- 0
  Jeig$l2[mask2] <- 0
  mode(mask2) <- "integer"
  
  # APPLY MASK TO local tensor and structure tensor
  Gxx[mask2] <- 0
  Gyy[mask2] <- 0
  Gxy[mask2] <- 0 
  Jxx[mask2] <- 0
  Jyy[mask2] <- 0
  Jxy[mask2] <- 0
  
  return(list(tensor  = list("xx" = Jxx,
                             "yy" = Jyy,
                             "xy" = Jxy),
              gradtens = list("xx" = Gxx,
                              "yy" = Gyy,
                              "xy" = Gxy),
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
  v_y = seq(1,(m),by=spacing[1])
  v_x = seq(1,(n), by=spacing[2])
  
  # Determine placement of orientation vectors
  if(length(x@coord)>0){
    xvalues <- posLine(x@coord)
  }else{
    xvalues <- x@pos
  }
  X = matrix(xvalues[v_y],nrow=length(v_x),ncol=length(v_y),byrow=TRUE)
  Y = matrix(x@depth[v_x],nrow=length(v_x),ncol=length(v_y),byrow=FALSE)
  
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
    for(i in seq_along(v_x)){
      for(j in seq_along(v_y)){
        aij <- ifelse(is.infinite(a[i,j]), 0, a[i,j])
        bij <- ifelse(is.infinite(b[i,j]), 0, b[i,j])
        maxab <- 1
        if(isTRUE(normalise)){
          maxab <- max(aij, bij)
        }
        if(!(aij == 0 & bij == 0)){
          E <- RConics::ellipse(saxes = c(aij/maxab, bij*ratio/maxab)*len1,
                                loc   = c(X[i,j], Y[i,j]), 
                                theta = angle[i,j], n=n)
          polygon(E, ...)
        }
      }
    }
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
          E <- RConics::ellipse(saxes = c(aij, bij*ratio)*len1,
                                #           /normdxdy[i,j], 
                                loc   = c(X[i,j], Y[i,j]), 
                                theta = pi/2 - angle[i,j], n=n)
          polygon(E, col=col[lcol[i,j]])
        }
      }
    }
  }
}
