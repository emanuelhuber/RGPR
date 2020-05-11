# 
# #------------------------------------------------------------------------------#
# 
# # identify 2D peaks:
# # - https://rdrr.io/cran/geodiv/src/R/localsurface.R
# # - https://www.sthu.org/blog/13-perstopology-peakdetection/index.html
# 
# 
# 
# 
# 
# #-------------------------------------------------------------------------
# # Simulate CMP
# 
# TWThyperbolic <- function(xt, v, a){
#   sqrt(xt^2 + (a/v)^2)
# }
# 
# 
# v <- 0.1
# a <- seq(0, to = 20, by = 0.25)
# plot(a, TWThyperbolic(0,  v, a), type = "l", ylim = rev(range(xt)))
# lines(a, TWThyperbolic(10, v = 0.1, a))
# lines(a, TWThyperbolic(40, a, v = 0.08))
# lines(a, TWThyperbolic(90, a, v = 0.105))
# lines(a, TWThyperbolic(120, a, v = 0.085))
# lines(a, TWThyperbolic(170, a, v = 0.095))
# 
# xt <- c(0,     30,   70, 110,  145,   170,   200)
# v <- c(0.1, 0.095, 0.08, 0.09, 0.105, 0.09, 0.095)
# d <- c(0.5,   2.2,  2.9,  4.4,     6,  7.6, 9)
# 
# U <- mapply(TWThyperbolic, xt, v, MoreArgs = list(a = a))
# 
# dim(U)
# plot3D::image2D(U)
# 
# 
# # FIXME: layer depth + internal velocity
# 
# TWThyperbolic_d <- function(a, t0, v){
#   sqrt(t0^2 + (a/v)^2)
# }
# 
# lyr <- list(vint = c(0.1, 0.095, 0.08, 0.09, 0.105, 0.09, 0.095),
#             d    = c(0.5,   1.2,  1.9,  2.4,     3,  4.6, 5),
#             vrms = sqrt(cumsum(vint * 2 * d)/(2 * cumsum(d/vint))),
#             t0  = 2 * cumsum(d/vint) )
# a <- seq(0, to = 20, by = 0.25)
# # vint <- c(0.1, 0.095, 0.08, 0.09, 0.105, 0.09, 0.095)
# # d <- c(0.5,   1.2,  1.9,  2.4,     3,  4.6, 5)
# # 
# # vrms <- sqrt(cumsum(vint * 2 * d)/(2 * cumsum(d/vint)))
# # t0 <- 2 * cumsum(d/vint)
# 
# U <- t(sapply(a, TWThyperbolic_d, t0 = lyr$t0, v = lyr$vrms))
# 
# dim(U)
# plot3D::image2D(U)
# 
# # GPR wavelet:
# #   Check chap 11 of Anan in the book Near-Surface Geophysics
# # "The radiated wavelet from a GPR system is a compli-
# # cated function of the antenna construction and the electronics 
# # drive circuitry.For impulse style ultra wideband systems using 
# # short electric dipole antennas, a simple mathematical model is 
# # helpful for numerical simulation."
# 
# vt <- function(xt, xT){
#   test <- xt > 0 & xt < xT
#   vt <- numeric(length(xt))
#   vt[test] <-  0.5 * ( 1 + cos(pi * (xt[test]- xT/2)/(xT/2)))
#   return(vt)
# }
# 
# # xt = time in ns
# # where 0 < q < 1 (damping factor)
# # fc = center frequency in MHz
# wavGPR <- function(xt, q, fc){
#   xt <- xt / 1000
#   xT <- (2/3 + (1-q)/7)/fc
#   vt(xt, xT) - (2 - q)* vt(xt - xT/2, xT) + (1 - q)*vt(xt-xT, xT)
# }
# 
# dz <- 0.25
# fc <- 100 # MHz 
# wt <- seq(0, by = dz, to = 15) # ns 
# 
# plot(wt, wavGPR(wt, q = 0.9, fc = fc), type = "l")
# 
# w <- wavGPR(wt, q = 0.9, fc = fc)
# 
# plot(wt, w, type = "l")
# 
# 
# zt <- seq(0, to = 250, by = dz)
# X <- matrix(0, nrow = length(zt), ncol = length(a))
# 
# 
# for(i in 1:ncol(U)){
#   for(j in 1:nrow(U)){
#     uij <- U[j, i]
#     k <- round(uij/dz) + wt/dz +1
#     k <- k[k > 0 & k < nrow(X)]
#     X[k,j] <- X[k,j]  + w[seq_along(k)]
#   }
# }
# 
# plot3D::image2D(X)
# 
# X_list <- list(data = X, x = a, z = zt, mode = "CMP", freq = 100)
# x <- as(X_list, "GPR")
# plot(x)
# plot(x[,1])
# 
# x
# 
# #------------------------------------------------------------------------------#
# 
# dsn <- "/mnt/data/RGPR/CODE/DEVELOPMENT/CMP/cmp.DT1"
# dsn <- "/mnt/data/RGPR/CODE/DEVELOPMENT/CMP/2018_10_17-20_cimano_grid02_LINE00.DT1"
# 
# # Trace no. 13 = doublons
# dsn <- "/mnt/data/RGPR/CODE/DEVELOPMENT/CMP/2018_10_17-20_cimano_grid02_LINE01.DT1"
# 
# x <- readGPR(dsn)
# 
# plot(x)
# #--- correction
# plot(x[, -13])
# x_pos <- xpos(x)
# x <- x[, -13]
# xpos(x) <- x_pos[1:ncol(x)]
# plot(x)
# #--------------
# 
# plot(x)
# 
# plot(x)
# plot(x, xlim = c(-1, 10))
# plot(x[,1])
# 
# x@x
# x@antsep
# 
# 
# x_NMO <- correctNMO(x) # ERROR
# 
# x@z0 <- rep(17.5, ncol(x))
# 
# x1 <- shiftToTime0(x)
# 
# plot(x1)
# 
# x_NMO0 <- correctNMO(x1)
# x_NMO1 <- correctNMO(x1, v = 0.05, method = "pchip")
# x_NMO2 <- correctNMO(x1, v = 0.05, method = "linear")
# plot(x_NMO1 - x_NMO2)
# plot(x_NMO0)
# plot(x_NMO1)
# plot(x_NMO2)
# 
# diff(x_NMO2@z)
# 
# plotTr(x1[,1:5])
# 
# 
# # COMPUTE THE Normal Move-Out
# D_NMO <- NMO(x1)
# x1@vel[[1]]
# D_NMO <- NMO(x1, v = 0.8)
# plot(D_NMO)
# 
# # NMO streching = D_NMO / t(x = 0)
# # S_NMO <- D_NMO / x1@z
# # S_NMO@data[is.infinite(S_NMO@data)] <- 0
# 
# S_NMO <- NMOstreching(x1, v = 0.061)
# S_NMO <- NMOstreching(x1)
# plot(S_NMO)
# plot(S_NMO > 0.15)
# 
# plot(log(S_NMO), col = palGPR("slice"))
# 
# 
# plot(NMO(x1, v = 0.8))
# NMO(x1, v = 0.8)@data[1:5,1:10]
# 
# x <- shiftToTime0(x)
# 
# unnorm_ccor <- function(x){
#   test <-  abs(sum( rowSums(x, na.rm = TRUE)^2 -  rowSums(x^2, na.rm = TRUE)))
#   return(test) #(sum(x^2, na.rm = TRUE)) / (ncol(x) * (ncol(x) - 1))
# }
# v = seq(0.02, to = 0.11, length.out = 50)
# i <- 1
# x_tv <- velocitySpectrum(x, v = v)
# 
# for(i in seq_along(v)){
#   y <- .NMOCor(x, v = v[i], asep = x@antsep)
#   wi <- 5
#   x_tv@data[,i] <- wapplyRowC(y@data, width = wi, by = 1,
#                               FUN = unnorm_ccor)
# }
# x_tv <- x_tv/max(x_tv)
# 
# 
# plot(x_tv, col = palGPR("slice"))
# 
# max(x_tv)
# 
# 
# for(i in seq_along(v)){
#   y <- .NMOCor(x, v = v[i], asep = x@antsep)
#   SS <- semblance(y@data)/ncol(x)
#   x_tv@data[,i] <- wapplyC(SS, width = wi, by = 1,
#                               FUN = min, na.rm = TRUE)
# }
# 
# plot(x_tv)
# x_tv@data[is.na(x_tv@data)] <- 0
# x_tv@data[is.infinite(x_tv@data)] <- 0
# 
# 
# x_tv <- velocitySpectrum(x)
# x_tv <- velocitySpectrum(x[,1:10], v = seq(0.02, to = 0.15, length.out = 50))
# plot(x_tv, clip = NULL)
# plot(x)
# x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50),
#                          method = "semblance")
# plot(x_tv)
# 
# x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50),
#                          method = "winsemblance", w = 2)
# plot(x_tv)
# 
# x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50),
#                          method = "minsemblance", w = 1)
# plot(x_tv)
# 
# x_tv <- velocitySpectrum(x[,1:10], v = seq(0.02, to = 0.11, length.out = 50),
#                          method = "wincoherence", w = 2)
# plot(x_tv)
# 
# 
# x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50),
#                          method = "wincoherence2", w = 10)
# plot(x_tv)
# 
# 
# #------------------------------------------------------------------------------#
# # APPLY FIRST MUTING BEFORE VELOCITY SPECTRUM
# x0 <- x
# x0_S <- NMOstreching(x0)
# x0[x0_S@data > 0.2] <- 0
# plot(x0)
# 
# par(mfrow = c(1, 2))
# x0_tv <- velocitySpectrum(x0, v = seq(0.02, to = 0.11, length.out = 50))
# plot(x0_tv, clip = NULL)
# 
# sel <- 1:20
# x0_tv <- velocitySpectrum(x0[,sel], v = seq(0.02, to = 0.11, length.out = 50))
# plot(x0_tv, clip = NULL)
# 
# x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50))
# plot(x_tv, clip = NULL)
# 
# x_tv <- velocitySpectrum(x[, sel], v = seq(0.02, to = 0.11, length.out = 50))
# plot(x_tv, clip = NULL)
# 
# plot(x_tv - x0_tv, clip = NULL)
# 
# #------------------------------------------------------------------------------#
# 
# sel <- 1:20
# 
# x_tv <- velocitySpectrum(x[,sel], v = seq(0.02, to = 0.11, length.out = 50),
#                          method = "semblance")
# plot(x_tv)
# plot(x[,sel])
# # antsep(x) <- seq(0.6, by = 0.5, length.out = ncol(x))
# # vlim <- vel(x) * c(0.65, 1.3)
# # x_tv <- CMPAnalysis(x[, 1:20], method = "winsemblance",
#                     # v =  seq(vlim[1], vlim[2], length = 20),
#                     # w = 7)
# 
# par(mfrow = c(1,2))
# plot(x[,1:25], barscale = FALSE, main = "CMP")
# plot(x_tv[1:nrow(x_tv),], barscale = FALSE,
#      main = "semblance analysis")
# contour(x_tv[1:nrow(x_tv),], add = TRUE, nlevels = 5)
# grid()
# 
# # Application of this formula can provide non-real velocities, if the travel time intervals are small or
# # if the NMO velocity change is large. Such problems were not encountered in our case.
# vv <- locator(type = "o", pch = 20, col = "green")
# 
# #--- NMO-velocities
# # vnmo <- c(vv$x[1], vv$x)
# vnmo <-  vv$x
# tnmo <-  vv$y
# v <- approxfun(tnmo, vnmo, rule = 2, method = "linear", f = 0)
# # v <- approxfun(vnmo, tnmo, rule = 2, method = "constant")
# # plot(v(x@z), x@z, type = "l", ylim = rev(range(x@z)))
# # points(vnmo, tnmo, pch = 20)
# 
# lines(v(x_tv@z), x_tv@z, lwd = 2, col = "green")
# points(vv, pch = 20)
# 
# 
# vrms <- approxfun(tnmo, vnmo, rule = 2, method = "constant", f = 1)
# 
# #--- internal velocities
# intVel <- function(tx, vx){
#   
#   # vint <- numeric(length(vx))
#   # vint[1] <- vx[1]
#   # vint[2:length(vx)] <-
#   if(tx[1] != 0){
#     tx <- c(0, tx)
#   }
#   if(vx[1] != 0){
#     vx <- c(0, vx)
#   }
#   tx_vx2 <- tx * vx^2
#   vint <- sqrt(diff(tx_vx2)/diff(tx))
#   # vint[1:(length(vx) - 1)] <- sqrt(diff(tx_vx2)/diff(tx))
#   # vint[length(vx)] <-  vint[length(vx) - 1]
#   # for( i in 2:length(vx) ){
#   #   # vint[i] <- suppressWarnings(sqrt( (vv$y[i] * vx[i]^2 - vv$y[i-1] * vx[i-1]^2) /
#   #   vint[i] <- (sqrt( (vv$y[i] * vx[i]^2 - vv$y[i-1] * vx[i-1]^2) /
#   #                       (tx[i] - tx[i-1]) ))
#   # }
#   sel <- is.na(vint)
#   if(sum(sel) > 0){
#     # vint[sel] <- approx( x = tx[!sel], y = vint[!sel], xout = tx[sel],
#     #                      rule = 2)$y
#     message(rep("*", sum(sel)))
#   }
#   # tx <- rep(tx, each = 2)
#   # tx <- c(0, tx[-length(tx)])
#   # vint <- rep(vint, each = 2)
#   
#   return(list(t = tx[-1], vel = vint))
# }
# 
# vin <- intVel(tx = vv$y, vx = vv$x)
# 
# vint <- approxfun(vin$t, vin$vel, rule = 2, method = "constant", f = 1)
# 
# # NMO correction with variable 1D velocity
# plot(vrms(x_tv@z), x_tv@z, type = "o", ylim = rev(range(x_tv@z)), pch = 20, xlim = c(0.02, 0.12))
# lines(vint(x_tv@z), x_tv@z, type = "o", ylim = rev(range(x_tv@z)), pch = 20, col = "red")
# x@vel <- list(vrms(x_tv@z))  # use v_rms for NMO correction but v_int for time to depth correction!
# # x@vel <- list(vint(x_tv@z))
# 
# #--- plot CMP with hyperbola & NMO correction
# par(mfrow = c(1,2))
# plot(x[,sel], barscale = FALSE, main = "CMP")
# .t_NMO <- function(t0, v, antsep){
#   sqrt(t0^2 + (antsep/v)^2)
# }
# TT <- mapply(.t_NMO, vv$y, vv$x, MoreArgs = list(antsep = x@x))
# for(i in seq_along(vv$y)){
#   lines(x@x, TT[,i])
# }
# plot(correctNMO(x[,sel]))
# 
# dim(correctNMO(x[,sel]))
# 
# #--- plot NMO correction & trace stacking
# par(mfrow = c(1,2))
# plot(correctNMO(x[,sel]), type = "wiggles", wsize = 5)
# plot(rowSums(correctNMO(x[,sel])@data), x@z, ylim = rev(range(x@z)), type = "l" )
# 
# 
# 
# dim(x)
# dim(NMOstreching(x))
# plot(NMOstreching(x))
# plot(NMOstreching(x) < 0.2)
# dim(NMOstreching(x)@data < 0.2)
# xnew <- x
# xnew[NMOstreching(xnew)@data > 0.25] <- 0
# plot(correctNMO(xnew))
# 
# 
# #----------------------
# #--- Vrms vs Vint as a function of two-way travel time & depth
# 
# DT <- diff(c(0, vin$t/2))
# zv <- cumsum(DT * vin$vel)
# 
# DT <- diff(c(0, x@z)/2)
# zv <- cumsum(DT * vint(x@z))
# 
# plot(zv, x@z, xlab = "depth", ylab = "two-way travel time", type = "l")
#   
# 
# plot(vrms(x_tv@z), x_tv@z,  ylim = rev(range(x_tv@z)), type = "l", xlim = c(0.0, 0.2),
#      lwd = 2, lty = 1, xlab = "velocity (m/ns)", ylab = "two-way travel time (ns)")
# lines(vint(x_tv@z), x_tv@z, col = "black", lwd = 2, lty = 3)
# 
# 
# plot(vrms(x_tv@z), zv,  ylim = rev(c(0, max(zv))), type = "l", xlim = c(0.00, 0.2),
#      xlab = "velocity (m/ns)", ylab = "depth (m)", lwd = 2, lty = 1)
# lines(vint(x@z), zv, lty = 3, lwd = 2)
# 
# 
# 
# 
# par(mfrow = c(1,2))
# plot(x[,1:25], barscale = FALSE, main = "CMP")
# plot(x_tv[1:nrow(x_tv),], barscale = FALSE,
#      main = "semblance analysis")
# contour(x_tv[1:nrow(x_tv),], add = TRUE, nlevels = 5)
# grid()
# lines(v(x_tv@z), x_tv@z, lwd = 2, col = "green")
# points(vv, pch = 20)
# 
# lines(vint(x@z), x@z,  lwd = 2, lty = 1)
# 
# 
# par(mfrow = c(1,2))
# plot(x[,1:25], barscale = FALSE, main = "CMP")
# .t_NMO <- function(t0, antsep, v){
#   sqrt(t0^2 + (antsep/v)^2)
# }
# 
# t0 <- vv$y
# TT <- matrix(nrow = length(vv$y), ncol = length(x@x))
# for(i in seq_along(vv$y)){
#   TT[i,] <- .t_NMO(vv$y[i], x@x, vv$x[i])
#   lines(x@x, TT[i,])
# }
# plot(x_tv[1:nrow(x_tv),], barscale = FALSE,
#      main = "semblance analysis")
# contour(x_tv[1:nrow(x_tv),], add = TRUE, nlevels = 5)
# grid()
# lines(v(x_tv@z), x_tv@z, lwd = 2, col = "green")
# points(vv, pch = 20)
# 
# lines(vint(x@z), x@z,  lwd = 2, lty = 1)
# 
# 
# ## TEST WITH DATA FROM ANNAN 2005
# v_nmo <- c(0, 0.095, 0.098, 0.105) # = v_rms
# t_nmo <- c(0, 40, 50, 80)
# 
# t_v2 <- t_nmo * v_nmo^2
# v_int <- sqrt(diff(t_v2)/diff(t_nmo))
# h <- v_int * diff(t_nmo)/2
# d <- cumsum(h)
# 
# round(cbind(t_nmo, v_nmo, c(0, h), c(0, d), c(0, v_int)), 3)
# 
# 
# 
# 
# 
