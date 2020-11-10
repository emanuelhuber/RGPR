
# ---------------------------------------------------------------------------- #
# -------------------------- TODO -------------------------------------------- 
# ---------------------------------------------------------------------------- #

# For every function with @vel, check
# - if length(vel) > 0
# - if isZDepth/isZTime
# - use .getVel to get velocity "v"
# - check if 1 numeric value, 1 vector, 1 matrix or ???



#------------------------------------------------------------------------------#

# identify 2D peaks:
# - https://rdrr.io/cran/geodiv/src/R/localsurface.R
# - https://www.sthu.org/blog/13-perstopology-peakdetection/index.html


# vel <- function(x){
#   return(x@vel)
# }


# stackNMO <- function(x, thrs = NULL){
#   x_NMOcor <- correctNMO(x, thrs = thrs)
#   x <- rowMeans(x_NMOcor, na.rm = TRUE)    
#   return(x)
#   
# }


# 
# setVel <- function(x, v, twt, type = c("vrms", "vint")){
#   type <- match.arg(type, c("vrms", "vint"))
#   if(length(v) != length(twt)){
#     stop("'v' and 'twt' must have the same length!")
#   }
#   i <- order(twt)
#   v <- v[i]
#   twt <- twt[i]
#   if(type == "vrms"){
#     v <- list("vrms" = list("t"    = twt,
#                             "v"    = v,
#                             "intp" = "stairs"),
#               "vint" = c(dixVel(twt = twt, v = v),
#                          "intp" = "stairs"))
#   }else{
#     v <- list("vint" = list("t"    = twt, 
#                             "v"    = v,
#                             "intp" = "stairs"),
#               )
#   }
#   x@vel <- v
#   return(x)
# }
# 


# interpVel <- function(x, 
#                       type = c("vrms", "vint"),
#                       method = c("stairs", "linear", "nearest", 
#                                  "pchip", "cubic", "spline")){
#   type <- match.arg(type, c("vrms", "vint"))
#   method <- match.arg(method, c("stairs", "linear", "nearest", "pchip", "cubic", "spline"))
#   x@vel[[type]][["intp"]] <- method
#   return(x)
# }

# smoothVel <- function(x, w, type = c("vrms", "vint")){
#   type <- match.arg(type, c("vrms", "vint"))
#   # v_stairs <- approxfun(x@vel[[type]][["t"]], x@vel[[type]][["v"]], 
#                         # rule = 2, method = "constant", f = 1)
#   # mmand::gaussianSmooth(v_stairs(x@z), sigma = w)
#   x@vel[[type]][["smooth"]] <- w
#   return(x)
# }



# 
# .intpSmoothVel <- function(x_vel_i, x_z){
#   if(is.list(x_vel_i) && !is.null(x_vel_i$intp)){
#     if(x_vel_i$intp == "stairs"){
#       v_stairs <- approxfun(x_vel_i[["t"]], x_vel_i[["v"]], 
#                             rule = 2, method = "constant", f = 1)
#       x_vel_i[["v"]] <- v_stairs(x_z)
#       x_vel_i[["t"]] <- x_z
#       
#     }else{
#       print(x_vel_i$intp)
#       x_vel_i[["v"]]  <- signal::interp1(x = x_vel_i[["t"]], y = x_vel_i[["v"]],
#                                             xi = x_z, method = x_vel_i$intp,
#                                             extrap = TRUE)
#       x_vel_i[["t"]] <- x_z
#       x_vel_i[["intp"]] <- "stairs"
#     }
#     if(!is.null(x_vel_i$smooth) && x_vel_i$smooth > 0){
#       x_vel_i[["v"]]  <- mmand::gaussianSmooth(x_vel_i[["v"]], sigma = x_vel_i$smooth)
#     }
#   }
#   return(x_vel_i)
# }
# 
# 
# .intpSmoothAllVel <- function(x_vel, x_z){
#   if(length(x_vel) > 0){
#     # interpolate velocities
#     for(i in seq_along(x_vel)){
#       # vi <- x_vel[[i]]
#       x_vel[[i]] <- .intpSmoothVel(x_vel[[i]], x_z)
#       
#     }
#   }
#   return(x_vel)
# }

# plotVel <- function(x){
#   if(length(x@vel) > 0){
#     x@vel <- .intpSmoothAllVel(x@vel, x@z)
#     v_lim <- range(sapply(x@vel, .getAllVel))
#     plot(0, type = "n", ylim = rev(range(x_tv@z)),  xlim = v_lim, yaxs = "i",
#          xlab = RGPR:::.vlab(x),
#          ylab = RGPR:::.zlab(x))
#     if(!is.null(x@vel[["vrms"]])){
#       # v_rms <- approxfun(x@vel[["vrms"]][["t"]], x@vel[["vrms"]][["v"]], rule = 2, method = "constant", f = 1)
#       lines(x@vel[["vrms"]][["v"]], x@vel[["vrms"]][["t"]], type = "s", lty = 1)
#     }
#     if(!is.null(x@vel[["vint"]])){
#       # v_int <- approxfun(x@vel[["vint"]][["t"]], x@vel[["vint"]][["v"]], rule = 2, method = "constant", f = 1)
#       # lines(v_int(x_z), x_z, type = "s", lty = 3)
#       lines(x@vel[["vint"]][["v"]], x@vel[["vint"]][["t"]], type = "s", lty = 3)
#     }
#     if(!is.null(x@vel[["v"]]) && is.numeric(x@vel[["v"]])){
#       lines(x@vel[["v"]], x_z, type = "s", lty = 2, col = "red")
#     }
#   }else{
#     if(isZDepth(x)){
#       stop(msg_set_zunitToDepth)
#     }else{
#       stop("")
#     }
#   }
# }
# 
# .getAllVel <- function(x){
#   if(inherits(x, "list") && !is.null(x[["v"]])){
#     return(x[["v"]])
#   }else if(is.numeric(x)){
#     return(x)
#   }
# }
# plotVel <- function(x){
#   if(length(x@vel)){
#     # interpolate velocities
#     for(i in seq_along(x@vel)){
#       vi <- x@vel[[i]]
#       if()
#     }
#     if(method == "stairs"){
#       v_stairs <- approxfun(x@vel[[type]][["t"]], x@vel[[type]][["v"]], 
#                             rule = 2, method = "constant", f = 1)
#       x@vel[["v"]] <- v_stairs(x@z)
#     }else{
#       x@vel[["v"]] <- signal::interp1(x = x@vel[[type]][["t"]], y = x@vel[[type]][["v"]],
#                                       xi = x@z, method = method,
#                                       extrap = TRUE)
#     }
#     
#     v_lim <- range(sapply(x@vel, .getAllVel))
#     plot(0, type = "n", ylim = rev(range(x_tv@z)),  xlim = v_lim, yaxs = "i",
#          xlab = RGPR:::.vlab(x),
#          ylab = RGPR:::.zlab(x))
#     if(!is.null(x@vel[["vrms"]])){
#       v_rms <- approxfun(x@vel[["vrms"]][["t"]], x@vel[["vrms"]][["v"]], rule = 2, method = "constant", f = 1)
#       lines(v_rms(x@z), x@z, type = "s", lty = 1)
#     }
#     if(!is.null(x@vel[["vint"]])){
#       v_int <- approxfun(x@vel[["vint"]][["t"]], x@vel[["vint"]][["v"]], rule = 2, method = "constant", f = 1)
#       lines(v_int(x@z), x@z, type = "s", lty = 3)
#     }
#     if(!is.null(x@vel[["v"]]) && is.numeric(x@vel[["v"]])){
#       lines(x@vel[["v"]], x@z, type = "s", lty = 2, col = "red")
#     }
#   }else{
#     if(isZDepth(x)){
#       stop(msg_set_zunitToDepth)
#     }else{
#       stop("")
#     }
#   }
# }

# hyperbolaFromVrms <- function(x){
#   if(is.null(x@vel[["vrms"]])){
#     stop("You must first set v_rms velocities with 'setVel()'")
#   }
#   y <- mapply(hyperbolicTWT, x@vel[["vrms"]]$t, x@vel[["vrms"]]$v, 
#               MoreArgs = list(antsep = x@x))
#   list(twt = x@x, antsep = y)
# }

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
# simWavelet <- function(xt, q, fc){
#   xt <- xt / 1000
#   xT <- (2/3 + (1-q)/7)/fc
#   vt(xt, xT) - (2 - q)* vt(xt - xT/2, xT) + (1 - q)*vt(xt-xT, xT)
# }




# ---------------------------------------------------------------------------- #
# -------------------------  SIMULATE CMP ------------------------------------ 
# ---------------------------------------------------------------------------- #

#----- 1. velocity model
lyr <- list(vint = c(0.1, 0.095, 0.08, 0.09, 0.105, 0.09, 0.095),
            d    = c(0.5,   0.75,  1.01,  1.4,     1.9,  2.2, 2.6))
lyr[["vrms"]] <- sqrt(cumsum(lyr$vint * 2 * lyr$d)/(2 * cumsum(lyr$d/lyr$vint)))
lyr[["t0"]]   <- 2 * cumsum(lyr$d/lyr$vint)


# ----- 2. two-way travel time
# antenna separation
a <- seq(0, to = 20, by = 0.25)
# two-way travel time
TWT <- t(sapply(a, hyperbolicTWT, t0 = lyr$t0, v = lyr$vrms))

matplot(TWT, type = "l", col = "black", ylim = rev(range(TWT[,1])))


#----- 3. wavelet model
dz <- 0.25
fc <- 100 # MHz
t_w <- seq(0, by = dz, to = 15) # ns
w <- simWavelet(t_w, q = 0.9, fc = fc)

plot(t_w, w, type = "l")



# ----- 4. simulated CMP
zt <- seq(0, to = 250, by = dz)
X <- matrix(0, nrow = length(zt), ncol = length(a))
for(i in 1:ncol(TWT)){
  for(j in 1:nrow(TWT)){
    uij <- TWT[j, i]
    k <- round(uij/dz) + t_w/dz +1
    k <- k[k > 0 & k < nrow(X)]
    X[k,j] <- X[k,j]  + w[seq_along(k)]
  }
}

X_list <- list(data = X, x = a, z = zt, mode = "CMP", freq = 100)
x <- as(X_list, "GPR")

plot(x)
plot(x[,1])

x@vel

# ---------------------------------------------------------------------------- #
# ----------------------- NORMAL MOVE-OUT & CO -------------------------------
# ---------------------------------------------------------------------------- #


#---- NMO correction
x_NMO <- correctNMO(x) # NO ERROR because z0 = 0
x1 <- shiftToTime0(x)
x_NMO0 <- correctNMO(x1)
x_NMO1 <- correctNMO(x1, v = 0.05, method = "pchip")
x_NMO2 <- correctNMO(x1, v = 0.05, method = "linear")
plot(x_NMO1 - x_NMO2)
plot(x_NMO0)
plot(x_NMO1)
plot(x_NMO2)


plotTr(x1[,1:5])
plotTr(x_NMO0[,1:5])


#---- COMPUTE THE Normal Move-Out
D_NMO <- NMO(x1)
plot(D_NMO)
x1@vel[[1]]
D_NMO <- NMO(x1, v = 0.8)
plot(D_NMO)


#---- COMPUTE THE NMO streching Move-Out
S_NMO <- NMOstreching(x1, v = 0.061)
plot(S_NMO)
S_NMO <- NMOstreching(x1)
plot(S_NMO)
plot(S_NMO > 0.15)

plot(log(S_NMO), col = palGPR("slice"))

# ---------------------------------------------------------------------------- #
# ------------------------ VELOCITY SPECTRUM ---------------------------------
# ---------------------------------------------------------------------------- #

#---- Comparison: velocity linearly vs. exponential distributed
# difference not visible
par(mfrow = c(1, 2))
x_tv <- velocitySpectrum(x, v = seq(0.05, to = 0.11, length.out = 50))
plot(x_tv, clip = NULL, main = "linear velocities")
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

x_tv <- velocitySpectrum(x, 
                         v = exp(seq(log(0.05), 
                                     to = log(0.11), 
                                     length.out = 50)))
plot(x_tv, clip = NULL,  main = "exponential velocities")
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")




#---- APPLY FIRST MUTING BEFORE VELOCITY SPECTRUM
x0 <- x
x0_S <- NMOstreching(x0)
x0[x0_S > 0.2] <- 0
plot(x0)

par(mfrow = c(1, 2))
x0_tv <- velocitySpectrum(x0, v = seq(0.05, to = 0.11, length.out = 50))
plot(x0_tv, clip = NULL, main = "Full data, muted")
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

sel <- 1:20
x0_tv <- velocitySpectrum(x0[,sel], v = seq(0.05, to = 0.11, length.out = 50))
plot(x0_tv, clip = NULL, main = "Data subset, muted")
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

#----- d
x_tv <- velocitySpectrum(x, v = seq(0.05, to = 0.11, length.out = 50))
plot(x_tv, clip = NULL, main = "Full data")
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

x_tv <- velocitySpectrum(x[, sel], v = seq(0.05, to = 0.11, length.out = 50))
plot(x_tv, clip = NULL, main = "Data subset")
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")


#------------------------------------------------------------------------------#

sel <- 1:20

# both identical: semblance + win-semblance (w = 1)
x_tv <- velocitySpectrum(x[,sel], v = seq(0.02, to = 0.11, length.out = 50),
                         method = "winsemblance", w = 1)
plot(x_tv)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

x_tv <- velocitySpectrum(x[,sel], v = seq(0.02, to = 0.11, length.out = 50))
plot(x_tv)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

plot(x[,sel])


# ---------------------------------------------------------------------------- #
# ------------------------ VELOCITY ESTIMATION -------------------------------
# ---------------------------------------------------------------------------- #

sel <- 1:25
sel <- 1:ncol(x)
par(mfrow = c(1,2))
plot(x[,sel], barscale = FALSE, main = "CMP")
x_tv <- velocitySpectrum(x, v = seq(0.05, to = 0.11, length.out = 50))
plot(x_tv[1:nrow(x_tv),], barscale = FALSE,
     main = "semblance analysis")
contour(x_tv[1:nrow(x_tv),], add = TRUE, nlevels = 5)
grid()
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")
# Application of this formula can provide non-real velocities, if the travel time intervals are small or
# if the NMO velocity change is large. Such problems were not encountered in our case.
vv <- locator(type = "o", pch = 20, col = "green")

# #--- NMO-velocities
# # vnmo <- c(vv$x[1], vv$x)
# i_vv <- order(vv$y)
# vnmo <-  vv$x[i_vv]
# tnmo <-  vv$y[i_vv]
# xy.coords(vv)
# v <- approxfun(vv$y, vv$x, rule = 2, method = "linear", f = 0)
# # v <- approxfun(vv$x, vv$y, rule = 2, method = "constant")
# # plot(v(x@z), x@z, type = "l", ylim = rev(range(x@z)))
# # points(vv$x, vv$y, pch = 20)
# 
# lines(v(x_tv@z), x_tv@z, lwd = 2, col = "green")
# points(vv, pch = 20)


# Dix's velocities
vin <- dixVel(twt = vv$y, v = vv$x)


x <- setVel(x, v = vv$x, twt = vv$y, type = "vrms")
# x@vel
vel(x)
plot(x, barscale = FALSE)
plotVel(x)

# FIXME - Functions to write
getVel(x)  # return GPR object

x <- interpVel(x)
vel(x)
plotVel(x)

x <- interpVel(x, method = "pchip")
vel(x)
plotVel(x)

x <- smoothVel(x, w = 10, type = "vint")
plotVel(x)

par(mfrow = c(1, 2))
x <- smoothVel(x, w = 10, type = "vrms")
plot(correctNMO(x), main = "smoothing")

x <- interpVel(x)
plot(correctNMO(x), main = "no interpolation")

# Comparison: velocity model vs. true velocities
plotVel(x)
lines(lyr$vrms, lyr$t0, type = "s", lty = 3, col = "dodgerblue")
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")



#--- plot CMP with hyperbola & NMO correction
HPB <- hyperbolaFromVrms(x)

par(mfrow = c(1,2))
plot(x[,sel], barscale = FALSE, main = "CMP")
matplot(HPB$twt, HPB$antsep, type = "l", col = "green", lwd = 2, add = TRUE, lty = 1)

plot(correctNMO(x[,sel]), barscale = FALSE)



#--- plot NMO correction & trace stacking
par(mfrow = c(1,2))
plot(x, type = "wiggles", wsize = 1, col = "black")
plot(correctNMO(x), type = "wiggles", wsize = 1, col = "black")

par(mfrow= c(1,2))
plot(NMOstreching(x))
plot(stackNMO(x, thrs = NULL))

plot(NMOstreching(x) < 0.2)
plot(stackNMO(x, thrs = 0.2))




# ---------------------------------------------------------------------------- #
# ----------------------- TIME-TO-DEPTH CONVERSION ---------------------------
# ---------------------------------------------------------------------------- #

## TEST with our data

x <- smoothVel(x, w = 10, type = "vrms")
xc <- correctNMO(x)
xc@antsep <- 0
plot(xc)
x <- smoothVel(x, w = 10, type = "vint")
# x_save <- x
x <- xc
x2 <- convertTimeToDepth(xc)
# 
# v <- RGPR:::.getVel(xc, type = "vrms", strict = FALSE)
# 
# plot(v, type = "l")
# # x0 <- x
# 
# x <- x0

plot(x2)
plotVel(x2)

names(lyr)
#  "vint" "d"    "vrms" "t0"  
x@vel
h <- x@vel[["vint"]]$v * diff(c(0, x@vel[["vint"]]$t)/2)
d <- cumsum(h)

round(cbind(d, cumsum(lyr$d)), 3)

round(cbind( x@vel[["vint"]]$v, lyr$vint), 3)

round(cbind( x@vel[["vint"]]$t, lyr$t0), 3)



x@vel[["vint"]]$t
x@vel[["vrms"]]$t

lyr$d



## TEST WITH DATA FROM ANNAN 2005
v_nmo <- c(0, 0.095, 0.098, 0.105) # = v_rms
t_nmo <- c(0,    40,    50, 80)

t_v2 <- t_nmo * v_nmo^2
v_int <- sqrt(diff(t_v2)/diff(t_nmo))
h <- v_int * diff(t_nmo)/2
d <- cumsum(h)

A <- round(cbind(t_nmo, v_nmo, c(0, h), c(0, d), c(0, v_int)), 3)
colnames(A) <- c("t_nmo", "v_nmo", "thickness", "depth", "v_int")
A

