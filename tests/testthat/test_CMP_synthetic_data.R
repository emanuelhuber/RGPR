
#------------------------------------------------------------------------------#

# identify 2D peaks:
# - https://rdrr.io/cran/geodiv/src/R/localsurface.R
# - https://www.sthu.org/blog/13-perstopology-peakdetection/index.html





#-------------------------------------------------------------------------
# Simulate CMP

# FIXME: layer depth + internal velocity

TWThyperbolic_d <- function(a, t0, v){
  sqrt(t0^2 + (a/v)^2)
}

lyr <- list(vint = c(0.1, 0.095, 0.08, 0.09, 0.105, 0.09, 0.095),
            d    = c(0.5,   0.75,  1.01,  1.4,     1.9,  2.2, 2.6))
lyr[["vrms"]] <- sqrt(cumsum(lyr$vint * 2 * lyr$d)/(2 * cumsum(lyr$d/lyr$vint)))
lyr[["t0"]]   <- 2 * cumsum(lyr$d/lyr$vint)
a <- seq(0, to = 20, by = 0.25)

U <- t(sapply(a, TWThyperbolic_d, t0 = lyr$t0, v = lyr$vrms))

dim(U)
plot3D::image2D(U)

# GPR wavelet:
#   Check chap 11 of Anan in the book Near-Surface Geophysics
# "The radiated wavelet from a GPR system is a compli-
# cated function of the antenna construction and the electronics
# drive circuitry.For impulse style ultra wideband systems using
# short electric dipole antennas, a simple mathematical model is
# helpful for numerical simulation."

vt <- function(xt, xT){
  test <- xt > 0 & xt < xT
  vt <- numeric(length(xt))
  vt[test] <-  0.5 * ( 1 + cos(pi * (xt[test]- xT/2)/(xT/2)))
  return(vt)
}

# xt = time in ns
# where 0 < q < 1 (damping factor)
# fc = center frequency in MHz
wavGPR <- function(xt, q, fc){
  xt <- xt / 1000
  xT <- (2/3 + (1-q)/7)/fc
  vt(xt, xT) - (2 - q)* vt(xt - xT/2, xT) + (1 - q)*vt(xt-xT, xT)
}

dz <- 0.25
fc <- 100 # MHz
wt <- seq(0, by = dz, to = 15) # ns

plot(wt, wavGPR(wt, q = 0.9, fc = fc), type = "l")

w <- wavGPR(wt, q = 0.9, fc = fc)

plot(wt, w, type = "l")


zt <- seq(0, to = 250, by = dz)
X <- matrix(0, nrow = length(zt), ncol = length(a))


for(i in 1:ncol(U)){
  for(j in 1:nrow(U)){
    uij <- U[j, i]
    k <- round(uij/dz) + wt/dz +1
    k <- k[k > 0 & k < nrow(X)]
    X[k,j] <- X[k,j]  + w[seq_along(k)]
  }
}

plot3D::image2D(X)

X_list <- list(data = X, x = a, z = zt, mode = "CMP", freq = 100)
x <- as(X_list, "GPR")
plot(x)
plot(x[,1])

x

#------------------------------------------------------------------------------#


x_NMO <- correctNMO(x) # NO ERROR because z0 = 0

plot(x)

x1 <- shiftToTime0(x)

plot(x1)

x_NMO0 <- correctNMO(x1)
x_NMO1 <- correctNMO(x1, v = 0.05, method = "pchip")
x_NMO2 <- correctNMO(x1, v = 0.05, method = "linear")
plot(x_NMO1 - x_NMO2)
plot(x_NMO0)
plot(x_NMO1)
plot(x_NMO2)

diff(x_NMO2@z)

plotTr(x1[,1:5])


# COMPUTE THE Normal Move-Out
D_NMO <- NMO(x1)
x1@vel[[1]]
D_NMO <- NMO(x1, v = 0.8)
plot(D_NMO)

# NMO streching = D_NMO / t(x = 0)
# S_NMO <- D_NMO / x1@z
# S_NMO@data[is.infinite(S_NMO@data)] <- 0

S_NMO <- NMOstreching(x1, v = 0.061)
S_NMO <- NMOstreching(x1)
plot(S_NMO)
plot(S_NMO > 0.15)

plot(log(S_NMO), col = palGPR("slice"))


plot(NMO(x1, v = 0.8))
NMO(x1, v = 0.8)@data[1:5,1:10]

x <- shiftToTime0(x)

unnorm_ccor <- function(x){
  test <-  abs(sum( rowSums(x, na.rm = TRUE)^2 -  rowSums(x^2, na.rm = TRUE)))
  return(test) #(sum(x^2, na.rm = TRUE)) / (ncol(x) * (ncol(x) - 1))
}
v = seq(0.06, to = 0.11, length.out = 50)
i <- 1
x_tv <- velocitySpectrum(x, v = v)

for(i in seq_along(v)){
  y <- .NMOCor(x, v = v[i], asep = x@antsep)
  wi <- 5
  x_tv@data[,i] <- wapplyRowC(y@data, width = wi, by = 1,
                              FUN = unnorm_ccor)
}
x_tv <- x_tv/max(x_tv)


plot(x_tv, col = palGPR("slice"))
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

max(x_tv)


for(i in seq_along(v)){
  y <- .NMOCor(x, v = v[i], asep = x@antsep)
  SS <- semblance(y@data)/ncol(x)
  x_tv@data[,i] <- wapplyC(SS, width = wi, by = 1,
                              FUN = min, na.rm = TRUE)
}

plot(x_tv)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")


x_tv <- velocitySpectrum(x)
x_tv <- velocitySpectrum(x[,1:10], v = seq(0.02, to = 0.15, length.out = 50))
plot(x_tv, clip = NULL)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")


x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50),
                         method = "semblance")
plot(x_tv)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")


x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50),
                         method = "winsemblance", w = 5)
plot(x_tv)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")


x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50),
                         method = "minsemblance", w = 1)
plot(x_tv)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

x_tv <- velocitySpectrum(x[,1:10], v = seq(0.02, to = 0.11, length.out = 50),
                         method = "wincoherence", w = 2)
plot(x_tv)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")


x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50),
                         method = "wincoherence2", w = 10)
plot(x_tv)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")


#------------------------------------------------------------------------------#
# APPLY FIRST MUTING BEFORE VELOCITY SPECTRUM
x0 <- x
x0_S <- NMOstreching(x0)
x0[x0_S > 0.2] <- 0
plot(x0)


u <- x0[1,]
u <- x0[1:3,]

plot(u)

u@x
u@z


par(mfrow = c(1, 2))
x0_tv <- velocitySpectrum(x0, v = seq(0.02, to = 0.11, length.out = 50))
plot(x0_tv, clip = NULL)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

sel <- 1:20
x0_tv <- velocitySpectrum(x0[,sel], v = seq(0.02, to = 0.11, length.out = 50))
plot(x0_tv, clip = NULL)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")
#-----
x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50))
plot(x_tv, clip = NULL)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

x_tv <- velocitySpectrum(x[, sel], v = seq(0.02, to = 0.11, length.out = 50))
plot(x_tv, clip = NULL)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")
#----
plot(x_tv - x0_tv, clip = NULL)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

#------------------------------------------------------------------------------#

sel <- 1:20

x_tv <- velocitySpectrum(x[,sel], v = seq(0.02, to = 0.11, length.out = 50),
                         method = "winsemblance", w = 1)
plot(x_tv)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

plot(x[,sel])


# antsep(x) <- seq(0.6, by = 0.5, length.out = ncol(x))
# vlim <- vel(x) * c(0.65, 1.3)
# x_tv <- CMPAnalysis(x[, 1:20], method = "winsemblance",
                    # v =  seq(vlim[1], vlim[2], length = 20),
                    # w = 7)
sel <- 1:25
sel <- 1:ncol(x)
par(mfrow = c(1,2))
plot(x[,sel], barscale = FALSE, main = "CMP")
plot(x_tv[1:nrow(x_tv),], barscale = FALSE,
     main = "semblance analysis")
contour(x_tv[1:nrow(x_tv),], add = TRUE, nlevels = 5)
grid()
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")
# Application of this formula can provide non-real velocities, if the travel time intervals are small or
# if the NMO velocity change is large. Such problems were not encountered in our case.
vv <- locator(type = "o", pch = 20, col = "green")

#--- NMO-velocities
# vnmo <- c(vv$x[1], vv$x)
vnmo <-  vv$x
tnmo <-  vv$y
xy.coords(vv)
v <- approxfun(tnmo, vnmo, rule = 2, method = "linear", f = 0)
# v <- approxfun(vnmo, tnmo, rule = 2, method = "constant")
# plot(v(x@z), x@z, type = "l", ylim = rev(range(x@z)))
# points(vnmo, tnmo, pch = 20)

lines(v(x_tv@z), x_tv@z, lwd = 2, col = "green")
points(vv, pch = 20)


#--- internal velocities
DixVel <- function(twt, v){
  if(twt[1] != 0){
    twt <- c(0, twt)
  }
  if(v[1] != 0){
    v <- c(0, v)
  }
  twt_v2 <- twt * v^2
  vint <- sqrt(diff(twt_v2)/diff(twt))
  sel <- is.na(vint)
  if(sum(sel) > 0){
    message(rep("*", sum(sel)))
  }
  return(list(t = twt[-1], v = vint))
}

vin <- DixVel(twt = vv$y, v = vv$x)

vint <- approxfun(vin$t, vin$vel, rule = 2, method = "constant", f = 1)
vrms <- approxfun(tnmo, vnmo, rule = 2, method = "constant", f = 1)

setVel <- function(x, v, twt, type = c("vrms", "vint")){
  type <- match.arg(type, c("vrms", "vint"))
  if(length(v) != length(twt)){
    stop("'v' and 'twt' must have the same length!")
  }
  i <- order(twt)
  v <- v[i]
  twt <- twt[i]
  if(type == "vrms"){
    v <- list("vrms" = list("t" = twt,
                            "v" = v),
              "vint" = DixVel(twt = twt, v = v))
  }else{
    v <- list("vint" = DixVel(twt = twt, v = v))
  }
  x@vel <- v
  return(x)
}

x <- setVel(x, v = vnmo, twt = tnmo, type = "vrms")
x@vel
plot(x)

range(sapply(x@vel, function(x) x$v))

plotVel <- function(x){
  v_lim <- range(sapply(x@vel, function(x) x$v))
  plot(0, type = "n", ylim = rev(range(x_tv@z)),  xlim = v_lim, yaxs = "i",
       xlab = .vlab(x),
       ylab = .zlab(x))
  if(!is.null(x@vel[["vrms"]])){
    v_rms <- approxfun(x@vel[["vrms"]][["t"]], x@vel[["vrms"]][["v"]], rule = 2, method = "constant", f = 1)
    lines(v_rms(x@z), x@z, type = "s", lty = 1)
  }
  if(!is.null(x@vel[["vint"]])){
    v_int <- approxfun(x@vel[["vint"]][["t"]], x@vel[["vint"]][["v"]], rule = 2, method = "constant", f = 1)
    lines(v_int(x@z), x@z, type = "s", lty = 3)
  }
  if(!is.null(x@vel[["v"]])){
   lines(v, x@z, type = "s", lty = 2)
  }

}

plotVel(x)

interpVel <- function(x, type = c("vrms", "vint"),
                      method = c("stairs", "linear", "nearest", "pchip", "cubic", "spline")){
  type <- match.arg(type, c("vrms", "vint"))
  method <- match.arg(method, c("stairs", "linear", "nearest", "pchip", "cubic", "spline"))
  if(method == "stairs"){
    v_stairs <- approxfun(x@vel[[type]][["t"]], x@vel[[type]][["v"]], 
                              rule = 2, method = "constant", f = 1)
    x@vel[["v"]] <- v_stairs(x@z)
  }else{
    x@vel[["v"]] <- signal::interp1(x = x@vel[[type]][["t"]], y = x@vel[[type]][["v"]],
                                   xi = x@z, method = method,
                                   extrap = TRUE)
  }
  return(x)
}

x <- interpVel(x)

plotVel(x)

# getVelocities <- function(x, type = )

# NMO correction with variable 1D velocity
plot(vrms(x_tv@z), x_tv@z, type = "l", ylim = rev(range(x_tv@z)), pch = 20, xlim = c(0.02, 0.12))
lines(lyr$vrms, lyr$t0, type = "s", lty = 3)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

plot(vint(x_tv@z), x_tv@z, type = "l", ylim = rev(range(x_tv@z)), pch = 20, col = "red", xlim = c(0.02, 0.12))
lines(lyr$vint, lyr$t0, type = "s", lty = 3, col = "red")
points(lyr$vint, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")


x@vel <- list(vrms(x_tv@z))  # use v_rms for NMO correction but v_int for time to depth correction!

# SMOOTH Vrms
vrms_smooth <- signal::interp1(x = tnmo, y = vnmo,
                               xi = x@z, method = "pchip",
                               extrap = TRUE)

plot(vrms(x_tv@z), x_tv@z, type = "l", ylim = rev(range(x_tv@z)), pch = 20, xlim = c(0.08, 0.10))
lines(lyr$vrms, lyr$t0, type = "s", lty = 3)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")
lines(vrms_smooth, x@z, type = "l", col = "red")
# x@vel <- list(vint(x_tv@z))


#--- plot CMP with hyperbola & NMO correction
par(mfrow = c(1,2))
plot(x[,sel], barscale = FALSE, main = "CMP")
.t_NMO <- function(t0, v, antsep){
  sqrt(t0^2 + (antsep/v)^2)
}
TT <- mapply(.t_NMO, vv$y, vv$x, MoreArgs = list(antsep = x@x))
for(i in seq_along(vv$y)){
  lines(x@x, TT[,i], col = "green", lwd = 2)
}
plot(correctNMO(x[,sel]))


#--- plot NMO correction & trace stacking
par(mfrow = c(1,2))
plot(correctNMO(x[,sel]), type = "wiggles", wsize = 5)
plot(rowSums(correctNMO(x[,sel])@data), x@z, ylim = rev(range(x@z)), type = "l" )


#-----
par(mfrow = c(1,2))
plot(x, barscale = FALSE, main = "CMP")
.t_NMO <- function(t0, v, antsep){
  sqrt(t0^2 + (antsep/v)^2)
}
TT <- mapply(.t_NMO, vv$y, vv$x, MoreArgs = list(antsep = x@x))
for(i in seq_along(vv$y)){
  lines(x@x, TT[,i], col = "green", lwd = 2)
}
plot(correctNMO(x, method = "pchip"))

#-----
x@vel <- list(vrms_smooth)
par(mfrow = c(1,2))
plot(x, barscale = FALSE, main = "CMP")
.t_NMO <- function(t0, v, antsep){
  sqrt(t0^2 + (antsep/v)^2)
}
TT <- mapply(.t_NMO, vv$y, vv$x, MoreArgs = list(antsep = x@x))
for(i in seq_along(vv$y)){
  lines(x@x, TT[,i], col = "green", lwd = 2)
}
plot(correctNMO(x, method = "pchip"))


#-----
plot(NMOstreching(x) < 0.2)
x@vel <- list(vrms_smooth)
xnew <- x
xnew[NMOstreching(xnew) > 0.2] <- 0
par(mfrow = c(1,2))
plot(xnew, barscale = FALSE, main = "CMP")
.t_NMO <- function(t0, v, antsep){
  sqrt(t0^2 + (antsep/v)^2)
}
TT <- mapply(.t_NMO, vv$y, vv$x, MoreArgs = list(antsep = x@x))
for(i in seq_along(vv$y)){
  lines(xnew@x, TT[,i], col = "green", lwd = 2)
}
plot(correctNMO(xnew, method = "pchip"))


#--- plot NMO correction & trace stacking
par(mfrow = c(1,2))
plot(correctNMO(xnew), type = "wiggles", wsize = 5)
# FIXME -> account for 0-traces -> for the mean (don't take number of columns)
plot(rowSums(correctNMO(xnew)@data), x@z, ylim = rev(range(x@z)), type = "l" )

plot(correctNMO(xnew))
     
stackNMO <- function(x, thrs = NULL){
  x_NMOcor <- correctNMO(x, thrs = thrs)
  # vn <- rowSums(!is.na(x_NMOcor@data), na.rm = TRUE)
  # xx <- rowSums(x_NMOcor, na.rm = TRUE)/vn
  x <- rowMeans(x_NMOcor, na.rm = TRUE)    
  return(x)
  
}

plot(xx - yy)
plot(yy)

xx@data

plot(xnew)
plot(stackNMO(xnew, thrs = NULL))
plot(stackNMO(xnew, thrs = 0.2))
plot(stackNMO(xnew, thrs = 0.1))

plot(NMOstreching(xnew) < 0.2)
plot(NMOstreching(xnew) < 0.1)



#----------------------
#--- Vrms vs Vint as a function of two-way travel time & depth

DT <- diff(c(0, vin$t/2))
zv <- cumsum(DT * vin$vel)

DT <- diff(c(0, x@z)/2)
zv <- cumsum(DT * vint(x@z))

plot(zv, x@z, xlab = "depth", ylab = "two-way travel time", type = "l")


plot(vrms(x_tv@z), x_tv@z,  ylim = rev(range(x_tv@z)), type = "l", xlim = c(0.0, 0.2),
     lwd = 2, lty = 1, xlab = "velocity (m/ns)", ylab = "two-way travel time (ns)")
lines(vint(x_tv@z), x_tv@z, col = "black", lwd = 2, lty = 3)


plot(vrms(x_tv@z), zv,  ylim = rev(c(0, max(zv))), type = "l", xlim = c(0.00, 0.2),
     xlab = "velocity (m/ns)", ylab = "depth (m)", lwd = 2, lty = 1)
lines(vint(x@z), zv, lty = 3, lwd = 2)




par(mfrow = c(1,2))
plot(x[,1:25], barscale = FALSE, main = "CMP")
plot(x_tv[1:nrow(x_tv),], barscale = FALSE,
     main = "semblance analysis")
contour(x_tv[1:nrow(x_tv),], add = TRUE, nlevels = 5)
grid()
lines(v(x_tv@z), x_tv@z, lwd = 2, col = "green")
points(vv, pch = 20)

lines(vint(x@z), x@z,  lwd = 2, lty = 1)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

par(mfrow = c(1,2))
plot(x[,1:25], barscale = FALSE, main = "CMP")
.t_NMO <- function(t0, antsep, v){
  sqrt(t0^2 + (antsep/v)^2)
}

t0 <- vv$y
TT <- matrix(nrow = length(vv$y), ncol = length(x@x))
for(i in seq_along(vv$y)){
  TT[i,] <- .t_NMO(vv$y[i], x@x, vv$x[i])
  lines(x@x, TT[i,], col = "green", lwd = 2)
}
plot(x_tv[1:nrow(x_tv),], barscale = FALSE,
     main = "semblance analysis")
contour(x_tv[1:nrow(x_tv),], add = TRUE, nlevels = 5)
grid()
lines(v(x_tv@z), x_tv@z, lwd = 2, col = "green")
points(vv, pch = 20)

lines(vint(x@z), x@z,  lwd = 2, lty = 1)
points(lyr$vrms, lyr$t0, pch = 21, col = "darkslateblue", lwd = 2, bg = "gold")

vin$vel
lyr$vint
lyr$t0

## TEST WITH DATA FROM ANNAN 2005
v_nmo <- c(0, 0.095, 0.098, 0.105) # = v_rms
t_nmo <- c(0, 40, 50, 80)

t_v2 <- t_nmo * v_nmo^2
v_int <- sqrt(diff(t_v2)/diff(t_nmo))
h <- v_int * diff(t_nmo)/2
d <- cumsum(h)

round(cbind(t_nmo, v_nmo, c(0, h), c(0, d), c(0, v_int)), 3)





