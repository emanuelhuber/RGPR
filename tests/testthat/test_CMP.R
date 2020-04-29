
dsn <- "/mnt/data/RGPR/CODE/DEVELOPMENT/CMP/cmp.DT1"
dsn <- "/mnt/data/RGPR/CODE/DEVELOPMENT/CMP/2018_10_17-20_cimano_grid02_LINE00.DT1"

# Trace no. 13 = doublons
dsn <- "/mnt/data/RGPR/CODE/DEVELOPMENT/CMP/2018_10_17-20_cimano_grid02_LINE01.DT1"

x <- readGPR(dsn)

plot(x)
#--- correction
plot(x[, -13])
x_pos <- xpos(x)
x <- x[, -13]
xpos(x) <- x_pos[1:ncol(x)]
plot(x)
#--------------

plot(x)

plot(x)
plot(x, xlim = c(-1, 10))
plot(x[,1])

x@x
x@antsep


x_NMO <- correctNMO(x) # ERROR

x@z0 <- rep(17.5, ncol(x))

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
plot(S_NMO > 0.15)

plot(log(S_NMO), col = palGPR("slice"))


plot(NMO(x1, v = 0.8))
NMO(x1, v = 0.8)@data[1:5,1:10]

x <- shiftToTime0(x)

v = seq(0.02, to = 0.11, length.out = 50)
i <- 1
plot3D::image2D(y@data)
x_tv <- velocitySpectrum(x, v = v)

for(i in seq_along(v)){
  y <- .NMOCor(x, v = v[i], asep = x@antsep)
  wi <- 5
  x_tv@data[,i] <- wapplyRowC(y@data, width = wi, by = 1,
                              FUN = unnorm_ccor)
}
x_tv <- x_tv/max(x_tv)

unnorm_ccor <- function(x){
  test <-  abs(sum( rowSums(x, na.rm = TRUE)^2 -  rowSums(x^2, na.rm = TRUE)))
  return(test) #(sum(x^2, na.rm = TRUE)) / (ncol(x) * (ncol(x) - 1))
}

plot(x_tv/max(x_tv), col = palGPR("slice"))

max(x_tv)


for(i in seq_along(v)){
  y <- .NMOCor(x, v = v[i], asep = x@antsep)
  SS <- semblance(y@data)/ncol(x)
  x_tv@data[,i] <- wapplyC(SS, width = wi, by = 1,
                              FUN = min, na.rm = TRUE)
}

plot(x_tv)
x_tv@data[is.na(x_tv@data)] <- 0
x_tv@data[is.infinite(x_tv@data)] <- 0

(y@data[,i])
x_tv@data[,i]

x_tv <- velocitySpectrum(x)
x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50))
plot(x_tv)

x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50),
                         method = "semblance")
plot(x_tv)

x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50),
                         method = "winsemblance", w = 2)
plot(x_tv)

x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50),
                         method = "minsemblance", w = 1)
plot(x_tv)

x_tv <- velocitySpectrum(x[,1:10], v = seq(0.02, to = 0.11, length.out = 50),
                         method = "wincoherence", w = 2)
plot(x_tv)


x_tv <- velocitySpectrum(x, v = seq(0.02, to = 0.11, length.out = 50),
                         method = "wincoherence2", w = 10)
plot(x_tv)


sel <- 1:10

x_tv <- velocitySpectrum(x[,sel], v = seq(0.02, to = 0.11, length.out = 50),
                         method = "semblance")
plot(x_tv)
plot(x[,sel])
# antsep(x) <- seq(0.6, by = 0.5, length.out = ncol(x))
# vlim <- vel(x) * c(0.65, 1.3)
# x_tv <- CMPAnalysis(x[, 1:20], method = "winsemblance",
                    # v =  seq(vlim[1], vlim[2], length = 20),
                    # w = 7)

par(mfrow = c(1,2))
plot(x[,1:25], barscale = FALSE, main = "CMP")
plot(x_tv[1:nrow(x_tv),], barscale = FALSE,
     main = "semblance analysis")
contour(x_tv[1:nrow(x_tv),], add = TRUE, nlevels = 5)
grid()

# Application of this formula can provide non-real velocities, if the travel time intervals are small or
# if the NMO velocity change is large. Such problems were not encountered in our case.
vv <- locator(type = "o")

#--- NMO-velocities
# vnmo <- c(vv$x[1], vv$x)
vnmo <-  vv$x
tnmo <-  vv$y
v <- approxfun(tnmo, vnmo, rule = 2, method = "linear", f = 0)
# v <- approxfun(vnmo, tnmo, rule = 2, method = "constant")
# plot(v(x@z), x@z, type = "l", ylim = rev(range(x@z)))
# points(vnmo, tnmo, pch = 20)

lines(v(x_tv@z), x_tv@z, lwd = 2, col = "green")
points(vv, pch = 20)


v <- approxfun(tnmo, vnmo, rule = 2, method = "constant", f = 1)
# NMO correction with variable 1D velocity
x@vel <- list(v(x_tv@z))

par(mfrow = c(1,2))
plot(x[,sel], barscale = FALSE, main = "CMP")
.t_NMO <- function(t0, v, antsep){
  sqrt(t0^2 + (antsep/v)^2)
}
TT <- mapply(.t_NMO, vv$y, vv$x, MoreArgs = list(antsep = x@x))
for(i in seq_along(vv$y)){
  lines(x@x, TT[,i])
}
plot(correctNMO(x[,sel]))



#--- internal velocities
intVel <- function(tx, vx){

  vint <- numeric(length(vx))
  vint[1] <- vx[1]
  for( i in 2:length(vx) ){
    # vint[i] <- suppressWarnings(sqrt( (vv$y[i] * vx[i]^2 - vv$y[i-1] * vx[i-1]^2) /
    vint[i] <- (sqrt( (vv$y[i] * vx[i]^2 - vv$y[i-1] * vx[i-1]^2) /
                        (tx[i] - tx[i-1]) ))
  }
  sel <- is.na(vint)
  if(sum(sel) > 0){
    vint[sel] <- approx( x = tx[!sel], y = vint[!sel], xout = tx[sel],
                         rule = 2)$y
    message("")
  }
  tx <- rep(tx, each = 2)
  tx <- c(0, tx[-length(tx)])
  vi <- rep(vint, each = 2)

  return(list(t = tx, vel = vi))
}

vin <- intVel(tx = vv$y, vx = vv$x)

DT <- diff(vin$t[c(1, seq(3, by = 2, to = length(vin$vel)), length(vin$vel))]/2)
zv <- c(cumsum(DT * vin$vel[seq(1, by= 2, to = length(vin$vel))]))
plot(vin$vel, vin$t, ylim = rev(c(0, max(vv$y))), type = "l", xlim = c(0.005, 0.2),
     lwd = 2, lty = 1)
lines(vv$x, vv$y, col = "black", lwd = 2, lty = 3)


zv2 <- rep(zv, each = 2)
zv2 <- c(0, zv2[-length(zv2)])
plot(vin$vel, zv2,  ylim = rev(c(0, max(zv))), type = "l", xlim = c(0.00, 0.13),
     xlab = "velocity", ylab = "depth", lwd = 2, lty = 1)
lines(vv$x, zv, lty = 3, lwd = 2)



par(mfrow = c(1,2))
plot(x[,1:25], barscale = FALSE, main = "CMP")
plot(x_tv[1:nrow(x_tv),], barscale = FALSE,
     main = "semblance analysis")
contour(x_tv[1:nrow(x_tv),], add = TRUE, nlevels = 5)
grid()
lines(v(x_tv@z), x_tv@z, lwd = 2, col = "green")
points(vv, pch = 20)

lines(vin$vel,  vin$t,  lwd = 2, lty = 1)


par(mfrow = c(1,2))
plot(x[,1:25], barscale = FALSE, main = "CMP")
.t_NMO <- function(t0, antsep, v){
  sqrt(t0^2 + (antsep/v)^2)
}

t0 <- vv$y
TT <- matrix(nrow = length(vv$y), ncol = length(x@x))
for(i in seq_along(vv$y)){
  TT[i,] <- .t_NMO(vv$y[i], x@x, vv$x[i])
  lines(x@x, TT[i,])
}
plot(x_tv[1:nrow(x_tv),], barscale = FALSE,
     main = "semblance analysis")
contour(x_tv[1:nrow(x_tv),], add = TRUE, nlevels = 5)
grid()
lines(v(x_tv@z), x_tv@z, lwd = 2, col = "green")
points(vv, pch = 20)

lines(vin$vel,  vin$t,  lwd = 2, lty = 1)
