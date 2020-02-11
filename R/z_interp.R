# xi <- cumsum(round(abs(rnorm(n = 100, 2, 1)), 1))
# yi <- round(0.0001 * (xi- 100)^3, 1)
# xy <- cbind(xi, yi)
# 
# vl <- c(4, 19, 34, 44, 59, 75, 79, 79, 65, 44, 33, 21, 21, 22, 21, 21, 21, 23, 25 , 28, 20)
# z <- c(10, 9,  7,  6,  4,  1,  1,  2,  3,  3,  1, -2 , 1,  1,   2,  2,  3, 5,  7 , 10, 10)
# 
# test <- interpToCoords(vl, z, xy)
# 
# 
# i <- vl
# u <- z

# i = index of xyz (some of the elements of xyz)
# u = value at xyz[i]
# xyz = coordinates
interpToCoords <- function(i, u, xy, method = "linear"){
  rmDpl <- diff(RGPR::posLine(cbind(i, u))) == 0
  # i = vl
  # u = y
  i <- i[!rmDpl]
  u <- u[!rmDpl]
  fsplit <- cumsum(c(0, diff(i) == 0))
  isplt <- split(i,     f = fsplit)
  usplt <- split(u,     f = fsplit)
  
  dxy <- RGPR::posLine(xy)
  # plot(dxy[i], z, type = "o", col = "red")
  v_all <- c()
  uk_all <- c()
  for(j in 1:length(isplt)){
    k <- isplt[[j]]
    # for(k in 1:(length(i) - 1)){
    #   v <- c(v, i[k]:(i[k+1] - 1))
    # }
    # v <- c(v, i[k+1])
    kdx <- 1
    v <- c()
    if(length(k) > 1){
      for(l in 1:(length(k) - 1)){
        if(k[l] < k[l+1]){
          v <- c(v, k[l]:(k[l+1] - 1))
        }else if(k[l] > k[l+1]){
          v <- c(v, k[l]:(k[l+1] + 1))
        }else{
          v <- c(v, k[l])
        }
        kdx <- c(kdx, length(v) + 1)
      }
      v <- c(v, k[l+1])
      dxyk <- RGPR::posLine(xy[v, ]) #cumsum(c(0, abs(diff(xi[v]))))
      dxyj <- dxyk[kdx]
      uj <- usplt[[j]]
      uk <- signal::interp1(dxyj, uj, dxyk, extrap = TRUE, method = method)
    }else{
      v <- k
      uk <- usplt[[j]]
    }
    # points(dxy[v], uk, type = "o", pch= 20, col = "blue")
    v_all <- c(v_all, v)
    uk_all <- c(uk_all, uk)
  }
  return(list(i = v_all, u = uk_all))
}




interpPath <- function(x, method = "linear", dx){
  # tst <- duplicated(x)
  dxy <- posLine(x)
  tst <- diff(dxy) == 0
  x <- x[!tst, ]
  dxy <- dxy[!tst]
  dxyi <- seq(min(dxy), max(dxy), by = dx)
  xint <- matrix(nrow = length(dxyi), ncol = ncol(x))
  for(i in 1:ncol(x)){
    xint[, i] <- signal::interp1(dxy, x[, i], dxyi, method = method )
  }
  # yi0 <-  signal::interp1(dxy, x[, 2], dxyi, method = "linear" )
  # xi0 <-  signal::interp1(dxy, x[, 1], dxyi, method = "linear" )
  return(xint)  
}

interpCoords <- function(x, method = "linear", dx){
  # tst <- duplicated(x)
  dxy <- posLine(x)
  tst <- diff(dxy) == 0
  x <- x[!tst, ]
  dxy <- dxy[!tst]
  dxyi <- seq(min(dxy), max(dxy), by = dx)
  xint <- matrix(nrow = length(dxyi), ncol = ncol(x))
  for(i in 1:ncol(x)){
    xint[, i] <- signal::interp1(dxy, x[, i], dxyi, method = method )
  }
  # yi0 <-  signal::interp1(dxy, x[, 2], dxyi, method = "linear" )
  # xi0 <-  signal::interp1(dxy, x[, 1], dxyi, method = "linear" )
  return(xint)  
}

# x <- c(2,  3,  3, 1, 1, 5, 6, 10, 7, 3, 1, 5, 1, 1, 4 , 6, 10)
# y <- c(2, -1,  0, 1, 1, 2, 4, 8, 8, 2, 4,  8, 1, 2, -1, - 3, 1)
# 
# 
# #x <- cbind(x,y)
# a <- interpPath(cbind(x, y), method = "linear", dx = 0.2)
# a2 <- interpPath(cbind(x, y), method = "pchip", dx = 0.2)
# 
# plot(x, y, type = "o")
# lines(a[, 1], a[, 2], col = "green", type = "o")
# lines(a2[, 1], a2[, 2], col = "blue", type = "o")


