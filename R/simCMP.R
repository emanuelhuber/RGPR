
#' Simulates CMP data
#' 
#' Simulates CMP data. 
#' 
#' @param vint (`numeric[n]`) internal velocity of the layers
#' @param d (`numeric[n]`) thickness of the layers 
#' @param antsep (`numeric[m]`) antenna separations use for the data acquisition 
#' @param dz (`numeric[1]`) time sampling (ns)
#' @param zmax (`numeric[1]`) maximum time (ns)
#' @param fc (`numeric[1]`) Center frequency in MHz
#' @param lw (`numeric[1]`) wavelet duration (ns)
#' @param qw (`numeric[1]`) Damping factor for the wavelet, where 0 < q < 1
#' @return (`GPR`) CMP data
#' @export
simCMP <- function(vint = c(0.1, 0.095, 0.08, 0.09, 0.105, 0.09, 0.095),
                   d = c(0.5,   0.75,  1.01,  1.4,     1.9,  2.2, 2.6),
                   antsep = seq(0, to = 20, by = 0.25),
                   dz = 0.25, zmax = 250, fc = 100, lw = 15, qw = 0.9){
  
  lyr <- list(vint = vint,
              d    = d)
  lyr[["vrms"]] <- sqrt(cumsum(lyr$vint * 2 * lyr$d)/(2 * cumsum(lyr$d/lyr$vint)))
  lyr[["t0"]]   <- 2 * cumsum(lyr$d/lyr$vint)
  
  
  # ----- 2. two-way travel time
  # antenna separation
  a <- antsep
  # two-way travel time
  TWT <- t(sapply(a, hyperbolicTWT, t0 = lyr$t0, v = lyr$vrms))
  
  matplot(TWT, type = "l", col = "black", ylim = rev(range(TWT[,1])))
  
  
  #----- 3. wavelet model
  # dz <- 0.25
  # fc <- 100 # MHz
  # t_w <- seq(0, by = dz, to = 15) # ns
  t_w <- seq(0, by = dz, to = lw) # ns
  w <- simWavelet(t_w, q = qw, fc = fc)
  
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
  
  X_list <- list(data = X, x = a, z = zt, surveymode = "CMP", freq = 100,
                 antsep = a, mode = "CMP")
  x <- as(X_list, "GPR")
  return(x)

}