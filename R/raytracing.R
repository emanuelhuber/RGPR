# delta_x
deltax <- function(h, theta){
  h * tan(theta)
}

#' Refraction angle
#' 
#' Compute the refraction angle based on the incidence angle and the 
#' refractive indices of the two media (applies Snell's Law, also known as the 
#' law of refraction)
#'
#' @param theta1 Incidence angle (media 1)
#' @param v1 Electromagnetic wave velocity of media 1
#' @param v2 Electromagnetic wave velocity of media 2
refracAng <- function(theta1, v1, v2){
  asin(sin(theta1)/v1*v2)
}



#' Ray tracing
#' 
#' @param h [`numeric(n)`] Layer heights
#' @param v [`numeric(n)`] Layer velocity
#' @param theta [`numeric(n)`] Incidence angle above first layer
#' @export
raytrace <- function(h, v, theta, x = 0){
  n <- length(h)
  thetas <- numeric(n)
  thetas[1] <- theta
  ray <- numeric(n)
  ray[1] <- deltax(h[1], thetas[1])
  RAY <- matrix(0, nrow = 2*n+1, ncol = 2)
  if(n > 1){
    for(i in 1:(n - 1)){
      thetas[i + 1] <- refracAng(thetas[i], v[i], v[i+1])
    }
    # ray <- cumsum(mapply(deltax, h, thetas))
    ht <- h * tan(thetas)
    ray <- cumsum(ht) 
    RAY[1, 1] <- x
    RAY[2:(n+1), 1] <- ray + x
    RAY[2:(n+1), 2] <- -cumsum(h)
    RAY[(n+2):(2*n + 1), 1] <- ray[n] + x + cumsum(rev(diff(c(0, ray)))) # RAY[n+1, 1] + RAY[n:1, 1]
    RAY[(n+2):(2*n + 1), 2] <- RAY[n:1, 2]
  }else{
    RAY[1, ] <- c(x, 0)
    RAY[2, ] <- c(ray[1] + x, -h)
    RAY[3, ] <- c(ray[1] + x + ray[1], 0)
  }
  return(RAY)
}

# return residuals
rayres <-function(theta, par_a, par_h, par_v){
  ray <- raytracehalf(par_h, par_v, theta)
  return( (tail(ray, 1) - par_a/2)^2 )
}

pathDist <- function(x){
  sqrt(rowSums( diff(x)^2 ))
}

#' Ray tracing
#' 
#' @param h [`numeric(n)`] Layer heights
#' @param v [`numeric(n)`] Layer velocity
#' @param theta [`numeric(n)`] Incidence angle above first layer
raytracehalf <- function(h, v, theta){
  n <- length(h)
  thetas <- numeric(n)
  thetas[1] <- theta
  ray <- numeric(n)
  ray[1] <- deltax(h[1], thetas[1])
  if(n > 1){
    for(i in 1:(n - 1)){
      thetas[i + 1] <- refracAng(thetas[i], v[i], v[i+1])
    }
    # ray <- cumsum(mapply(deltax, h, thetas))
    x <- h * tan(thetas)
    ray <- cumsum(x) 
  }
  return(ray)
}

#' CMP ray tracing
#'
#' Smulate ray tracing
#' @param av [`numeric(m)`] antenna separation
#' @param h [`numeric(n)`] Layer heights
#' @param v [`numeric(n)`] Layer velocity
#' @export
rayCMP <- function(av, h, v){
  m <- length(av)
  n <- length(h)
  MAT <- matrix(0, nrow = m, ncol = n)
  allRays <- vector("list", m * n)
  
  # it <- 0
  for (k in 1:n) {
    hk <- h[1:k]
    vk <- v[1:k]
    cum_hk <- cumsum(hk)
    
    for (j in 1:m) {
      theta_max <- atan2(av[j] / 2, h[k])
      
      if (k == 1) {
        ray <- raytracehalf(hk, vk, theta_max)
      } else {
        opt <- optim(theta_max, rayres, lower = 0, upper = theta_max,
                     method = "L-BFGS-B", par_a = av[j], par_h = hk, par_v = vk)
        ray <- raytracehalf(hk, vk, opt$par)
      }
      nr <- length(ray)
      # "-av[j]" to center the ray around zeoro
      RAY <- matrix(0, nrow = 2*nr+1, ncol = 2)
      RAY[1, 1] <- -av[j]/2
      RAY[2:(nr+1), 1] <- ray - av[j]/2
      RAY[2:(nr+1), 2] <- -cum_hk
      RAY[(nr+2):(2*nr + 1), 1] <- - RAY[nr:1, 1]
      RAY[(nr+2):(2*nr + 1), 2] <-  RAY[nr:1, 2]
      # RAY <- rbind(c(-av[j], 0), cbind(ray - av[j], -cum_hk))
      # RAY2 <- RAY
      # # flip for the other side
      # RAY2[, 1] <- -1 * AA[, 1]
      # allRays[[ (k - 1) * m + j ]] <- rbind(RAY, RAY2)
      allRays[[ (k - 1) * m + j ]] <- RAY
      
      # if(k > 3 && j > 3) stop()
      # Compute two-way travel time
      dists <- pathDist(RAY[1:(nr+1),])
      times <- dists / vk
      MAT[j, k] <- 2 * sum(times)
    }
    # sda
    
  }
  return(list(antsep = av, h = h, twt = MAT, rays = allRays))
}
