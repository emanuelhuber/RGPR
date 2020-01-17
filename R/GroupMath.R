# getGroupMembers("Math")
#' Basic mathematical functions
#'
#' Methods for the base Math methods \link[methods]{S4groupGeneric}
#' @param x An object of the class RGPR.
#' @details Currently implemented methods include:
#' \itemize{
#'  \item{"abs", "sign", "sqrt", "ceiling", "floor", "trunc",
#'        "exp", "expm1", "log", "log10", "log2", "log1p", "cos",
#'        "cosh", "sin", "sinh", "tan", "tanh"}
#'  }
#' @examples
#' data(frenkeLine00)
#' A <- exp(frenkeLine00)
#' @rdname Math-methods
#' @aliases Math-GPR-method
setMethod(
  f="Math",
  signature="GPR",
  definition=function(x){
    x@data <- switch(.Generic,
                     abs      = abs(x@data),
                     sign     = sign(x@data),
                     sqrt     = sign(x@data)*sqrt(abs(x@data)),
                     floor    = floor(x@data),
                     ceiling  = ceiling(x@data),
                     trunc    = trunc(x@data),
                     round    = round(x@data),
                     signif   = signif(x@data),
                     cumsum   = paste("not allowed"),  
                     cumprod  = paste("not allowed"),  
                     cummax   = paste("not allowed"),  
                     cummin   = paste("not allowed"),  
                     exp      = exp(x@data),
                     log      = sign(x@data) *log(abs(x@data)),
                     expm1    = expm1( x@data),
                     log1p    = sign(x@data) *log1p(abs(x@data)), 
                     log10    = sign(x@data) *log10(abs(x@data)),
                     log2     = sign(x@data) *log2(abs(x@data)), 
                     cos      = cos(x@data),
                     sin      = sin(x@data),
                     tan      = tan(x@data),
                     cospi    = cospi(x@data),
                     sinpi    = sinpi(x@data),
                     tanpi    = tanpi(x@data),
                     cosh     = cosh(x@data),
                     sinh     = sinh(x@data),
                     tanh     = tanh(x@data),
                     acos     = acos(x@data),
                     asin     = asin(x@data),
                     atan     = atan(x@data),
                     acosh    = acosh(x@data),
                     asinh    = asinh(x@data),
                     atanh    = atanh(x@data),
                     gamma    = gamma(x@data),
                     lgamma   = lgamma(x@data),
                     digamma  = digamma(x@data),
                     trigamma = trigamma(x@data),
                     # "acos" "acosh" "asin" "asinh" "atan" "atanh" "gamma" "lgamma"
                     # "digamma" "trigamma"
                     stop(paste(.Generic, "not yet allowed on GPR objects"))
    )
    # proc(x) <- getArgs()
    return(x)
  }
)
