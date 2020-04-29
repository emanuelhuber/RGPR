# getGroupMembers("Math")
#' Basic mathematical functions
#'
#' Methods for the base Math methods \link[methods]{S4groupGeneric}
#' @param x An object of the class GPRvirtual
#' @details Currently implemented methods include:
#' \itemize{
#'  \item{"abs", "sign", "sqrt", "ceiling", "floor", "trunc", "cummax", 
#'        "cummin", "cumprod", "cumsum", "log", "log10", "log2", "log1p", 
#'        "acos", "acosh", "asin", "asinh", "atan", "atanh", "exp", "expm1", 
#'        "cos", "cosh", "cospi", "sin", "sinh", "sinpi", "tan", "tanh", 
#'        "tanpi", "gamma", "lgamma", "digamma", "trigamma"}
#'  }
# @examples
# data(frenkeLine00)
# A <- exp(frenkeLine00)
#' @rdname Math-methods
#' @aliases Math-GPRvirtual-method
setMethod(
  f = "Math",
  signature = "GPRvirtual",
  definition = function(x){
    x@data <- switch(.Generic,
                     abs      = abs(x@data),
                     sign     = sign(x@data),
                     sqrt     = sign(x@data)*sqrt(abs(x@data)),
                     ceiling  = ceiling(x@data),
                     floor    = floor(x@data),
                     trunc    = trunc(x@data),
                     # round    = round(x@data, ...),
                     # signif   = signif(x@data, ...),
                     cummax   = paste("not allowed"),  
                     cummin   = paste("not allowed"),  
                     cumprod  = paste("not allowed"),  
                     cumsum   = paste("not allowed"),  
                     log      = sign(x@data) *log(abs(x@data)),
                     log10    = sign(x@data) *log10(abs(x@data)),
                     log2     = sign(x@data) *log2(abs(x@data)), 
                     log1p    = sign(x@data) *log1p(abs(x@data)), 
                     acos     = acos(x@data),
                     acosh    = acosh(x@data),
                     asin     = asin(x@data),
                     asinh    = asinh(x@data),
                     atan     = atan(x@data),
                     atanh    = atanh(x@data),
                     exp      = exp(x@data),
                     expm1    = expm1( x@data),
                     cos      = cos(x@data),
                     cosh     = cosh(x@data),
                     cospi    = cospi(x@data),
                     sin      = sin(x@data),
                     sinh     = sinh(x@data),
                     sinpi    = sinpi(x@data),
                     tan      = tan(x@data),
                     tanh     = tanh(x@data),
                     tanpi    = tanpi(x@data),
                     gamma    = gamma(x@data),
                     lgamma   = lgamma(x@data),
                     digamma  = digamma(x@data),
                     trigamma = trigamma(x@data),
                     stop(paste(.Generic, "not yet allowed on GPR objects"))
    )
    # proc(x) <- getArgs()
    return(x)
  }
)