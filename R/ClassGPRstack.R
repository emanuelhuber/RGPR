
#' Class GPRstack
#' 
#' @name GPRstack-class
#' @rdname GPRstack-class
#' @export
setClass(
  Class = "GPRstack",
  contains = "GPR",
  slots = c(
    formula  = "expression" # or "function" GPR = a*GPR1 + b*GPR2 + c*GPR3 + ....
  )
)