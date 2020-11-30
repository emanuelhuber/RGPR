
#' Class GPRset
#' 
#' A S4 class to represent GPR data sets. 
#' Array of dimension \eqn{n \times m \times p} (\eqn{n} samples, 
#' \eqn{m} traces or A-scans, and \eqn{p} set elements).
#' A GPR set is, for example, the 
#' multi-dimensional output of a transform function. Examples:
#' \describe{
#'   \item{Fourier transform}{The GPR data can be decomposed into two
#'   element set: phase and amplitude as a function of the frequency.}
#'   \item{Eigenimage decomposition}{Also called Karhunen-Loeve (KL) 
#'   transformation. The GPR data is decomposed into a set of eigenimages.}
#' }
#' @slot y       [\code{numeric(p)}] Values associated with the \code{p} 
#'               elements of the set.
#' @slot yunit   [\code{character(1|p)}] Unit(s) of set elements.
#' @slot ylab    [\code{character(1|p)}] Label(s) of set elements.
#' @slot formula [\code{expression}] Expression to reconstruct the GPR data
#'               (backtransform).
#' @name GPRset-class
#' @rdname GPRset-class
#' @export
setClass(
  Class = "GPRset",
  contains = "GPR",
  slots = c(
    y            = "numeric",    # y-values, length = p
    yunit        = "character",  # set units, length = 1|p
    ylab         = "character",  # set names, length = 1|p
    formula      = "expression"   # or "function" GPR = a*GPR1 + b*GPR2 + c*GPR3 + ....
    # extraData   = "list"
  )
)

