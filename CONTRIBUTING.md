


# Plot

Reset the plot parameters to their default values

```{r eval=FALSE}
op <- par(no.readonly=TRUE)
# plot...
par(op)
```

# Conventions
* empty slot should have length 0: `length(object@mySlot) = 0`. Therefore, use: 
    + `character(0)` (instead of `""`, because `length("") = 1`),
    + `matrix(ncol = 0, nrow = 0)`,
    + `numeric(0)`,
    + `integer(0)`,
    + `list()`

* with generic method: use exactly the same arguments as the existing methods. To know the arguments of a method use `args(myMethod)`.
* Naming:
    + `fPath` (instead of filePath, fileName, filename, path)
    + private function starts with a dot. Example: `.myPrivateFunction()`.
* for functions in time domain, use either the number of points or the time
* `return()` Only use `return()` for early returns. Otherwise rely on R to return the result of the last evaluated expression.
* Miscenallenous
    + use `paste0()` instead of `paste(..., sep = "")`
    + use `message()` inside a function instead of `cat()` or `print()`!
  
## Tricks
* `invisible()`: Return a (temporarily) invisible copy of an object, used in plot function
* `missing()`: check if an arguments is missing
* Don't want to pass `...`-arguments to a function? Solution: use a wrapper function, where the args after `...` are the args that you don't want to have in the function. e.g.:

    ```{r}
    lPoints <- function(..., log, axes, frame.plot, panel.first, panel.last) {
      points(...)
    }
    ```
* `unname()`

# Keeping track of the processing

The adopted notations is:

```r
functionName@arg1=val1+arg2=val2+arg3=val3
```

Within a method, to add the current processing step and its arguments, write
```r
proc(x) <- getArgs()
```

To insert "manually" an additional processing step, use the function `addArg()`:
  ```r
proc(x) <- getArgs( addArgs = c('arg10' = val10, 'arg11' = val11))
```

But unfortunately, the function name when passed as argument (e.g. `FUN = mean`) is not caught by the method `getArgs()` and must be explicitely extracted and provided:

```r
funName <- getFunName(FUN)
proc(x) <- getArgs( addArgs = c('FUN' = funName))
```

# Documentation

## Functions

### Minimal documentation

```r
#' Title
#'
#' Description
#'
#' Details
#' @param parameter1 Description parameter1
#' @param parameter2 Description parameter2
#' @return A nice value
#' examples
#' sum(1:10)
#' sum(.Machine$integer.max, 1L)
#' \dontrun{
#' sum("a")
#' }
myFunction <- function(..., na.rm = TRUE){}
```


## Class

```r
#' Class Title
#' 
#' Description: An S4 class to represent a ground-penetrating radar (GPR) data.
#'
#' Details concerning the class = TODO
#' @slot version A length-one character vector indicating the version of RGPR
#' @slot data A \eqn{m \times n} 
#' @name GPR-class
#' @rdname GPR-class
#' @export
setClass(
  Class="GPR", 
  ...
```

## S4 Method & Generic

### Generic

```r
#' @name CMPAnalysis
#' @rdname CMPAnalysis-methods
#' @exportMethod CMPAnalysis
setGenericVerif("CMPAnalysis", function(x, method = c("semblance", 
               "winsemblance", "wincoherence", "wincoherence2"), v = NULL, 
               asep = NULL, w = NULL) standardGeneric("CMPAnalysis"))
```               
      
### Method
```r
#' Common mid-point (CMP) analysis
#' 
#' either use 'rec' and 'trans' to compute the distance between the antennas
#' or give the distance between the antennas (asep)
#' or seq(x@antsep, by = x@dx, length.out = length(x))
#'
#' Details...
#' @param x An object of the class \code{GPR}
#' @param method A length-one character vector 
#' @rdname CMPAnalysis-methods
#' @aliases CMPAnalysis,GPR-method
#' @export
setMethod("CMPAnalysis", "GPR", function(x, method = c("semblance", 
                                         "winsemblance",   "wincoherence", 
                                         "wincoherence2"), v = NULL, 
                                         asep = NULL, w = NULL){
  ...
```

### Aliases

```r
#' Basic arithmetical functions
#'
#' 
#' @param e1 An object of the class RGPR.
#' @param e2 An object of the class RGPR.
#' @examples
#' data(frenkeLine00)
#' A <- exp(frenkeLine00)
#' B <- A + frenkeLine00
#' @rdname Arith-methods
#' @aliases Arith,GPR,ANY-method
setMethod(
  f= "Arith",
  signature=c(e1="GPR",e2="ANY"), 
  definition=.GPR.arith
)

#' @name Arith
#' @rdname Arith-methods
#' @aliases Arith,GPR,GPR-method
setMethod(
  f= "Arith",
  signature=c(e1="GPR",e2="GPR"), 
  definition=.GPR.arith
)
#' @name Arith
#' @rdname Arith-methods
#' @aliases Arith,ANY,GPR-method
setMethod(
  f= "Arith",
  signature=c(e1="ANY",e2="GPR"), 
  definition=.GPR.arith
)
```
