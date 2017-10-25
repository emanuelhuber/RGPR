


## Plot

Reset the plot parameters to their default values

```{r eval=FALSE}
op <- par(no.readonly=TRUE)
# plot...
par(op)
```

## Conventions
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
* Miscenallenous
    + `paste0()` instead of `paste(..., sep = "")`
    + 
    
## Tricks
* use `message()` inside a function instead of `cat()` or `print()`!
* `invisible()`: Return a (temporarily) invisible copy of an object, used in plot function
* `missing()`: check if an arguments is missing
* Don't want to pass `...`-arguments to a function? Solution: use a wrapper function, where the args after `...` are the args that you don't want to have in the function. e.g.:
    ```{r}
    lPoints <- function(..., log, axes, frame.plot, panel.first, panel.last) {
     points(...)
    }
    ```
* `unname()`

## Keeping track of the processing

The adopted notations is:
```
functionName@arg1=val1+arg2=val2+arg3=val3
```

Within a method, to add the current processing step and its arguments, write
```{r eval=FALSE}
proc(x) <- getArgs()
```

To insert "manually" an additional processing step, use the function `addArg()`:
```{r eval=FALSE}
proc(x) <- getArgs( addArgs = c('arg10' = val10, 'arg11' = val11))
```

But unfortunately, the function name when passed as argument (e.g. `FUN = mean`) is not caught by the method `getArgs()` and must be explicitely extracted and provided:
```{r eval=FALSE}
funName <- getFunName(FUN)
proc(x) <- getArgs( addArgs = c('FUN' = funName))
```

