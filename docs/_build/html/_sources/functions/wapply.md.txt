# Accross trace computations

```r
wapply(x, w = NULL, FUN = mean, ..., track = TRUE)

## S4 method for signature 'GPR'
wapply(x, w = NULL, FUN = mean, ..., track = TRUE)

## S4 method for signature 'numeric'
wapply(x, w = NULL, FUN = mean, ..., track = TRUE)
```

## Arguments

- `x`: An object of the class GPR
- `w`: A length-one integer vector equal to the window length of the average window. If `w = NULL` a single trace corresponding to the average trace of the whole profile is returned.
- `FUN`: A function to compute the average (default is `mean`)
- `...`: Additional parameters for the FUN functions
- `track`: (`logical[1]`) Should the processing step be tracked?

## Returns

An object of the class GPR. When `w = NULL`, this function returns a GPR object with a single trace corresponding to the average trace of the whole radargram. When `w` is equal to a strictly positive interger this function returns a GPR object with a size identical to x where each trace corresponds to the function `FUN` applied to `w` neighboring traces centered on the considered trace.

## Description

`wapply` is a generic function used to produce results defined by an user function. The user function is applied accross traces (horizontal) using a moving window. Note that if the moving window length is not defined, all traces are averaged into one single trace (the results is similar to `apply(x, 1, FUN, ...)`.


