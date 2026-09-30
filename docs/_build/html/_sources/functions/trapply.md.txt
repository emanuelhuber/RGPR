# Trace statistics

```r
trapply(x, w = NULL, FUN = mean, ..., track = TRUE)

## S4 method for signature 'GPR'
trapply(x, w = NULL, FUN = mean, ..., track = TRUE)
```

## Arguments

- `x`: An object of the class GPR
- `w`: A length-one integer vector equal to the window length of the average window. If `w = NULL` similar to `apply(x, MARGIN = 2, FUN, ...)`
- `FUN`: A function to compute the average (default is `mean`)
- `...`: Additional parameters for the FUN functions
- `track`: (`logical[1]`) Should the processing step be tracked?

## Returns

An object of the class GPR. When `w = NULL`, this function returns a GPR object with a as many trace as the original GPR object but with potentially a different number of samples.

## Description

`trapply` is a generic function used to produce results defined by an user function. The user function is applied accross the samples (vertical) using a moving window. Note that if the moving window length is not defined, all samples are averaged into one single vector (the results is similar to `apply(x, 2, FUN, ...)`.


