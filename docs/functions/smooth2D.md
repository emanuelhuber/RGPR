# Adapative weights 2D smoothing

```r
smooth2D(x, method = c("awsaniso", "aws", "awsp"), ...)

## S4 method for signature 'GPR'
smooth2D(x, method = c("awsaniso", "aws", "awsp"), ...)
```

## Arguments

- `x`: (`GPR`)
- `method`: (`character[1]`) `awsaniso` stand for anisotropic adaptive weights smoothing (call the function `adimpro::awsaniso()`), `aws` stand for adaptive weights smoothing using a local constant model (call the function `adimpro::awsimage()`), and `awsp` stand for adaptive weights smoothing using a local polynomial models up to a degree of 2. (call the function `adimpro::awsimage()`).
- `...`: additional parameters to be passed to the `adimpro`
    
    functions: `adimpro::awsaniso()`, `adimpro::awsimage()`, and `adimpro::awsimage()`.

## Returns

(`GPR`)

A wrapper for the following `adimpro` functions: `adimpro::awsaniso()`, `adimpro::awsimage()`, and `adimpro::awsimage()`.