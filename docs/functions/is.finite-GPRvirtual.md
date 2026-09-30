# Finite, Infinite and NAN Numbers

```r
## S4 method for signature 'GPRvirtual'
is.finite(x)

## S4 method for signature 'GPRvirtual'
is.infinite(x)

## S4 method for signature 'GPRvirtual'
is.nan(x)
```

## Arguments

- `x`: (`GPR* object`)

## Returns

(`GPR* object`) With logical values (`TRUE` is the value is finite, `FALSE` if not.)

## Description

is.finite and is.infinite return an object of the same dimension as x, indicating which elements are finite (not infinite and not missing) or infinite.


