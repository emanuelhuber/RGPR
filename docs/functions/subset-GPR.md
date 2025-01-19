# Extract and replace parts of a GPR object

```r
## S4 method for signature 'GPR,ANY,ANY'
x[i, j, ..., drop = TRUE]

## S4 replacement method for signature 'GPR,ANY,ANY'
x[i, j, ...] <- value
```

## Arguments

- `x`: (`GPR`)
- `i`: (`integer`) Indices specifying elements to extract or replace.
- `j`: (`integer`) Indices specifying elements to extract or replace.
- `...`: Not used.
- `drop`: Not used.
- `value`: (`numeric`) Value to set.

## Returns

(`GPR|`)numeric`] Returns a numeric vector only if`x[]`.

Extract parts of a GPR object