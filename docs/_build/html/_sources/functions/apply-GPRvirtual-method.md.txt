# Apply a function along the rows (samples per trace) or columns (traces)

```r
## S4 method for signature 'GPRvirtual'
apply(X, MARGIN, FUN, ..., simplify = TRUE)
```

## Arguments

- `X`: (`GPR`)
- `MARGIN`: (`integer[1|2]`) A vector giving the subscripts which the function will be applied over (see `apply()`).
- `FUN`: (`function`) The function to be applied (see `apply()`).
- `...`: Additional parameters to be passed (see `apply()`).
- `simplify`: (`logical[1]`) If `TRUE` the results should be simplified if possible.

Apply a function along the rows (samples per trace) or columns (traces)