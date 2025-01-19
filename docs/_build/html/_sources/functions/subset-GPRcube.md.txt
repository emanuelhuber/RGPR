# Extract and replace parts of a GPRcube object

```r
## S4 method for signature 'GPRcube,ANY,ANY'
x[i, j, k, drop = TRUE]
```

## Arguments

- `x`: (`GPRcube`)
- `i`: (`integer`) Indices specifying elements to extract or replace.
- `j`: (`integer`) Indices specifying elements to extract or replace.
- `k`: (`integer`) Indices specifying elements to extract or replace.
- `drop`: Not used.

## Returns

(`GPR|numeric`) Returns a numeric vector only if `x[]`.

Extract parts of a GPR object