# Get processing steps applied to the data

```r
proc(x)

proc(x) <- value

## S4 method for signature 'GPR'
proc(x)

## S4 replacement method for signature 'GPR'
proc(x) <- value
```

## Arguments

- `x`: (`GPR`) An object of the class GPR.
- `value`: (`character`)

## Returns

A character vector whose elements contain the name of the processing functions with their arguments applied previously on the GPR data.

`processing` returns all the processing steps applied to the data.