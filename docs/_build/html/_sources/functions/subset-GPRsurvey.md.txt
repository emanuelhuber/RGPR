# Extract and replace parts of a GPRsurvey object

```r
## S4 method for signature 'GPRsurvey,ANY,ANY'
x[i, j, ..., drop = TRUE]

## S4 method for signature 'GPRsurvey,ANY,ANY'
x[[i, j, ..., exact = TRUE]]

## S4 replacement method for signature 'GPRsurvey,ANY,ANY,GPR'
x[[i, j, ...]] <- value
```

## Arguments

- `x`: (`GPRsurvey`)
- `i`: (`integer`) Indices specifying elements to extract or replace.
- `j`: (`integer`) Not used.
- `...`: Not used.
- `drop`: Not used.
- `exact`: Not used.

## Returns

(`GPRsurvey`)

## Description

Subsetting a GPRsurvey returns a view. A view does not create a new HDF5 file. It remains backed by the original HDF5 file and is not modified in place.

## Details

Extract parts of a GPRsurvey object


