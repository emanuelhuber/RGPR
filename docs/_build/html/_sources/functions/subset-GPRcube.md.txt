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

## Description

Extract parts of a GPR object

## Details

Works transparently whether `x@data` is an in-memory array or `x` is HDF5-backed (see `isH5Backed()`). Reading is always lazy where it can be: for an HDF5-backed source, only the requested `[i, j, k]` region is ever touched, and slicing/profile extraction (`GPRslice`/`GPR` results, below) reads just that region and returns a plain in-memory object, since those are expected to be small.

A `GPRcube`-returning sub-cube extraction (`x[i, j, k]` where `i`, `j`, and `k` all have length > 1) behaves differently for an HDF5-backed source: it returns a **view** (`@view = TRUE`, see `GPRcube-class`) rather than reading anything at all. A view's `@data` stays empty and `@path` still points at `x`'s original backing file; subsetting a view again composes the index mapping (`@viewIdx`) rather than reading, so chained subsetting like `x[1:50,,][ ,1:20, ]` still resolves back to the original file correctly. Use `loadCube()` to pull just a view's region into memory, or `materialize()` to write it to its own independent HDF5 file without ever loading it into R (see `materialize_GPRcube.R`). For an in-memory source, the sub-cube branch still reads eagerly as before (`@view = FALSE`) -- there's no backing file to defer reading from.


