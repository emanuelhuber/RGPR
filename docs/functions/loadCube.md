# Load an HDF5-backed (or view) `GPRcube`'s data into memory

```r
loadCube(x, ...)

## S4 method for signature 'GPRcube'
loadCube(x, ...)
```

## Arguments

- `x`: (`GPRcube`)
- `...`: Currently unused.

## Returns

(`GPRcube`) with `@data` populated and `@view = FALSE`. If `x` is already in-memory, returned unchanged.

## Description

`interpSlices(..., hdf5 = "always")` (or `"auto"` for large cubes) returns a `GPRcube` whose `@data` is empty and whose `@path` points at the HDF5 file holding the actual array; subsetting such a cube (or a view of one) with `[` returns another view rather than reading anything (see `subset_GPRcube.R`). `loadCube()` reads a cube's **own**

region -- the whole file for a non-view cube, or just the view's region for a view -- into `@data`, returning an ordinary in-memory `GPRcube` with `@view` cleared. Note this defeats the purpose of HDF5 backing if the region doesn't actually fit in memory -- prefer `x[i, j, k]` to read only what you need, or `materialize()` to write a view out to its own independent HDF5 file without loading it into R at all.


