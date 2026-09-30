# Is this GPRcube backed by an HDF5 file rather than an in-memory array?

```r
isH5Backed(x)

## S4 method for signature 'GPRcube'
isH5Backed(x)
```

## Arguments

- `x`: (`GPRcube`)

## Returns

(`logical[1]`)

## Description

`TRUE` both for a "whole" HDF5-backed cube (`@view = FALSE`) and for an unmaterialized view/subset of one (`@view = TRUE`) -- in both cases `@data` is empty and the real data lives at `@path`. Use `x@view` directly if you need to distinguish the two.


