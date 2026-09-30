# Extract GPR object from GPRsurvey object

```r
getGPR(x, id, verbose = FALSE)

## S4 method for signature 'GPRsurvey'
getGPR(x, id, verbose = FALSE)
```

## Arguments

- `x`: (`GPRsurvey`)
- `id`: (`integer[1]|character[1]`) Index or name of the GPR line to extract.
- `verbose`: (`logical[1]`) If `TRUE`, prints a short progress message.

## Returns

(`GPR`) An object of class `GPR`.

## Description

Extract a single GPR line from a `GPRsurvey` object.

## Details

`getGPR()` is now a thin wrapper around `x[[id]]` (see `?"subset-GPRsurvey"`). Previously, `getGPR()` re-read the line from its **original** raw file path (`x@paths[[id]]`), bypassing the HDF5 backing file entirely, and then patched in coordinates/CRS/units/intersection annotations from the survey object by hand. That made `getGPR()` depend on the original input files still being present at their original paths -- which defeats the point of having an HDF5 backup -- and it behaved inconsistently with `x[[id]]`, which already reads a self-consistent record straight from the HDF5 file (coordinates included: `gridCoords()` writes updated coordinates directly into `/lines/<name>/coords/xyz`, so `x[[id]]` always reflects the latest values).

One behavior change: the previous implementation also annotated crossing points from `x@intersections[[id]]` onto the returned `GPR` object. That annotation step is not reproduced here for now (it doesn't fit the "just read what's stored" model); if you rely on it, compute the annotation explicitly after calling `getGPR()`/`x[[id]]`, e.g. with `ann<-()` and `findClosestCoord()`.


