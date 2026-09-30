# Finish a line-replacement transaction: recompute intersections once and write survey metadata + intersections once (internal)

```r
.finalize_replace_write_hdf5(h5, x)
```

## Arguments

- `h5`: Open, writable hdf5r::H5File handle (the temporary copy managed by `.h5_update_survey()`/`.h5_update_survey_with_source()`).
- `x`: (`GPRsurvey`) Survey with all in-memory line metadata for this call already applied.

## Returns

The (possibly updated) `GPRsurvey` object.

## Description

Called exactly once per `[<-`/`[[<-` call, after every line for that call has already been (re)written under the same open `h5` handle -- mirrors the "intersections computed at the end" pattern used in `.finalize_gridCoords_GPRsurvey()` (see `gridCoords.R`).


