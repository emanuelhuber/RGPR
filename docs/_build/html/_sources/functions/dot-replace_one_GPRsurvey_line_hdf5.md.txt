# Replace one survey line's data and in-memory metadata, given an already open HDF5 handle (internal)

```r
.replace_one_GPRsurvey_line_hdf5(h5, x, i, value, compress = 5L)
```

## Arguments

- `h5`: Open, writable hdf5r::H5File handle.
- `x`: (`GPRsurvey`) Survey to update.
- `i`: (`integer(1)`) Index (into `x@names`) of the line to replace.
- `value`: (`GPR`) Replacement line.
- `compress`: (`integer(1)`) gzip level for the rewritten line.

## Returns

The updated `GPRsurvey` object (HDF5 line already written).

## Description

Low-level building block used by both `[<-` (looped over indices) and `[[<-` (a single call). Unlike the previous `.replace_one_GPRsurvey_line()` (removed -- see `papply.R` for its other former caller, now updated to use this function too), this does not open/close the HDF5 file itself and does not recompute intersections or write survey metadata; callers are expected to wrap one or more calls to this function in a single `.h5_update_survey()` transaction and call `.finalize_replace_write_hdf5()` once at the end.


