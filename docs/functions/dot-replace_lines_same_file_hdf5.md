# Replace several survey lines with lines copied from a **different** survey backed by the SAME HDF5 file (internal)

```r
.replace_lines_same_file_hdf5(h5, x, ii, value)
```

## Arguments

- `h5`: Open, writable hdf5r::H5File handle for the (shared) temporary copy of the backing file.
- `x`: (`GPRsurvey`) Destination survey.
- `ii`: (`integer`) Destination indices (into `x@names`).
- `value`: (`GPRsurvey`) Source survey (same backing file as `x`).

## Returns

The updated `GPRsurvey` object.

## Description

Only used from `[<-` when `x@path` and `value@path` point at the same file. Line groups are copied to temporary names first (protecting against self-overlapping replacements such as `SU[1:2] <- SU[2:1]`, where the destination of one replacement is the source of another), then moved into their final names, then the temporary names are removed. Everything happens under the single `h5` handle supplied by `.h5_update_survey()`.


