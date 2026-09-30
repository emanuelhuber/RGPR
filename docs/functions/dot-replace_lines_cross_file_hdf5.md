# Replace several survey lines with lines copied from a **different** survey backed by a DIFFERENT HDF5 file (internal)

```r
.replace_lines_cross_file_hdf5(h5, src_h5, x, ii, value)
```

## Arguments

- `h5`: Open, writable handle for the temporary copy of `x@path`.
- `src_h5`: Open, read-only handle for `value@path`.
- `x`: (`GPRsurvey`) Destination survey.
- `ii`: (`integer`) Destination indices (into `x@names`).
- `value`: (`GPRsurvey`) Source survey (different backing file).

## Returns

The updated `GPRsurvey` object.

## Description

Used from `[<-` when `x@path` and `value@path` point at different files. No self-overlap protection is needed here (source and destination are different files), so lines are copied directly to their final names.


