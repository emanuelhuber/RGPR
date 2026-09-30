# Write updated trace coordinates for selected lines (internal)

```r
.write_GPRsurvey_coords_hdf5(h5, obj, ids)
```

## Arguments

- `h5`: Open, writable hdf5r::H5File handle.
- `obj`: Object of class `GPRsurvey` (already updated in memory).
- `ids`: (`integer`) Indices (into `obj@names`) of the lines whose coordinates changed.

## Description

Unlike the previous version, this function takes an already open `hdf5r` handle (supplied by `.h5_update_survey()`) instead of opening and closing the backing file itself. Callers are responsible for wrapping this in `.h5_update_survey()`.


