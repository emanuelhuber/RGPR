# Resolve display names to HDF5 group ids via a survey's `/survey/names`

```r
.h5_resolve_line_ids(h5, names_vec)
```

## Arguments

- `h5`: Open hdf5r::H5File handle for the survey that `names_vec` belongs to.
- `names_vec`: (`character`) Display names to resolve (typically `value@names`, or a subset of it).

## Returns

(`character`) HDF5 group ids, same length/order as `names_vec`.

## Description

Used when copying line groups between (or within) HDF5 files based on another `GPRsurvey` object's `@names` (e.g. `SU1[1:2] <- SU2[3:4]`): `value@names` are display names, but the **physical** groups that must be copied are identified by position within `value@path`'s own `/survey/names`, not by `value@names` directly.


