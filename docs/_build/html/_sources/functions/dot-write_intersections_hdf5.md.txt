# Write line intersection data into the HDF5 file

```r
.write_intersections_hdf5(h5, obj)
```

## Arguments

- `h5`: Open, writable hdf5r::H5File handle.
- `obj`: `GPRsurvey` object, typically just returned by `findIntersection()`.

## Description

Writes the `@intersections` slot (if non-empty) to `/survey/intersections` as one dataset per named element. Assumes `/survey` already exists (i.e. this is called after `.write_survey_group_hdf5()`).


