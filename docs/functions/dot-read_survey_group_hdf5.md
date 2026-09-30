# Read the complete `/survey` metadata group (internal)

```r
.read_survey_group_hdf5(h5)
```

## Arguments

- `h5`: Open, read-only (or read/write) hdf5r::H5File handle.

## Returns

A named list with one element per `GPRsurvey` slot that `.write_survey_group_hdf5()` writes.

## Description

Mirror image of `.write_survey_group_hdf5()`. Handles files written by older versions of this code that lack fields introduced later (`paths`, `transf`) by falling back to sensible defaults.


