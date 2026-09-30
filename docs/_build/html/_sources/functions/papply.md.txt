# Apply batch processing to a GPRsurvey object

```r
papply(obj, prc = NULL)

## S4 method for signature 'GPRsurvey'
papply(obj, prc = NULL)

## S4 method for signature 'GPR'
papply(obj, prc = NULL)
```

## Arguments

- `obj`: Object of class `GPRsurvey`.
- `prc`: A named list of processing functions and their arguments.

## Returns

A processed `GPRsurvey` object.

## Description

Applies a list of processing functions to each GPR line of a materialized `GPRsurvey`. The processed lines are written back to the HDF5 backing file. Survey-level metadata and intersections are updated once at the end, which is more efficient than replacing each line through `[[<-`.

## Details

All lines are rewritten under a single HDF5 update transaction (see `.h5_update_survey()` in `hdf5_update.R`): one lock, one temporary copy of the backing file, one open handle for the whole loop, one checksum verification pass, one atomic swap into place at the end. If processing fails partway through (e.g. one of the functions in `prc` errors on some line), the backing file is left completely untouched -- you get the error back with the **original** file still intact, rather than a file that's been partially reprocessed.


