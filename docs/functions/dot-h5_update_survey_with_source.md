# Safely apply changes to a GPRsurvey HDF5 file that copies data in from a **second**, different HDF5 file

```r
.h5_update_survey_with_source(dsn, src_dsn, FUN, verify = TRUE, timeout = 30)
```

## Arguments

- `dsn`: (`character(1)`) Path to the destination `.h5` backing file (the one being modified).
- `src_dsn`: (`character(1)`) Path to the source `.h5` backing file (read-only). May be identical to `dsn`, in which case this behaves like `.h5_update_survey()` with a single handle passed as both `h5` and `src_h5`.
- `FUN`: (`function(h5, src_h5)`) Called once with the open, writable handle on the temporary copy of `dsn` (`h5`) and the open, read-only handle on `src_dsn` (`src_h5`). Its return value is passed back to the caller.
- `verify, timeout`: See `.h5_update_survey()`.

## Returns

Whatever `FUN` returned, invisibly.

## Description

Same idea as `.h5_update_survey()`, extended for updates that need read access to another backing file at the same time -- the canonical example being `SU1[1:2] <- SU2[3:4]`, which copies line groups from `SU2`'s file into `SU1`'s file. `SU1`'s file is updated via the usual temp-copy + verify + atomic-replace sequence; `SU2`'s file is only ever opened read-only.

## Details

Both files are locked for the duration of the update, in a fixed order (sorted by normalized path) regardless of which one is `dsn` and which is `src_dsn`. This avoids a deadlock if two concurrent replacements run in opposite directions at the same time (e.g. `SU1[i] <- SU2[j]` and `SU2[k] <- SU1[l]` running in two different R sessions).


