# HDF5 write helper for `writeGPR("GPRsurvey")`

```r
.writeGPR_h5(obj, dsn, overwrite, compress)
```

## Arguments

- `obj`: Object of class `GPRsurvey`.
- `dsn`: (`character(1)` or `NULL`) Destination `.h5` path.
- `overwrite`: (`logical(1)`) Overwrite an existing `dst`?
- `compress`: Unused here (compression is fixed by whatever the source file already contains) -- kept for a consistent call signature with the rest of the `writeGPR()` dispatch.

## Description

Three cases:

## Details

1. `dsn` is `NULL` or identical to `obj@path`: the file is already current -- no-op, return `obj`.
2. `dsn` is a different path: copy the backing HDF5 file there and return an updated `obj` pointing at the new location.
3. `obj@path` no longer exists: raise an informative error asking the user to re-create with `GPRsurvey()`.

The copy in case 2 uses the same lock + temporary-file + checksum-verify

 * atomic-replace pattern as `.h5_update_survey()` (see `hdf5_update.R`), so an interrupted copy never leaves a partial/corrupt file at `dst`, and a concurrent writer to the **source** file is guarded against with a lock.


