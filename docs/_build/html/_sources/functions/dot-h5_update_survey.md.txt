# Safely apply changes to an existing GPRsurvey HDF5 backing file

```r
.h5_update_survey(dsn, FUN, verify = TRUE, timeout = 30)
```

## Arguments

- `dsn`: (`character(1)`) Path to the existing `.h5` backing file.
- `FUN`: (`function(h5)`) Function called once with the open, writable hdf5r::H5File handle on the **temporary** copy. Its return value is passed back to the caller of `.h5_update_survey()`.
- `verify`: (`logical(1)`) Re-read every dataset after writing to validate checksums before the file is swapped in. Default `TRUE`; set to `FALSE` to skip the extra read pass on very large surveys where the write has already been verified by other means.
- `timeout`: (`numeric(1)`) Seconds to wait for the lock before giving up.

## Returns

Whatever `FUN` returned, invisibly.

## Description

This is the single entry point used by every function that **modifies** an existing `.h5` backing file (as opposed to `GPRsurvey()`, which creates one from scratch -- see that function for the equivalent "create" version of this same pattern).

## Details

Workflow:

1. Acquire a lock on `dsn` (see `.h5_lock_acquire()`).
2. Copy `dsn` to a temporary file in the same directory.
3. Open that temporary file once , in `"a"` (read/write) mode.
4. Call `FUN(h5)`, where `h5` is the open hdf5r::H5File handle. `FUN` should perform **all** the mutations for this update (e.g. write updated coordinates for several lines, then rewrite `/survey/intersections`) so the whole logical update happens under one open file handle.
5. Flush and close the file.
6. If `verify = TRUE` (default), read every dataset back to validate checksums (`.h5_verify_checksums()`).
7. Atomically replace `dsn` with the temporary file.

If any step fails, `dsn` is left completely untouched: the lock is released, the temporary file is deleted, and the error propagates to the caller.


