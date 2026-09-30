# Create a GPRsurvey object

```r
GPRsurvey(
  x,
  dsn,
  name = "",
  desc = "",
  overwrite = FALSE,
  compress = 5L,
  verify = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- `x`: (`character[k]`) Vector of `k` file paths to GPR data files. All formats supported by `readGPR()` are accepted.
- `dsn`: (`character(1)`) Path for the output HDF5 file (must end in `.h5` by convention). If it already exists and `overwrite = FALSE`, an error is raised before any work is done; if `overwrite = TRUE`, the existing file is only replaced at the very end, once the new file has been fully built and verified (see Details).
- `name`: (`character(1)`) Name of the survey.
- `desc`: (`character(1)`) Description of the survey.
- `overwrite`: (`logical(1)`) Overwrite an existing HDF5 file? Default `FALSE`.
- `compress`: (`integer(1)`) gzip compression level 0-9 for the data arrays inside the HDF5 file; `0` disables compression. Default `5L`. See Details for guidance on whether compression is worth it for GPR data.
- `verify`: (`logical(1)`) Re-read every dataset after writing to validate checksums before the file is swapped in. Default `TRUE`.
- `verbose`: (`logical(1)`) Print progress messages.
- `...`: Additional arguments passed to `readGPR()`.

## Returns

An object of class `GPRsurvey`.

## Description

Reads a set of GPR data files, collects survey-level metadata, writes everything to a single HDF5 file, and returns a lightweight `GPRsurvey` object backed by that file. A `GPRsurvey` object is backed by an HDF5 file. The R object contains survey-level metadata and references line data stored in the backing file.

## Details

### How the file is written

 The backing file is built in a temporary file in the same directory as ‘dsn’ , under a lock that prevents two processes from building/ replacing the same file at the same time (see `.h5_lock_acquire()` in `hdf5_update.R`). Only once every line has been written, survey-level metadata has been written, intersections have been computed from the **final** coordinates of every line, and (by default) every dataset has been read back to validate its checksum, is the temporary file atomically swapped in for `dsn` (`file.rename()`). If anything fails partway through -- a malformed input file, a disk error, an interrupted session -- `dsn` is left completely untouched: either the previous file (if `overwrite = TRUE` and one existed) or nothing at all. You never end up with a truncated/corrupt file sitting at the path you expect a valid backup.

### Precision and compression

 The main data array is always stored as 64-bit floating point (`H5T_NATIVE_DOUBLE`), matching R's native numeric precision -- so writing to HDF5 never loses precision relative to the in-memory `GPR` object. Every dataset (including small metadata vectors) is chunked and protected with an HDF5 fletcher32 checksum. gzip compression (with a byte-shuffle pre-filter, which typically improves the ratio noticeably for floating-point data) is applied to the radar-data arrays and, for large surveys, to per-line coordinates; see `compress`.

Whether compression is worth it for GPR data depends on the data: amplitude-sampled radargrams are noisy and don't compress as well as, say, images with large flat regions, so don't expect dramatic ratios -- but shuffle+gzip typically still buys a modest (roughly 1.3-2x) reduction for a low CPU cost, which is usually worth it for a backup copy that is written once and read occasionally. If you process huge surveys very frequently and disk space is not a concern, set `compress = 0L` to skip compression entirely and maximize write/read speed.

## See Also

`readGPRsurvey()`, `writeGPR()`


