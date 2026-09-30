# Verify the checksums of every dataset in an HDF5 file

```r
.h5_verify_checksums(path, verbose = FALSE)
```

## Arguments

- `path`: (`character(1)`) Path to the HDF5 file to verify.

## Returns

`TRUE`, invisibly, if every dataset reads back cleanly. Raises an error otherwise.

## Description

Opens `path` read-only and reads every dataset in it, which forces HDF5 to validate the fletcher32 checksums that were set when the datasets were written (see `.h5_write_vector()`, `.h5_write_matrix()`, `.h5_write_data_array()`). Intended to be run on the **temporary** copy of a backing file, right before it is atomically swapped into place, so that a corrupted write is caught before it ever becomes "the backup".


