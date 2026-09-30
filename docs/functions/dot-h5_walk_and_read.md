# Recursively read every dataset under an HDF5 group

```r
.h5_walk_and_read(grp, verbose = FALSE)
```

## Description

Reading a dataset that was written with the fletcher32 filter forces HDF5 to validate its checksum; a mismatch raises an error. This function's only purpose is to trigger that validation for every dataset in the file.


