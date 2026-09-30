# Write a 2-D matrix as a chunked, checksummed HDF5 dataset

```r
.h5_write_matrix(grp, name, data, compress = 0L)
```

## Arguments

- `grp`: An open, writable `hdf5r` group.
- `name`: (`character(1)`) Dataset name.
- `data`: A matrix. If `NULL`, zero-length, or has zero rows, nothing is written.
- `compress`: (`integer(1)`) gzip level 0-9; `0` disables compression.

## Description

Write a 2-D matrix as a chunked, checksummed HDF5 dataset


