# Write a 1-D vector as a chunked, checksummed HDF5 dataset

```r
.h5_write_vector(grp, name, dta, compress = 0L)
```

## Arguments

- `grp`: An open, writable `hdf5r` group.
- `name`: (`character(1)`) Dataset name.
- `dta`: Atomic vector to write. If `length(data) == 0`, nothing is written (this keeps optional fields such as `z0` or `transf` absent from the file rather than present-but-empty).
- `compress`: (`integer(1)`) gzip level 0-9; `0` disables compression.

## Description

Every dataset gets the fletcher32 checksum filter (HDF5 requires chunked storage for this, which is why even tiny metadata vectors are chunked). Compression is off by default (`compress = 0`) because it is rarely worth the CPU cost for small metadata arrays; pass a gzip level for larger vectors such as per-line coordinates if desired.


