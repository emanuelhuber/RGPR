# Write the main GPR data array (radargram) as 64-bit float, chunked and checksummed, with optional gzip compression

```r
.h5_write_data_array(grp, gpr, compress = 0L)
```

## Arguments

- `grp`: The (already created) HDF5 group for this line, i.e. `/lines/<name>`.
- `gpr`: Object of class `GPR`.
- `compress`: (`integer(1)`) gzip level 0-9; `0` disables compression. See the package documentation / `?GPRsurvey` for guidance on whether compression is worth it for GPR data.

## Description

Unlike the earlier version of this code, the dataset type is always `H5T_NATIVE_DOUBLE` (64-bit) -- the same precision R itself uses for numeric vectors -- so writing to HDF5 never loses precision relative to the in-memory `GPR` object.


