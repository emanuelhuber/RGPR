# Compute the opaque HDF5 group id for a line, from its position

```r
.h5_line_group_id(i)
```

## Arguments

- `i`: (`integer`) 1-based position(s) in `x@names` (and equivalently in `/survey/names`, `/survey/nz`, etc. -- every per-line dataset is ordered the same way).

## Returns

(`character`) e.g. `"line000001"`.

## Description

Compute the opaque HDF5 group id for a line, from its position


