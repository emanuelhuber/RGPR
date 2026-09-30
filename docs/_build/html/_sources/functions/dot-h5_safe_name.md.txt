# Sanitize a string for use as an HDF5 group/dataset name

```r
.h5_safe_name(name)
```

## Arguments

- `name`: (`character(1)`) Proposed name.

## Returns

(`character(1)`) `name` with `/`, `\\`, and control characters replaced by `_`. Falls back to `"default_name"` if `name` is empty, `NA`, or becomes empty after trimming.

## Description

HDF5 uses `/` as a path separator -- including inside a single `H5Group$create_group(name)`/`create_dataset(name, ...)` call. A name containing `/` is therefore **not** created as one literal group/dataset; HDF5 tries to traverse into a nested path implied by the slash (e.g. `"2024/03/15"` is read as "create `03/15` inside existing group `2024`"), and fails with a traversal error if that parent path doesn't already exist.

## Details

Line groups themselves no longer need this (see the positional-id scheme above), but a handful of places still turn an arbitrary, caller-supplied string directly into an HDF5 name -- notably the flattened, human-browsable copy of `GPR@md` keys in `.write_GPR_line_hdf5()` (`writeHDF5.R`). This function protects those.


