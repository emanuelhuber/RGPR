# Serialize an arbitrary R object into an HDF5 dataset

```r
.h5_write_r_object(grp, name, obj)
```

## Arguments

- `grp`: An open, writable `hdf5r` group.
- `name`: (`character(1)`) Dataset name.
- `obj`: Any R object understood by `base::serialize()`.

## Description

Some slots -- notably `GPR@md`, the raw manufacturer metadata -- can contain nested lists, `NULL`s, factors, or other structures that don't map cleanly onto native HDF5 types. Rather than dropping everything that isn't a length-one atomic scalar (as the previous version of this code did), the **entire** object is serialized with `base::serialize()` into a raw vector, stored as a 1-D array of unsigned bytes (0-255), and can be perfectly reconstructed with `.h5_read_r_object()`.

## Details

This is intentionally opaque to generic HDF5 tools (h5dump, HDFView, h5py, ...) -- it is a backup mechanism, not a browsing format. See `.write_GPR_line_hdf5()` for how this is paired with a second, human-readable (but lossy) flattened copy for browsability.


