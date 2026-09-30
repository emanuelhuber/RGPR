# Build a temporary path in the same directory as `dsn`

```r
.h5_temp_path(dsn)
```

## Description

Using the same directory guarantees `file.rename()` is an atomic, same-filesystem operation when the file is later swapped into place.


