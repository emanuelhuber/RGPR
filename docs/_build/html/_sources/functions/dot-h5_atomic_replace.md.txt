# Atomically replace `dsn` with `tmp`

```r
.h5_atomic_replace(tmp, dsn)
```

## Description

Tries `file.rename()` first (atomic, instantaneous, same filesystem). Falls back to copy+remove only if the rename fails (e.g. `tmp` and `dsn` end up on different filesystems/mounts for some reason) -- in that case the operation is no longer atomic, so this is a best-effort fallback, not the primary path.


