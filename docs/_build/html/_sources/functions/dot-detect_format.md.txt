# Detect the format of a supplied set of file extensions

```r
.detect_format(ext_vec)
```

## Arguments

- `ext_vec`: (`character`) Uppercase extensions extracted from `dsn`.

## Returns

The matching format descriptor list, or `NULL` if none matched.

## Description

Iterates over the registry in insertion order and returns the first descriptor whose `detect_ext` intersects with `ext_vec`.


