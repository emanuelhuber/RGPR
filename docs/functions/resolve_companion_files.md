# Resolve companion file paths for a GPR format

```r
resolve_companion_files(dsn, fPath, fmt)
```

## Arguments

- `dsn`: Named list, slot name (UPPERCASE ext) -> path or connection.
- `fPath`: Named character vector, UPPERCASE ext -> absolute file path.
- `fmt`: Format descriptor from `.GPR_FORMAT_REGISTRY`.

## Returns

Updated `dsn` list with all mandatory + optional slots populated.

## Description

Resolve companion file paths for a GPR format


