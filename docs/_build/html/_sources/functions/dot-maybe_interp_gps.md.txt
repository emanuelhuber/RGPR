# Apply GPS interpolation or store GPS as metadata

```r
.maybe_interp_gps(x, x_gps, dsn, interpGPS, UTM, verbose, ...)
```

## Arguments

- `x`: GPR object.
- `x_gps`: GPS data returned by the reader (sf object or NULL).
- `dsn`: Resolved dsn list (used only for the "no GPS found" warning).
- `interpGPS`: logical(1).
- `UTM`: logical(1) or character(1).
- `verbose`: logical(1).
- `...`: Passed to interpCoords().

## Returns

Updated GPR object.

## Description

Apply GPS interpolation or store GPS as metadata


