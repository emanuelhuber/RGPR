# Read GPS file with GPGGA string

```r
readGPGGA(dsn, sep = ",", returnSf = TRUE)
```

## Arguments

- `dsn`: (`character(1)|connection object`) Data source name: either the filepath to the GPR data (character), or an open file connection.
- `sep`: (`character(1)`) The field separator character (see`read.table()`).
- `returnSf`: (`logical(1)`) If `TRUE` returns an object of class `sf`. If `FALSE` returns a `data.frame`.

## Returns

(`SpatialPoints`)

Read GPS file with GPGGA string