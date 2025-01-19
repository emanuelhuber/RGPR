# Get properties of ASCII file to read it with `read.table`

```r
detectASCIIProp(dsn, lns = 20, verbose = TRUE)
```

## Arguments

- `dsn`: (character) File path or connection
- `lns`: (numeric) Number of lines to read to get the properties of the ASCII file
- `verbose`: (boolean) If `TRUE` print messages allowed.

## Returns

1. header, 2) skip, 3)

To get header, separator, column with na values, etc.

## Details

don't forget to skip blank line when reading dsn