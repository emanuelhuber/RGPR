# Write the GPR object in a file.

```r
writeGPR(
  x,
  fPath = NULL,
  format = c("rds", "dt1", "ascii", "xta", "xyza"),
  overwrite = FALSE,
  ...
)

## S4 method for signature 'GPR'
writeGPR(
  x,
  fPath = NULL,
  format = c("rds", "dt1", "ascii", "xta", "xyza"),
  overwrite = FALSE,
  ...
)

## S4 method for signature 'GPRsurvey'
writeGPR(
  x,
  fPath = NULL,
  format = c("DT1", "rds", "ASCII", "xta", "xyzv"),
  overwrite = FALSE,
  ...
)
```

## Arguments

- `x`: Object of the class `GPR` or `GPRsurvey`
- `fPath`: Filepath (Length-one character vector). If `fPath = NULL`, the file will be save in the current working directory with the name of x (`name(x)`) with the extension depending of `format`.
- `format`: Format type. See Details.
- `overwrite`: Boolean. If `TRUE` existing files will be overwritten, if `FALSE` an error will be thrown if the file(s) already exist(s).
- `...`: additional parameters to be passed to `write.table()`
    
    when `format = "ASCII"` or `format = "xyza"`.

Write the GPR object in a file.

## See Also

`readGPR()`