# Write the GPRsurvey objects in VTK format.

```r
writeVTK(obj, dsn = NULL, overwrite = TRUE)

## S4 method for signature 'GPR'
writeVTK(obj, dsn = NULL, overwrite = FALSE)

## S4 method for signature 'GPRsurvey'
writeVTK(obj, dsn = NULL, overwrite = FALSE)
```

## Arguments

- `obj`: Object of the class `GPR` or `GPRsurvey`
- `dsn`: Filepath (Length-one character vector). If `dsn = NULL`, the file will be save in the current working directory with the name of obj (`name(obj)`) with the extension depending of `format`.
- `overwrite`: Boolean. If `TRUE` existing files will be overwritten, if `FALSE` an error will be thrown if the file(s) already exist(s).

## Description

Write the GPRsurvey objects in VTK format.

## See Also

`readGPR()`


