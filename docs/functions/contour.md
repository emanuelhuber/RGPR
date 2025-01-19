# Contours

```r
## S3 method for class 'GPR'
contour(
  x,
  col = NULL,
  horiz = TRUE,
  interpolate = TRUE,
  sym = TRUE,
  clim = NULL,
  add = FALSE,
  asp = NA,
  secaxis = TRUE,
  elev = FALSE,
  export = NULL,
  fac = 1,
  wiggles = list(side = 1, size = 0.2, col = "black", lwd = 0.5),
  markers = list(lineSymbols = 0.35, pch = 25, colSymbols = "red", bgSymbols = "yellow",
    cexSymbols = 1, lineText = 0.9, cexText = 0.6, colText = "red"),
  ann = list(lineText = 1, colLine = "red", colText = "red", cexText = 0.75, lwd = 1),
  z0 = list(lwd = 1, col = "green", lty = 1),
  cbar = list(w = 1, pos = 1, hst = 0.5, fticks = 0.5, vclab = 0.5, clab = NULL),
  ...
)
```

`contour` extends `plot3D::contour2D` and creates a contour plot.