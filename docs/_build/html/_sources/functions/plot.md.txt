# Plot GPR data

```r
## S3 method for class 'GPR'
plot(
  x,
  col = NULL,
  type = NULL,
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

## S3 method for class 'GPRset'
plot(x, ...)

## S3 method for class 'GPRsurvey'
plot(
  x,
  markers = list(pch = 21, col = "black", bg = "red", cex = 0.7),
  ann = list(pch = 1, cex = 0.8, col = "black"),
  dirArrows = list(col = "red", length = 0.1),
  asp = 1,
  add = FALSE,
  ...
)

## S3 method for class 'GPRcube'
plot(x, ...)

## S3 method for class 'GPRslice'
plot(
  x,
  type = "raster",
  col = NULL,
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
    cexSymbols = 1, lineText = 0.9, cexText = 0.6),
  ann = list(lineText = 1.7, colLine = "red", colText = "red", cexText = 0.9, lwd = 1),
  z0 = list(lwd = 1, col = "green", lty = 1),
  cbar = list(w = 1, pos = 1, hst = 0.5, fticks = 0.5, vclab = 0.5, clab = NULL),
  ...
)
```

## Arguments

- `x`: (`class GPR`)
- `col`: Color palette. See details.
- `type`: (`character[1]`) Plot type. See details
- `horiz`: (`logical[1]`) Only for 1D plot. If `TRUE`, the 1D plot is horizontal (else vertical).
- `interpolate`: (`logical[1]`)
- `sym`: (`logical[1]`) if `TRUE` the colorscale is symmetric if the amplitudes vary around zero. FIXME
- `clim`: (`numeric[2]`) The range of the color values, used in the color palette.
- `add`: (`logical[1]`) If `TRUE`, add to current plot.
- `asp`: (`numeric[1]`) The y/x aspect ratio.
- `secaxis`: (`logical[1]`) If `TRUE`, add a secondary axis. If the GPR data was acquired in common-offset mode, a secondary depth axis is added (based on the average wave velocity). Else a secondary time axis is added. Note that we use the Sensors and Software method to plot the depth axis when the data are in time domain: because of the offset between transmitter and receiver, there is an offset between time-zero and depth, the depth axes is squished.
- `elev`: (`logical[1]`) If `TRUE`, plot signal as a function of elevation (datum).
- `export`: (`NULL|character[1]`) If `export` is a filename with png or pdf extension, the plot is exported in a png/pdf with filename equal to `export`.
- `fac`: (`numeric[1]`) Factor to set the size of the pdf (aspect ratio is defined by `asp`).
- `wiggles`: (`list`) Parameter list for plotting wiggles. Only used when `type = "wiggles"`. See details below.
- `markers`: (`list|NULL`) If not `NULL`, display the fiducial markers according to the parameter list. See details below.
- `ann`: (`list|NULL`) If not `NULL`, display the fiducial markers according to the parameter list. See details below.
- `z0`: (`list|NULL`) If not `NULL`, display a line corresponding to time-zero. See details.
- `cbar`: (`list|NULL`) If not `NULL`, display a colorbar. See details
- `...`: additional arguments passed to the plotting methods `graphics::plot()` for 1D plot and `plot3D::image2D()` for 2D plot. See also `details`.
- `dirArrows`: (`list|NULL`) If not `NULL`, display an arrow indicating the survey direction (only for plot of GPRsurvey) data.

Nice function to plot data. If the GPR object consists of a single trace, wiggle plot is shown. For CMP, the position of the traces on the x-axis is defined by the antenna separation (`antsep(x)`).

`contour` extends `plot3D::contour2D` and creates a contour plot.

## Details

The argument `col` is :

 * 1D plot: a single color
 * 2D plot: a color palette. Default = `palGPR()`

The argument `type` is :

 * 1D plot: `p`, `l`, `b`, `c`, `o`, `h`, `s`, `S`, `n`
   
   (see argument `type` in `graphics::plot()`).
 * 2D plot: `"raster"` (default), `"wiggles"` or `"contour"`.

The argument `wiggles` is a list with following items:


 * `side` Either `1` or `-1` defining on which side the wiggles are drawn.
 * `size` Size of the wiggles (default = 0.2).
 * `col` Color of the wiggles.
 * `lwd` Line thickness.

The argument `markers` is a list with following items:


 * `lineSymbols` Symbol position on margin lines (starting at 0, counting outwards).
 * `pch` plotting ‘character’, i.e., symbol to use. See `graphics::points()`.
 * `colSymbols` Symbol color
 * `bgSymbols` Background color for the open plot symbols given by `pch = 21:25`.
 * `cexSymbols` Symbol expansion: a numerical value.
 * `lineText` Text position on margin lines (starting at 0, counting outwards).
 * `cexText` Character expansion: a numerical value.
 * `colText` Text color.

The argument `ann` is a list with following items:


 * `lineText` Text position on margin lines (starting at 0, counting outwards).
 * `colLine` Line color.
 * `colText` Text color.
 * `cexText` Character expansion: a numerical value.
 * `lwd` Line thickness.

The argument `z0` is a list with following items:


 * `lwd` Line thickness.
 * `col` Line color.
 * `lty` Line type.

The argument `cbar` is a list with following items:


 * `w` colorbar width in lines (default = 1)
 * `pos` left position of the colorbar in lines (default = 1)
 * `hst` space between colorbar and text in lines (default = 0.5)
 * `fticks` length factor for the ticks (default = 0.5)
 * `vclab` length factor for the ticks (if `vclab = 1`, then the tick length is equal to the space between the colorbar and the text.
 * `clab` The label of the colorbar that is plotted above the colorbar (default = NULL, the label is inferred from the GPR data `x`)