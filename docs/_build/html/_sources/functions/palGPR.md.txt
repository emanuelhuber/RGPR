# Color palettes for GPR data

```r
palGPR(pal = "default", n = 101, power = 1, returnNames = FALSE)

palPlot(col, border = NA)

palDisplay()
```

## Arguments

- `pal`: (`character[1]`) Name of the color palette.
- `n`: (`integer[1]`) Number of colors to be in the palette.
- `power`: (`integer[1]`) Control parameter determining how chroma should be increased (1 = linear, 2 = quadratic, etc.).
- `returnNames`: (`logical[1]`) If `TRUE`, returns only the color palette names.
- `col`: (`character`) Colors to be plotted.
- `border`: (`character`) color for rectangle border(s). The default means par("fg"). Use border = NA to omit borders. If there are shading lines, border = TRUE means use the same colour for the border as for the shading lines.

source: vignette of the R-package "colorspace" (Color Space Manipulation)

## Examples

```r
palPlot(palGPR("hcl_5"))
palDisplay()
```