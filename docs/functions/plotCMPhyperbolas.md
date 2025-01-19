# Plot hyperbolas associated with Vrms velociies

```r
plotCMPhyperbolas(x, ...)

## S4 method for signature 'GPR'
plotCMPhyperbolas(x, lty = 1, lwd = 1, type = "l", ...)
```

## Arguments

- `x`: (`GPR class`) A CMP object of the class `GPR`
- `...`: see arguments of `graphics::matplot()`
- `lty`: (`numeric[1]`) The line type (default is `1` for solid lines). See `graphics::par()`
- `lwd`: (`numeric[1]`) Line width (default is `1`). See `graphics::par()`
- `type`: (`character[1]`) what type of plot should be drawn (default is `"l"` for lines). See `graphics::plot()`

## Returns

(`list`) A list element key `antsep` containing a numeric vector of `n` antenna separation values, and element `twt`

containing a `n \times m` matrix, where `m` is the number of hyperbolas (i.e., the number of velocities).

Plot hyperbolas associated with root-mean-square velocities stored in the CMP data.