# Set grid coordinates the trace position.

```r
gridCoords(x, value)

gridCoords(x) <- value

## S4 replacement method for signature 'GPRsurvey'
gridCoords(x) <- value
```

## Arguments

- `x`: An object of the class GPRsurvey
- `value`: A list with following elements: `xlines` (number or id of the GPR data along the x-coordinates), `ylines` (number or id of the GPR data along the y-coordinates), `x`
    
    (position of the x-GPR data on the x-axis), `x` (position of the y-GPR data on the y-axis)

Set grid coordinates to a survey