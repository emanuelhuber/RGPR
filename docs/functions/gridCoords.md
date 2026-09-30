# Get or set grid coordinates

```r
gridCoords(x, value)

gridCoords(x) <- value

## S4 replacement method for signature 'GPRsurvey'
gridCoords(x) <- value
```

## Arguments

- `x`: A `GPRsurvey` object.
- `value`: A named list describing the grid geometry. It may contain:
    
    - **`xlines`**: Integer indices of lines having a constant x-coordinate.
    - **`ylines`**: Integer indices of lines having a constant y-coordinate.
    - **`x`**: Constant x-coordinate for every line in `xlines`.
    - **`y`**: Constant y-coordinate for every line in `ylines`.
    - **`xstart`**: Optional starting y-coordinate for every line in `xlines`. Defaults to zero.
    - **`ystart`**: Optional starting x-coordinate for every line in `ylines`. Defaults to zero.
    - **`xlength`**: Optional ending y-coordinate for every line in `xlines`. If omitted, the original trace positions are read from HDF5.
    - **`ylength`**: Optional ending x-coordinate for every line in `ylines`. If omitted, the original trace positions are read from HDF5.
    - **`xreverse`**: Optional logical vector indicating whether the trace direction of each line in `xlines` should be reversed. Defaults to `FALSE`.
    - **`yreverse`**: Optional logical vector indicating whether the trace direction of each line in `ylines` should be reversed. Defaults to `FALSE`.
    
    The vectors associated with `xlines` must have the same length as `xlines`. Likewise, the vectors associated with `ylines` must have the same length as `ylines`.
    
    Line indices must be unique, valid indices into the survey, and cannot occur in both `xlines` and `ylines`.

## Returns

`gridCoords(x)` returns the grid coordinates associated with `x`. The replacement form returns the modified `GPRsurvey` object.

## Description

Get or set grid coordinates for the traces of a `GPRsurvey` object.

## Details

Grid lines are divided into two groups:

 * `xlines`: survey lines with a constant x-coordinate and varying y-coordinates.
 * `ylines`: survey lines with varying x-coordinates and a constant y-coordinate.

When `xlength` or `ylength` is supplied, trace coordinates are generated as an evenly spaced sequence between the corresponding start and end coordinates. Otherwise, the original trace-position vectors are read from the HDF5 backing file and shifted by `xstart` or `ystart`.

Coordinates cannot be modified while the survey is an HDF5 view. Use `materialize()` first to create an independent, writable survey.

## Examples

```r
## Not run:

gridCoords(SU) <- list(
  xlines  = 1:10,
  x       = seq(0, by = 2, length.out = 10),
  xstart  = rep(0, 10),
  xlength = rep(20, 10),
  ylines  = 11:15,
  y       = seq(0, by = 2, length.out = 5),
  ystart  = rep(0, 5),
  ylength = rep(18, 5)
)
## End(Not run)
```


