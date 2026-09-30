# Shift trace positions of one GPR data

```r
shiftGridLine(obj, i, dx = 0, dy = 0, dz = 0)

## S4 method for signature 'GPRsurvey'
shiftGridLine(obj, i, dx = 0, dy = 0, dz = 0)
```

## Arguments

- `obj`: (`GPRsurvey`) Index of the line to be shifted
- `i`: (`numeric[1]`) Index of the line to be shifted
- `dx`: (`numeric[1]`) How much should the line be shifted in the x-direction (in spatial unit).
- `dy`: (`numeric[1]`) How much should the line be shifted in the y-direction (in spatial unit).
- `dz`: (`numeric[1]`) How much should the line be shifted in the z-direction (in spatial unit).

## Description

Shift trace positions of GPR data `i` by `dx` along x-axis and by `dy` along y-axis.


