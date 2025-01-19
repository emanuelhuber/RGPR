# Angle of the GPR data

```r
angle(x)

## S4 method for signature 'GPR'
angle(x)

## S4 method for signature 'GPRsurvey'
angle(x)
```

## Arguments

- `x`: (`GPR|GPRsurvey`)

## Returns

(`numeric[1]`) The angle of the oriented bounding box.

The angle is computed based on the orientation of the oriented bounding box (`obbox()`).