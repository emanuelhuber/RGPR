# Arc-tangent

```r
atan2(y, x)

## S4 method for signature 'GPRvirtual,ANY'
atan2(y, x)

## S4 method for signature 'GPRvirtual,GPRvirtual'
atan2(y, x)

## S4 method for signature 'ANY,GPRvirtual'
atan2(y, x)
```

## Arguments

- `y`: (`GPR|numeric|matrix|complex`)
- `x`: (`GPR|numeric|matrix|complex`)

## Returns

(`GPR`)

The arc-tangent of two arguments atan2(y, x) returns the angle between the x-axis and the vector from the origin to (x, y), i.e., for positive arguments atan2(y, x) == atan(y/x).