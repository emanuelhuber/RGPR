# Get velocity model as GPR object

```r
getVel(x, type = c("vrms", "vint"))

## S4 method for signature 'GPR'
getVel(x, type = c("vrms", "vint"))
```

## Arguments

- `x`: (`GPR class`) An object of the class `GPR`
- `type`: (`vrm|vint`) Set the velocity you want (either root-mean square or internal velocity)

## Returns

(`GPR class`) An object of the class `GPR` containing the velocity model.

Return the velocity model (either the root-mean square or internal velocity).