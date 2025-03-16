# Get velocity model as GPR object

```r
getVel(obj, type = c("vint", "vrms"))

## S4 method for signature 'GPR'
getVel(obj, type = c("vint", "vrms"))
```

## Arguments

- `obj`: (`GPR class`) An object of the class `GPR`
- `type`: (`vrm|vint`) Set the velocity you want (either root-mean square or internal velocity)

## Returns

(`GPR class`) An object of the class `GPR` containing the velocity model.

Return the velocity model (either the root-mean square or internal velocity).