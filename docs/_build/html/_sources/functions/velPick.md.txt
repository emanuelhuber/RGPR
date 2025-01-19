# Pick velocity interactively

```r
velPick(x, ...)

## S4 method for signature 'GPR'
velPick(x, ...)
```

## Arguments

- `x`: (`GPR class`) An object of the class `GPR`
- `...`: additional graphics parameters used if type != "n" for plotting the locations

## Returns

(`GPR class`) An object of the class `GPR`.

Pick velocity interactively on a previously plotted velocity spectrum. The picked velocity are NMO velocities which are a good approximation for RMS velocities. The internal velocities are estimated from the NMO velocities through the Dix's formula `velPick()` add the NMO velocities as RMS velocity as well as the internal velocities to `x`.

## Details

Application of the Dix's formula can provide non-real velocities, if the travel time intervals are small or if the NMO velocity change is large. In this case, this