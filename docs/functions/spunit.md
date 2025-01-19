# Spatial unit

```r
spunit(x)

spunit(x) <- value

## S4 method for signature 'GPR'
spunit(x)

## S4 replacement method for signature 'GPR'
spunit(x) <- value

## S4 method for signature 'GPRsurvey'
spunit(x)

## S4 replacement method for signature 'GPRsurvey'
spunit(x) <- value
```

## Arguments

- `x`: (`GPR class`) An object of the class `GPR`
- `value`: (`character[1]`) Spatial units: "m", "feet", etc. see units package.

## Returns

(`GPR class`) An object of the class `GPR`

Spatial unit of the trace coordinates