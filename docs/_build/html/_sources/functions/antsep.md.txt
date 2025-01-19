# Antenna separation distance(s)

```r
antsep(x)

antsep(x) <- value

## S4 method for signature 'GPR'
antsep(x)

## S4 replacement method for signature 'GPR'
antsep(x) <- value
```

## Arguments

- `x`: (`GPR`) An object of the class GPR.
- `value`: (`numeric[1]|numeric[m]`)

## Returns

(`GPR`)

Antenna separation distance(s)

## Details

Modified slots

 * `antsep` the antenna separation distance
 * `x` the x-position only for CMP/WARR (plot twt as a function of antenna separation)