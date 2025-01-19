# x-position

```r
xpos(x)

xpos(x) <- value

## S4 method for signature 'GPR'
xpos(x)

## S4 replacement method for signature 'GPR'
xpos(x) <- value
```

## Arguments

- `x`: (`GPR`) An object of the class GPR.
- `value`: (`numeric[m]`)

## Returns

(`GPR`)

Antenna separation distance(s)

## Details

Modified slots

 * `x` the x-position
 * `xpos` the antenna separation distance only for CMP/WARR (plot twt as a function of antenna separation)