# Set and get velocity

```r
vel(x)

## S4 method for signature 'GPR'
vel(x)

vel(x) <- value

## S4 replacement method for signature 'GPR'
vel(x) <- value
```

## Arguments

- `x`: (`GPR class`) An object of the class `GPR`
- `value`: (`[numeric[1]|numeric[m]|matrix[m,n]|list`) The velocity model, see section Details

## Returns

(`list|numeric|matrix`) The velocities as they are stored in `x`.

Set and get velocity model

## Details

The argument `value` can be

 * a scalar (length-one vector) for uniform velocity
 * a vector of length equal to the sample number (row number) of x (`m`).
 * a matrix of dimension equal to the sample and trace numner (row and column number) of x (`m` `\times` `n`).
 * a list with elements `t` and `v` having the same length. `t` defines the lower time boundaries of the velocities `v`. FIXME: "intp", "smooth"