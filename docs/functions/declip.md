# Declip the GPR signal

```r
declip(obj, drange = NULL, lambda = 1, mu = 0, objclip = NULL)

## S4 method for signature 'GPR'
declip(obj, drange = NULL, lambda = 1, mu = 0, objclip = NULL)
```

## Arguments

- `obj`: (`GPR`)
- `drange`: (`numeric[2]`) the desired range of the declipped values (for the negatively and positively clipped values) used as a soft constraint whose strength depends on the value of `lambda`. If `drange = NULL`, the desired range is set equal to the clipped data range +/- 10 percent.
- `lambda`: (`numeric[1]`) Positive value defining the strength of signal range constraint. The larger `lamda` is the larger the constraint on the signal range. If `lambda = 0`, there is no constraint on the data range and `drange` is not used.
- `mu`: (`numeric[1]`) Positive value that add some noise to stabilize the matrix inversion used in the least-square approach (could be useful if the matrix to invert is singular, use small value, e.g., 0.00001 of the signal amplitude).
- `objclip`: (`matrix[m,n]`) The clipped values, a matrix with the Same dimension as `obj`, with `1` for the positively clipped values, `-1` for the negatively clipped values and `0` everywhere else. If `objclip = NULL`, the clipped values are estimated with the function `clippedData()`.

## Returns

(`GPR`)

Use constrained least squares. Based on the code of Ivan Selesnick: we minimize the energy of the the third derivative. This encourages the filled in data to have the form of a parabola (second order polynomial), because the third derivative of a parabola is zero.".

## Details

M. J. Harvilla and R. M. Stern, "Efficient audio declipping using regularized least squares," 2015 IEEE International Conference on Acoustics, Speech and Signal Processing (ICASSP), South Brisbane, QLD, Australia, 2015, pp. 221-225, doi: 10.1109/ICASSP.2015.7177964.