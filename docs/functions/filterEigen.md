# Eigenimage filter

```r
filterEigen(obj, eigenvalue = NA, center = TRUE, scale = FALSE, track = TRUE)

## S4 method for signature 'GPR'
filterEigen(obj, eigenvalue = NA, center = TRUE, scale = FALSE, track = TRUE)
```

## Arguments

- `obj`: An object of the class GPR
- `eigenvalue`: An integer vector specifying the eigenvalues selected. `eigenvalue = 1:5` returns the image reconstructed using the first five eigenvalues while `c(2,4,6)` will return the image reconstructed for these three specific eigenvalues. If `NA`, a plot of the eigenvalues spectrum will be displayed and the user will be prompted to enter the pair of selected eigenvalues separated by a comma. That is for example `1,2` will return the image reconstructred using tthe first two eigenvalues. If `NULL`, the image is reconstructed using all eigenvalues. The number of eigenvalue is equal to `min(dim(x))`.
- `center`: Logical or numeric. If `TRUE`, centering is done by subtracting the layer means (omitting `NA`'s), and if `FALSE`, no centering is done. If center is a numeric vector with length equal to the `nlayers(x)`, then each layer of `x` has the corresponding value from center subtracted from it.
- `scale`: Logical or numeric. If `TRUE`, scaling is done by dividing the (centered) layers of `x` by their standard deviations if center is `TRUE`, and the root mean square otherwise. If scale is `FALSE`, no scaling is done. If scale is a numeric vector with length equal to `nlayers(x)`, each layer of `obj` is divided by the corresponding value. Scaling is done after centering.

## Returns

An object of the class GPR.

## Description

Decompose the GPR data (radargram) using singular value decomposition (SVD) and return the reconstructed data for the selected singular values (eigenvalues). This method is sometimes refered to in the litterature as the Karhunen-Loeve (KL) transformation or the eigenimage decomposition.

## References

 * Textbook: Sacchi (2002) Statistical and Transform Methods in Geophysical Signal Processing


