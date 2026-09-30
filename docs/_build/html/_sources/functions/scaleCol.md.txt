# Scale the columns of a numeric matrix

```r
scaleCol(
  A,
  type = c("stat", "min-max", "95", "eq", "sum", "rms", "mad", "invNormal")
)
```

## Arguments

- `A`: A numeric matrix or an object coercible to a numeric matrix. Columns are scaled independently.
- `type`: Character or numeric scalar defining the scaling method. Available character methods are:
    
    - **`"stat"`**: Standardize each column by subtracting its mean and dividing by its standard deviation.
    - **`"min-max"`**: Divide each column by its range, without subtracting the minimum.
    - **`"95"`**: Divide each column by the difference between the 95th and 5th percentiles. Other numeric percentages between 0 and 100 may also be supplied.
    - **`"eq"`**: Apply trace-energy equalization based on the sum of squared amplitudes.
    - **`"sum"`**: Divide each column by the sum of its absolute amplitudes.
    - **`"rms"`**: Divide each column by its root-mean-square amplitude. This corresponds to the scaling factor used by `base::scale()` when `center = FALSE`.
    - **`"mad"`**: Center each column on its median and divide it by its median absolute deviation.
    - **`"invNormal"`**: Apply a rank-based normal-score transformation independently to each column.

## Returns

A numeric matrix with the same dimensions and dimnames as `A`. Non-finite values introduced by undefined scaling factors, such as scaling a constant trace, are replaced by zero. Existing missing values are preserved by the normal-score transformation.

## Description

Applies a selected scaling or normalization method independently to each column of a numeric matrix. In the context of GPR data, each column is generally interpreted as an individual trace.

## Details

The normal-score transformation (`type = "invNormal"`) replaces the empirical distribution of each trace with an approximately Gaussian distribution while preserving the original mean and standard deviation. Equal input values receive equal transformed values.

A numeric value between 0 and 100 can also be supplied through `type`. In that case, each column is divided by the difference between two complementary quantiles. For example, `"95"` uses the difference between the 95th and 5th percentiles.

Scaling is performed independently for each column.

For `type = "invNormal"`, average ranks are assigned to tied amplitudes. Consequently, identical input amplitudes receive identical normal scores. This avoids interpolation warnings caused by duplicated amplitude values.

The normal-score transformation is nonlinear. It changes relative amplitudes within a trace and should therefore be used cautiously when amplitudes have a physical interpretation. It is generally more appropriate for visualization or distribution normalization than for amplitude-preserving processing.

## Examples

```r
A <- matrix(
c(
1, 1, 2, 3, 4,
2, 3, 4, 5, 6
),
nrow = 5,
ncol = 2
)

# Standardize each column
scaleCol(A, type = "stat")

# Divide each column by its amplitude range
scaleCol(A, type = "min-max")

# Rank-based normal-score transformation
scaleCol(A, type = "invNormal")

# Scale using the difference between the 90th and 10th percentiles
scaleCol(A, type = "90")
```

## See Also

`base::scale()`, `stats::quantile()`


