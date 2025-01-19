# Form Row and Column Sums and Means

```r
## S4 method for signature 'GPRvirtual'
colSums(x, na.rm = FALSE, dims = 1)

## S4 method for signature 'GPRvirtual'
rowSums(x, na.rm = FALSE, dims = 1)

## S4 method for signature 'GPRvirtual'
colMeans(x, na.rm = FALSE, dims = 1)

## S4 method for signature 'GPRvirtual'
rowMeans(x, na.rm = FALSE, dims = 1)
```

## Arguments

- `x`: (`GPR`)
- `na.rm`: (`logical[1]`). Should missing values (including `NaN`) be omitted from the calculations?
- `dims`: (`integer[1]`) Which dimensions are regarded as ‘rows’ or ‘columns’ to sum over.(see `colSums()`).

Form row and column sums and means