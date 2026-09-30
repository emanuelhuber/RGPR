# Persist changed grid coordinates

```r
.finalize_gridCoords_GPRsurvey(x, changed_ids)
```

## Arguments

- `x`: A `GPRsurvey` object whose in-memory coordinates have already been updated.
- `changed_ids`: Integer indices into `x@names` identifying the lines whose coordinates changed.

## Returns

The updated `GPRsurvey` object.

## Description

Finalize a grid-coordinate update after all requested coordinate assignments have been applied to the in-memory survey object.

## Details

The function performs the following operations:

1. Removes duplicated, missing, and invalid changed-line indices.
2. Recomputes survey intersections once, after all coordinate changes are complete.
3. Persists the changed coordinates and recomputed intersections in a single HDF5 update transaction.

Updating coordinates and intersections together prevents the two datasets from becoming inconsistent if an error occurs during persistence.


