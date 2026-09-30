 class

# Class GPRcube

```r
## S4 method for signature 'GPRcube'
dim(x)
```

## Description

An S4 class to represent 3D ground-penetrating radar (GPR) data. Array of dimension `n \times m \times p` (`n` samples, `m` traces or A-scans along `x`, and `p` traces or A-scans along `y`), with horizontal grid cell sizes `dx` and `dy`, and slices at the depths/times given by `z` (which need not be evenly spaced). We assume that the unit along y is the same as the unit along x.

## Slots

- **`dx`**: (`numeric[1]`) Grid cell size along x
- **`dy`**: (`numeric[1]`) Grid cell size along y
- **`z`**: (`numeric[p]`) Depth/time of each of the `p` slices (the cube's 3rd data dimension), in `@zunit`. Slices need not be evenly spaced, but `z` must be **strictly monotonic**
       
       from top to bottom: strictly increasing (small to large) when `@zunit` is a time unit (see `isZTime()`), strictly decreasing (large to small) when it's a depth/elevation unit -- enforced by this class's validity check (see `setValidity()` below). `length(z)` must match the size of the cube's 3rd dimension.
- **`ylab`**: (`character[1|p]`) Label of `y`.
- **`center`**: (`numeric[3]`) Coordinates of the bottom left grid corner; the 3rd element is redundant with (and should equal) `z[1]`, kept for backward compatibility.
- **`rot`**: (`numeric[1]`) Rotation angle
- **`view`**: (`logical[1]`) `TRUE` if this object is an unmaterialized view: a subset of an HDF5-backed cube (see `isH5Backed()`) obtained via `[`. A view's `@data` is empty and `@path` still points at the **original** backing file -- nothing is read from disk until it's actually needed (see `ClassGPRcube_hdf5.R`). Call `materialize()` to write a view's data to its own independent HDF5 file, or `loadCube()` to pull just the view's region into memory. Always `FALSE` for an in-memory cube or a "whole", non-view HDF5-backed cube.
- **`viewIdx`**: (`list`) Only meaningful when `view = TRUE`: `list(i = integer, j = integer, k = integer)`, the indices into the **original** HDF5 dataset at `@path` that this view represents. Composed (not overwritten) when a view is subsetted again, so `x[1:10,,][ ,1:5, ]` still resolves back to `x`'s original backing file correctly. Empty list when `view = FALSE`.


