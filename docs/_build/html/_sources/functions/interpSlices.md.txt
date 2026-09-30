# Interpolate horizontal slices

```r
interpSlices(
  obj,
  dx = NULL,
  dy = NULL,
  dz = NULL,
  zlim = NULL,
  vz = NULL,
  h = 6,
  extend = c("bbox", "obbox", "chull", "buffer"),
  bufferDist = NULL,
  shp = NULL,
  rot = FALSE,
  verbose = TRUE,
  estimate = FALSE,
  hdf5 = c("auto", "always", "never"),
  dsn = NULL,
  compress = 5L,
  overwrite = FALSE,
  mem_threshold_mb = 500,
  batch_size = NULL
)

## S4 method for signature 'GPRsurvey'
interpSlices(
  obj,
  dx = NULL,
  dy = NULL,
  dz = NULL,
  zlim = NULL,
  vz = NULL,
  h = 6,
  extend = c("bbox", "obbox", "chull", "buffer"),
  bufferDist = NULL,
  shp = NULL,
  rot = FALSE,
  verbose = TRUE,
  estimate = FALSE,
  hdf5 = c("auto", "always", "never"),
  dsn = NULL,
  compress = 5L,
  overwrite = FALSE,
  mem_threshold_mb = 500,
  batch_size = NULL
)
```

## Arguments

- `obj`: (`GPRsurvey`)
- `dx`: (`numeric[1]`) x-resolution
- `dy`: (`numeric[1]`) y-resolution
- `dz`: (`numeric[1]`) z-resolution. Ignored if `vz` is given. One of `dz` or `vz` must be provided.
- `zlim`: (`numeric[2]|NULL`) `c(minz, maxz)`: restrict the target depth/time slices to this range. Applies whether the slice vector comes from `dz` or is given directly via `vz` -- either way, values outside `[min(zlim), max(zlim)]` are dropped. `NULL` (default) keeps the full range.
- `vz`: (`numeric[n]|NULL`) Explicit vector of target depths/times for the slices, bypassing `dz`-based generation entirely (the slices need not be evenly spaced). One of `dz` or `vz` must be provided. `NULL` (default) derives the vector from `dz`.
- `h`: (`numeric[1]`) FIXME: Number of levels in MBA hierarchy (see function...)
- `extend`: (`character[1]`) FIXME: Method to define interpolation extent.
- `bufferDist`: (`numeric[1]`) FIXME: Buffer distance around survey lines.
- `shp`: (`matrix[n,2]|list[2]|sf`) FIXME: Shape/polygon defining interpolation bounds.
- `rot`: (`logical[1]|numeric[1]`) If `TRUE` the GPR lines are fist rotated such to minimise their axis-aligned bounding box. If `rot` is numeric, the GPR lines is rotated first rotated by `rot` (in radian).
- `verbose`: (`logical[1]`) If TRUE, verbose.
- `hdf5`: (`character[1]`) Whether the resulting `GPRcube` should be backed by an HDF5 file rather than held fully in memory: `"auto"` (default) decides based on `mem_threshold_mb`, `"always"` forces HDF5 backing, `"never"` forces an in-memory array. Ignored when the result is a `GPRslice` (a single slice is always small enough to keep in memory). See Details.
- `dsn`: (`character[1]|NULL`) Destination path for the HDF5 backing file when `hdf5` results in HDF5 backing. If `NULL`, a temporary file is created (see `base::tempfile()`) -- move it with `writeGPR()` if you want to keep it beyond the session.
- `compress`: (`integer[1]`) gzip compression level (0-9) for the HDF5 backing file; `0` disables compression. Ignored for in-memory results.
- `overwrite`: (`logical[1]`) Overwrite `dsn` if it already exists?
- `mem_threshold_mb`: (`numeric[1]`) When `hdf5 = "auto"`, the estimated cube size (see `estimate = TRUE`) above which HDF5 backing is used instead of an in-memory array.
- `batch_size`: (`integer[1]|NULL`) Number of depth slices computed per parallel batch before being written out / accumulated. If `NULL`, a size is chosen automatically so that one batch stays under ~300 MB. Smaller values bound peak memory more tightly at the cost of more scheduling overhead.

## Returns

(`GPRcube|GPRslice`)

## Description

Interpolate horizontal slices

## Memory and HDF5 backing

Depth-slice interpolation (MBA) is embarrassingly parallel across slices, so slices are computed in batches via future.apply::future_lapply , bounding peak memory during computation to roughly one batch regardless of the number of depth slices. When the resulting cube is large (`hdf5 = "auto"` and estimated size > `mem_threshold_mb`, or `hdf5 = "always"`), each batch is written directly to a chunked, checksummed HDF5 file as it is computed instead of being accumulated in an R array; the returned `GPRcube` then has `data = array(dim = c(0,0,0))` and `path` pointing at that HDF5 file (see `loadCube()` to pull the full array back into memory when needed).


