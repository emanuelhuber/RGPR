
#' Interpolate horizontal slices
#' 
#' 
#' Interpolate horizontal slices
#' @param obj (`GPRsurvey`)
#' @param dx (`numeric[1]`) x-resolution
#' @param dy (`numeric[1]`) y-resolution
#' @param dz (`numeric[1]`) z-resolution
#' @param h (`numeric[1]`) FIXME: Number of levels in MBA hierarchy (see function...)
#' @param extend (`character[1]`) FIXME: Method to define interpolation extent.
#' @param bufferDist (`numeric[1]`) FIXME: Buffer distance around survey lines.
#' @param shp (`matrix[n,2]|list[2]|sf`) FIXME: Shape/polygon defining interpolation bounds.
#' @param rot (`logical[1]|numeric[1]`) If `TRUE` the GPR lines are 
#'            fist rotated such to minimise their axis-aligned bounding box. 
#'            If `rot` is numeric, the GPR lines is rotated first
#'            rotated by `rot` (in radian).
#' @param verbose (`logical[1]`) If TRUE, verbose.
#' @param hdf5 (`character[1]`) Whether the resulting `GPRcube` should be
#'            backed by an HDF5 file rather than held fully in memory:
#'            `"auto"` (default) decides based on `mem_threshold_mb`,
#'            `"always"` forces HDF5 backing, `"never"` forces an in-memory
#'            array. Ignored when the result is a `GPRslice` (a single
#'            slice is always small enough to keep in memory). See Details.
#' @param dsn (`character[1]|NULL`) Destination path for the HDF5 backing
#'            file when `hdf5` results in HDF5 backing. If `NULL`, a
#'            temporary file is created (see [base::tempfile()]) -- move it
#'            with `writeGPR()` if you want to keep it beyond the session.
#' @param compress (`integer[1]`) gzip compression level (0-9) for the HDF5
#'            backing file; `0` disables compression. Ignored for in-memory
#'            results.
#' @param overwrite (`logical[1]`) Overwrite `dsn` if it already exists?
#' @param mem_threshold_mb (`numeric[1]`) When `hdf5 = "auto"`, the
#'            estimated cube size (see `estimate = TRUE`) above which HDF5
#'            backing is used instead of an in-memory array.
#' @param batch_size (`integer[1]|NULL`) Number of depth slices computed
#'            per parallel batch before being written out / accumulated.
#'            If `NULL`, a size is chosen automatically so that one batch
#'            stays under ~300 MB. Smaller values bound peak memory more
#'            tightly at the cost of more scheduling overhead.
#' @return (`GPRcube|GPRslice`)
#' @details
#' # Memory and HDF5 backing
#' Depth-slice interpolation (MBA) is embarrassingly parallel across
#' slices, so slices are computed in batches via [future.apply::future_lapply],
#' bounding peak memory during computation to roughly one batch regardless
#' of the number of depth slices. When the resulting cube is large
#' (`hdf5 = "auto"` and estimated size > `mem_threshold_mb`, or
#' `hdf5 = "always"`), each batch is written directly to a chunked,
#' checksummed HDF5 file as it is computed instead of being accumulated in
#' an R array; the returned `GPRcube` then has `data = array(dim = c(0,0,0))`
#' and `path` pointing at that HDF5 file (see [RGPR::loadCube()] to pull the full
#' array back into memory when needed).
#' @name interpSlices
#' @rdname interpSlices
#' @export
#' @concept 3D
setGeneric("interpSlices", function(obj, 
                                    dx = NULL, 
                                    dy = NULL, 
                                    dz = NULL, 
                                    h = 6,
                                    extend = c("bbox", "obbox", "chull", "buffer"),
                                    bufferDist = NULL,
                                    shp = NULL,
                                    rot = FALSE, verbose = TRUE,
                                    estimate = FALSE,
                                    hdf5 = c("auto", "always", "never"),
                                    dsn = NULL,
                                    compress = 5L,
                                    overwrite = FALSE,
                                    mem_threshold_mb = 500,
                                    batch_size = NULL) 
  standardGeneric("interpSlices"))

#' @rdname interpSlices
#' @export
setMethod("interpSlices", "GPRsurvey", function(obj, 
                                                dx = NULL, 
                                                dy = NULL, 
                                                dz = NULL, 
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
                                                batch_size = NULL){
  hdf5 <- match.arg(hdf5)
  
  
  extend = match.arg(extend, c("bbox", "obbox", "chull", "buffer"))
  test <- sapply(obj@coords, function(x) length(x) > 0)
  
  if(any(!test)){
    stop("Some of the data have no coordinates.\n", 
         " Please set first coordinates to all data,\n",
         " or remove these data!")
  }
  if( length(unique(obj@spunit)) > 1 ){
    stop("Position units are not identical: \n",
         paste0(unique(obj@spunit), collapse = ", "), "!")
  }
  if(length(unique(obj@zunits)) > 1){
    stop("Depth units are not identical: \n",
         paste0(unique(obj@zunits), collapse = ", "), "!\n")
  }
  stopifnot(length(obj@coords) == length(obj@zlengths))
  
  if(is.null(dx)){
    dx <- mean(obj@xlengths/(max(obj@nx, 2) - 1))
  }
  if(is.null(dy)){
    dy <- dx
  }
  if(is.null(dz)){
    dz <- mean(obj@zlengths/(max(obj@nz,2) - 1))
  }
  
  
  if(isTRUE(rot)){
    obj <- georef(obj)
    x_rot <- obj@transf[5]
  }else if(!isFALSE(rot)){
    if(is.numeric(rot)){
      x_rot <- rot
      obj <- georef(obj, alpha = x_rot)
    }else{
      stop("'rot' must be either TRUE/FALSE or numeric")
    }
  }else{
    x_rot <- 0
  }
  
  SXY <- .sliceInterp(obj = obj[test], dx = dx, dy = dy, dz = dz, h = h,
                      extend = extend,
                      bufferDist = bufferDist,
                      shp = shp, verbose = verbose, estimate = estimate,
                      hdf5 = hdf5, dsn = dsn, compress = compress,
                      overwrite = overwrite,
                      mem_threshold_mb = mem_threshold_mb,
                      batch_size = batch_size)
  
  if(isTRUE(estimate)) return(SXY)
  
  xyref <- c(min(SXY$x), min(SXY$y), SXY$vz[1])
  # xpos <- SXY$x #- min(SXY$x)
  # ypos <- SXY$y #- min(SXY$y)
  
  xfreq <- ifelse(length(unique(obj@freqs[test])) == 1, obj@freqs[test][1], numeric(0))
  
  # SXY$h5 is TRUE only when the cube was written straight to an HDF5 file
  # (see .sliceInterp()/.writeCubeHDF5()); SXY$z is NULL in that case and
  # SXY$dim carries the [nx, ny, nz] shape instead.
  is_h5 <- isTRUE(SXY$h5)
  nz_result <- if (is_h5) SXY$dim[3] else dim(SXY$z)[3]

  if (nz_result == 1) {
    class_name <- "GPRslice"
    ddata <- SXY$z[,,1]
  } else if (is_h5) {
    class_name <- "GPRcube"
    ddata <- array(dim = c(0L, 0L, 0L))   # sentinel: data lives at @path (see loadCube())
  } else {
    class_name <- "GPRcube"
    ddata <- SXY$z
  }
  xtrsf <- numeric(0)
  if(length(obj@transf)>0) xtrsf <- c(obj@transf[1:2], x_rot)

  # @path normally records the *source* survey path (see GPRvirtual). For an
  # HDF5-backed GPRcube, @path instead points at the cube's own backing file
  # -- that's where the data actually lives -- and the source survey path is
  # kept in @md so it isn't silently lost.
  obj_path <- if (is_h5) SXY$path else obj@paths[test][1]
  obj_md   <- if (is_h5) list(source_survey_path = obj@paths[test][1]) else list()

  y <- new(class_name,
           #----------------- GPRvirtual --------------------------------------#
           version      = "0.3",
           name         = "",
           path         = obj_path,
           desc         = "GPR cube",  # data description
           mode         = obj@modes[test][1],  # reflection/CMP/WARR (CMPAnalysis/spectrum/...)?
           date         = Sys.Date(),       # survey date (format %Y-%m-%d)
           freq         = xfreq,    # antenna frequency
           
           data         = ddata,      # data
           dunit        = "",         # data unit
           dlab         = "", 
           
           spunit       = obj@spunit[test][1],  # spatial unit
           crs          = obj@crs[test][1],  # coordinate reference system of @coord
           # ? coordref = "numeric",    # coordinates references or "center" or "centroid"
           
           xunit        = obj@spunit[test][1],  # horizontal unit
           xlab         = "",
           
           zunit        = obj@zunits[test][1],  # time/depth unit
           zlab         = "",
           
           # vel          = "list",   # velocity model (function of x, y, z)
           
           # proc         = "list",       # processing steps
           # delineations = "list",       # delineations
           md           = obj_md,        # data from header file/meta-data
           #----------------- GPRcube -----------------------------------------#
           dx     = dx,   # xpos,
           dy     = dy,   # ypos,
           # FIXME: dz sign handling is confusing and fragile. 
           # In setMethod() you take dz * sign(mean(diff(SXY$vz))) — that stores 
           # a signed dz in the object. This is confusing for consumers of the 
           # class. Better to keep dz positive (the sampling spacing) and store 
           # orientation/direction by storing z0 (top) and vz explicitly (which 
           # you already do).
           dz     = dz * sign(mean(diff(SXY$vz))),   # SXY$vz,
           ylab   = "",   #,  # set names, length = 1|p
           
           center = xyref,    # coordinates grid corner bottom left (0,0)
           rot    = xtrsf #x_rot     # rotation angle
  )
  if(class_name == "GPRslice") y@z <- SXY$vz[1]
  return(y)
})


# # define z
# defVz <- function(obj){
#   if(all(isZDepth(obj)) && all(sapply(obj@coords, length) > 0)){
#     # elevation coordinates
#     zmax <- sapply(obj@coords, function(x) max(x[,3]))
#     zmin <- sapply(obj@coords, function(x) min(x[,3])) - max(obj@zlengths)
#     d_z  <- obj@zlengths/(obj@nz - 1)
#     vz   <- seq(from = min(zmin), to = max(zmax), by = min(d_z))
#   }else{
#     # time/depth
#     d_z <- obj@zlengths/(obj@nz - 1)
#     vz <- seq(from = 0, by = min(d_z), length.out = max(obj@nz))
#   }
#   return(vz)
# }



#' Interpolate trace to target depths/times
#' 
#' @param x Amplitude values
#' @param z Depth/time values
#' @param zi Target depth/time values for interpolation
#' @return Interpolated amplitudes at zi
#' @noRd
# x = amplitude
# z = time/depth
# zi = time/depth at which to interpolate
#
# NOTE: earlier versions of the caller zeroed out NA amplitudes across the
# *entire* trace matrix before interpolation (an O(nrow*ncol) allocation
# per profile) so that every column could be interpolated the same way.
# Instead we drop non-finite points per-column here -- cheaper (no extra
# matrix copy upstream) and arguably more correct (a real 0 amplitude and
# a missing sample are not the same thing).
# NOT: with method = "pchip" troubles because of Non-Monotonic Z Values
trInterp <- function(x, z, zi){
  ok <- is.finite(x) & is.finite(z)
  if (sum(ok) < 2L) return(rep(NA_real_, length(zi)))
  xi <- signal::interp1(x = z[ok], y = x[ok], xi = zi, method = "spline",
                        extrap = 0)
  return(xi)
}


#' Interpolate GPR slices (refactored version)
#' 
#' This function interpolates GPR survey data onto a regular 3D grid using
#' multilevel B-spline approximation (MBA). Depth slices are computed in
#' batches (bounding peak memory during computation regardless of the
#' number of slices) and either accumulated into an in-memory array or
#' streamed straight to a chunked HDF5 file, depending on `hdf5`/
#' `mem_threshold_mb` -- see `interpSlices()` for the user-facing parameter
#' docs.
#' 
#' @param obj GPRsurvey object
#' @param dx x-resolution
#' @param dy y-resolution
#' @param dz z-resolution (depth or time spacing)
#' @param h MBA hierarchy levels (controls smoothness, default 6)
#' @param extend Extent method: "bbox", "obbox", "chull", or "buffer"
#' @param bufferDist Buffer distance around survey lines
#' @param shp Shape specification (sf object, list, or matrix)
#' @param m MBA row refinement (usually leave as default)
#' @param n MBA column refinement (usually leave as default)
#' @param verbose (`logical[1]`) If TRUE, verbose.
#' @param hdf5,dsn,compress,overwrite,mem_threshold_mb,batch_size See
#'   `interpSlices()`.
#' @return list with interpolation results:
#'   \item{x}{x-coordinates of grid}
#'   \item{y}{y-coordinates of grid}
#'   \item{z}{3D array of interpolated values `[nx x ny x nz]`, or `NULL`
#'            when `h5 = TRUE` (data was streamed to `path` instead)}
#'   \item{vz}{depth/time vector}
#'   \item{x0}{original x-coordinates of observations}
#'   \item{y0}{original y-coordinates of observations}
#'   \item{z0}{interpolated data matrix `[nz x n_traces]`}
#'   \item{dim}{`c(nx, ny, nz)`, always present (even when `z` is `NULL`)}
#'   \item{h5}{`TRUE` if the cube was streamed to an HDF5 file}
#'   \item{path}{path to the HDF5 backing file when `h5 = TRUE`, else `NULL`}
#' @noRd
.sliceInterp <- function(obj, dx = NULL, dy = NULL, dz = NULL, h = 6,
                         extend = "bbox", bufferDist = NULL, shp = NULL, 
                         m = 1, n = 1, verbose = TRUE, estimate = FALSE,
                         hdf5 = c("auto", "always", "never"),
                         dsn = NULL, compress = 5L, overwrite = FALSE,
                         mem_threshold_mb = 500, batch_size = NULL) {
  
  hdf5 <- match.arg(hdf5)
  
  # We deliberately never call future::plan() ourselves -- CRAN policy (and
  # good manners) reserve that decision for the user, since it affects the
  # whole session, not just this call. But running under the default
  # "sequential" plan silently gives zero parallelism for the MBA slice
  # loop below, which is easy to miss, so just point it out once.
  if (verbose && inherits(future::plan(), "sequential")) {
    message(
      "Note: depth-slice interpolation below runs under future::plan(\"sequential\") ",
      "(no parallelism). Call e.g. future::plan(future::multisession) before ",
      "interpSlices() to use multiple workers."
    )
  }
  
  # Step 1: Compute target depth vector
  vz <- .computeTargetDepths(obj, dz)
  
  # Step 2: Interpolate all profiles to target depths
  V <- .interpolateAllProfiles(obj, vz)
  
  # Step 3: Extract spatial coordinates (returns matrix [n x 2])
  xypos <- do.call(rbind, obj@coords)
  
  # Step 4: Process shape input
  x_shp <- .processShapeInput(shp, obj)
  shp_provided <- !is.null(shp)
  
  # Step 5: Compute interpolation extent
  extent <- .computeInterpolationExtent(
    extend, x_shp, xypos, dx, dy, bufferDist, shp_provided, obj
  )
  bbox_params <- extent$bbox_params
  
  # ------------- Estimate size ---------------- #
  nz <- length(vz)
  n_cells <- bbox_params$nx * bbox_params$ny * nz
  cube_size_mb <- n_cells * 8 / 1024^2   # double precision
  if(verbose){
    message(
      sprintf(
        paste(
          "Creating cube:",
          "%d x %d x %d cells",
          "(%.2f million voxels)",
          "~ %.1f MB"
        ),
        bbox_params$nx,
        bbox_params$ny,
        nz,
        n_cells / 1e6,
        cube_size_mb
      )
    )
  }
  
  if(isTRUE(estimate)){
    return(list(nx = bbox_params$nx,
                ny = bbox_params$ny,
                nz = nz,
                ncells = n_cells,
                sizeMB = cube_size_mb))
  }
  
  # Decide backend now that we know the real size. A single slice
  # (GPRslice) is always tiny -- never worth HDF5 overhead -- so only a
  # true cube (nz > 1) is eligible.
  use_h5 <- nz > 1 && switch(hdf5,
                             "always" = TRUE,
                             "never"  = FALSE,
                             "auto"   = cube_size_mb > mem_threshold_mb)
  
  if (!use_h5 && cube_size_mb > 2000) {
    warning(
      sprintf(
        "Cube will require approximately %.1f GB of memory. Consider hdf5 = \"always\" or a lower mem_threshold_mb.",
        cube_size_mb/1000
      )
    )
  }
  
  xy_clip <- extent$clip_polygon
  
  # Step 6: Compute MBA refinement parameters
  mba_params <- .computeMBARefinement(bbox_params$bbox, m, n)
  
  gx <- seq(bbox_params$bbox[1], bbox_params$bbox[2], length.out = bbox_params$nx)
  gy <- seq(bbox_params$bbox[3], bbox_params$bbox[4], length.out = bbox_params$ny)
  
  clip_mask <- NULL
  if (!is.null(xy_clip)) {
    clip_mask <- .createClippingMask(gx, gy, xy_clip)
  }
  
  if (use_h5) {
    if (verbose) {
      message(sprintf("Cube exceeds %.0f MB: streaming directly to an HDF5 file.",
                      mem_threshold_mb))
    }
    path <- .writeCubeHDF5(
      xypos = xypos, V = V, vz = vz,
      bbox_params = bbox_params, mba_params = mba_params,
      clip_mask = clip_mask, gx = gx, gy = gy, h = h,
      dsn = dsn, compress = compress, overwrite = overwrite,
      batch_size = batch_size, verbose = verbose
    )
    return(list(
      x = gx, y = gy, z = NULL, vz = vz,
      x0 = xypos[, 1], y0 = xypos[, 2], z0 = V,
      dim = c(bbox_params$nx, bbox_params$ny, nz),
      h5 = TRUE, path = path
    ))
  }
  
  # ---- in-memory path: still batched, so peak memory during computation
  # stays bounded to ~one batch even though the final result is a single
  # in-memory array (Phase 1 fix: previously future_lapply computed and
  # held *all* nz slices simultaneously before simplify2array()). ---- #
  SL <- .computeSlicesBatched(
    xypos = xypos, V = V, vz = vz,
    bbox_params = bbox_params, mba_params = mba_params,
    clip_mask = clip_mask, h = h, batch_size = batch_size, verbose = verbose
  )
  
  list(
    x = gx, y = gy, z = SL, vz = vz,
    x0 = xypos[, 1], y0 = xypos[, 2], z0 = V,
    dim = c(bbox_params$nx, bbox_params$ny, nz),
    h5 = FALSE, path = NULL
  )
}

#' Compute depth slices in memory-bounded batches
#' 
#' Shared batching logic used by both the in-memory and HDF5-streaming
#' paths of `.sliceInterp()`. Splits `seq_along(vz)` into batches sized to
#' stay under a target memory budget, computes each batch in parallel via
#' [future.apply::future_lapply()], and assembles the full in-memory array.
#' Each batch's rows are extracted from `V` fresh, right before that
#' batch's `future_lapply()` call, and discarded immediately after -- so,
#' unlike splitting the whole of `V` into a persistent row-list up front,
#' peak extra memory is bounded to one batch's worth of rows rather than a
#' full second copy of `V` held for the entire loop. The extracted subset
#' is passed directly as `future_lapply()`'s `X` argument (not referenced
#' by name from inside `FUN`), so only that batch's rows are ever
#' serialized to workers -- referencing a captured list from inside `FUN`
#' would instead make `future`'s automatic globals detection export the
#' *whole* list on every batch, since it has no way to know only a few
#' elements are used.
#' 
#' @param xypos (`matrix[n,2+]`) Observation coordinates; only the first
#'   two columns (x, y) are used -- `obj@coords` (and hence `xypos`) may
#'   carry a third (z) column that must NOT reach `.interpolateSlice()`,
#'   since it does `cbind(xy, values)` and expects exactly x, y, value.
#' @param V (`matrix[nz,n]`) Resampled trace values at all target depths
#' @param vz Target depth/time vector
#' @param bbox_params,mba_params,clip_mask,h See `.interpolateSlice()`
#' @param batch_size (`integer[1]|NULL`) Slices per batch; auto if `NULL`
#' @param verbose (`logical[1]`)
#' @return `array[nx,ny,nz]`
#' @noRd
.computeSlicesBatched <- function(xypos, V, vz, bbox_params, mba_params,
                                  clip_mask, h, batch_size = NULL,
                                  verbose = TRUE) {
  nx <- bbox_params$nx
  ny <- bbox_params$ny
  nz <- length(vz)
  xy <- xypos[, 1:2]   # sliced once, outside the loop: tiny vs. V
  
  if (is.null(batch_size)) {
    slice_mb   <- nx * ny * 8 / 1024^2
    batch_size <- max(1L, min(nz, floor(300 / max(slice_mb, 1e-6))))
  }
  batches <- split(seq_len(nz), ceiling(seq_len(nz) / batch_size))
  
  SL <- array(NA_real_, dim = c(nx, ny, nz))
  
  for (b in batches) {
    values_b <- lapply(b, function(j) V[j, , drop = TRUE])
    
    slices_b <- future.apply::future_lapply(
      values_b,   # this batch's rows only, passed as X
      function(values) {
        S <- .interpolateSlice(xy, values, bbox_params, h,
                               mba_params$m, mba_params$n)
        if (!is.null(clip_mask)) S$z[clip_mask] <- NA_real_
        S$z
      },
      future.packages = "MBA",
      future.seed = FALSE
    )
    SL[, , b] <- simplify2array(slices_b)
    rm(values_b, slices_b)
    if (verbose) {
      message(sprintf("  slices %d-%d / %d done", min(b), max(b), nz))
    }
  }
  
  SL
}

#' Compute target depth/time vector
#' 
#' @param obj GPRsurvey object
#' @param dz depth/time resolution
#' @return numeric vector of z values
#' @noRd
.computeTargetDepths <- function(obj, dz) {
  if (all(isZDepth(obj))) {
    # Depth mode: work from surface down
    zmax <- max(sapply(obj@coords, function(x) max(x[, 3])))
    zmin <- min(mapply(function(x, y) min(x[, 3] - y), 
                       obj@coords, obj@zlengths))
    vz <- seq(zmax, to = zmin, by = -dz)
  } else {
    # Time mode: work from zero up
    vz <- seq(from = 0, by = dz, to = max(obj@zlengths))
  }
  return(vz)
}

#' Interpolate single GPR profile to target depths
#' 
#' @param gpr_obj Single GPR object
#' @param vz Target depth/time vector
#' @return matrix of interpolated values `[length(vz) x ncol(gpr_obj)]`
#' @noRd
.interpolateProfile <- function(gpr_obj, z, vz, coordz, isDepth) {
  # NOTE: NAs are no longer zeroed out across the whole matrix here --
  # trInterp() drops non-finite points per column instead, so we save one
  # full nrow x ncol allocation/copy per profile.
  n_traces <- ncol(gpr_obj)
  
  if (isDepth) {
    if (length(unique(coordz)) > 1) {
      # Variable topography: each trace has different z-values.
      # NOTE: previously built two full [nrow x ncol] matrices (via
      # matrix(..., byrow=TRUE) and matrix(z, ...)) just to subtract them
      # column-by-column below; since we already loop per column, compute
      # each column's z-vector directly instead of allocating full matrices.
      result <- vapply(seq_len(n_traces),
                       function(j) trInterp(gpr_obj[, j], coordz[j] - z, vz),
                       numeric(length(vz)))
    } else {
      # Constant topography: all traces share z-values
      x_z <- coordz[1] - z
      result <- vapply(
        seq_len(n_traces),
        function(i)
          trInterp(gpr_obj[, i], x_z, vz),
        numeric(length(vz))
      )
    }
  } else {
    # Time/depth mode (no topography)
    x_z <- z
    result <- vapply(
      seq_len(n_traces),
      function(i)
        trInterp(gpr_obj[, i], x_z, vz),
      numeric(length(vz))
    )
  }
  
  return(result)
}


#' Interpolate all profiles to common depth grid
#' 
#' @param obj GPRsurvey object
#' @param vz Target depth/time vector
#' @return `matrix[length(vz),total_traces]`
#' @noRd
.interpolateAllProfiles <- function(obj, vz) {
  total_traces <- sum(obj@nx)
  V <- matrix(0, nrow = length(vz), ncol = total_traces)
  
  pos_start <- 0
  
  h5 <- hdf5r::H5File$new(obj@path, mode = "r")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  nms <- names(h5[["lines"]])
  isDepth <- isZDepth(SU)
  
  if (length(unique(isDepth)) != 1L) {
    stop(
      "The survey mixes depth-domain and time-domain profiles. ",
      "Check with `isDepth(obj)`. ",
      "All profiles must use the same vertical domain before creating slices.",
      call. = FALSE
    )
  }
  
  for (i in seq_along(nms)) {
    n_traces <- obj@nx[i] # ncol(obj[[i]])
    idx <- pos_start  + seq_len(n_traces)
    
    grp <- h5[["lines"]][[nms[i]]]
    gpr_val <- grp[["data"]]$read()
    # z <- grp[["z"]]$read()
    
    z <- grp[["z"]]$read()
    coordz <- obj@coords[[i]][, 3]
    
    # isDepth <- !grepl("(s|min|h)$", obj@zunits[i])
    
    V[, idx] <- .interpolateProfile(gpr_val, z, vz, coordz, isDepth = isDepth[i])
    
    pos_start <- pos_start + n_traces
  }
  
  return(V)
}

#' Process shape input into standard matrix format
#' 
#' @param shp Shape specification (sf, list, matrix, or NULL)
#' @param obj GPRsurvey object (fallback if shp is NULL)
#' @return `matrix[n,2]` of coordinates or GPRsurvey object
#' @noRd
.processShapeInput <- function(shp, obj) {
  if (is.null(shp)) {
    return(obj)
  }
  
  if (inherits(shp, c("sf", "sfc", "sfg"))) {
    return(sf::st_coordinates(shp)[, 1:2])
  } else if (is.list(shp)) {
    return(cbind(shp[[1]], shp[[2]]))
  } else {
    return(shp)
  }
}

#' Compute default buffer size
#' 
#' @param xy_coords (`matrix[n,2]`) Coordinate 
#' @param fraction Fraction of extent to use as buffer (default 0.05)
#' @return numeric buffer distance
#' @noRd
.computeDefaultBuffer <- function(xy_coords, fraction = 0.05) {
  min(
    diff(range(xy_coords[, 1])) * fraction,
    diff(range(xy_coords[, 2])) * fraction
  )
}

#' Compute interpolation extent for convex hull method
#' 
#' @param x_shp Shape coordinates
#' @param bufferDist Buffer distance (will compute default if NULL and shp not provided)
#' @param shp_provided Was shape explicitly provided?
#' @param dx x-resolution
#' @param dy y-resolution
#' @return list with bbox_params and clip_polygon
#' @noRd
.computeExtentConvexHull <- function(x_shp, bufferDist, shp_provided, dx, dy) {
  xsf_chull <- convexhull(x_shp)
  
  if (is.null(bufferDist)) {
    bufferDist <- if (shp_provided) {
      0
    } else {
      xsf_chull_xy <- sf::st_coordinates(xsf_chull)
      .computeDefaultBuffer(xsf_chull_xy)
    }
  }
  
  if (bufferDist > 0) {
    xsf_chull <- sf::st_buffer(xsf_chull, bufferDist)
  }
  
  xy_clip <- sf::st_coordinates(xsf_chull)
  para <- getbbox_nx_ny(xy_clip[, 1], xy_clip[, 2], dx, dy, bufferDist = 0)
  
  list(bbox_params = para, clip_polygon = xy_clip)
}

#' Compute interpolation extent for bounding box method
#' 
#' @param x_shp Shape coordinates (matrix or GPRsurvey)
#' @param xypos Observation coordinates matrix
#' @param bufferDist Buffer distance
#' @param shp_provided Was shape explicitly provided?
#' @param dx x-resolution
#' @param dy y-resolution
#' @return list with bbox_params and clip_polygon (NULL)
#' @noRd
.computeExtentBBox <- function(x_shp, xypos, bufferDist, shp_provided, dx, dy) {
  if (shp_provided) {
    if (is.null(bufferDist)) bufferDist <- 0
    para <- getbbox_nx_ny(x_shp[, 1], x_shp[, 2], dx, dy, bufferDist)
  } else {
    # When shp is NULL, bufferDist=NULL means getbbox_nx_ny uses 5% default
    para <- getbbox_nx_ny(xypos[, 1], xypos[, 2], dx, dy, bufferDist)
  }
  
  list(bbox_params = para, clip_polygon = NULL)
}


#' Compute interpolation extent for oriented bounding box method
#' 
#' @param x_shp Shape coordinates
#' @param bufferDist Buffer distance (will compute default if NULL and shp not provided)
#' @param shp_provided Was shape explicitly provided?
#' @param dx x-resolution
#' @param dy y-resolution
#' @return list with bbox_params and clip_polygon
#' @noRd
.computeExtentOBBox <- function(x_shp, bufferDist, shp_provided, dx, dy) {
  sf_obb <- obbox(x_shp)
  
  if (is.null(bufferDist)) {
    bufferDist <- if (shp_provided) {
      0
    } else {
      xsf_obb_xy <- sf::st_coordinates(sf_obb)
      .computeDefaultBuffer(xsf_obb_xy)
    }
  }
  
  if (bufferDist > 0) {
    sf_obb <- sf::st_buffer(sf_obb, bufferDist)
    sf_obb <- obbox(sf_obb)
  }
  
  xy_clip <- sf::st_coordinates(sf_obb)
  para <- getbbox_nx_ny(xy_clip[, 1], xy_clip[, 2], dx, dy, bufferDist = 0)
  
  list(bbox_params = para, clip_polygon = xy_clip)
}

#' Compute interpolation extent for buffer method
#' 
#' @param obj GPRsurvey object
#' @param bufferDist Buffer distance (must be > 0)
#' @param dx x-resolution
#' @param dy y-resolution
#' @return list with bbox_params and clip_polygon
#' @noRd
.computeExtentBuffer <- function(obj, bufferDist, dx, dy) {
  if (is.null(bufferDist) || !(bufferDist > 0)) {
    stop("When 'extend = bufferDist', 'bufferDist' must be larger than 0!")
  }
  
  x_shp <- buffer(obj, bufferDist)
  xy_clip <- sf::st_coordinates(x_shp)
  para <- getbbox_nx_ny(xy_clip[, 1], xy_clip[, 2], dx, dy, bufferDist = 0)
  
  list(bbox_params = para, clip_polygon = xy_clip)
}


#' Compute interpolation extent based on method
#' 
#' @param extend Method: "bbox", "obbox", "chull", or "buffer"
#' @param x_shp Shape coordinates (matrix or GPRsurvey)
#' @param xypos Observation coordinates matrix
#' @param dx x-resolution
#' @param dy y-resolution
#' @param bufferDist Buffer distance
#' @param shp_provided Was shape explicitly provided?
#' @param obj GPRsurvey object (for buffer method)
#' @return list with bbox_params and clip_polygon
#' @noRd
.computeInterpolationExtent <- function(extend, x_shp, xypos, 
                                        dx, dy, bufferDist, shp_provided, obj) {
  switch(extend,
         "chull" = .computeExtentConvexHull(x_shp, bufferDist, shp_provided, dx, dy),
         "bbox"  = .computeExtentBBox(x_shp, xypos, bufferDist, shp_provided, dx, dy),
         "obbox" = .computeExtentOBBox(x_shp, bufferDist, shp_provided, dx, dy),
         "buffer" = .computeExtentBuffer(obj, bufferDist, dx, dy),
         stop("Invalid extend method: ", extend)
  )
}


#' Compute MBA refinement parameters based on aspect ratio
#' 
#' @param bbox Bounding box vector c(xmin, xmax, ymin, ymax)
#' @param m Row refinement (will compute from aspect ratio if NULL)
#' @param n Column refinement (will compute from aspect ratio if NULL)
#' @return list with m and n
#' @noRd
.computeMBARefinement <- function(bbox, m = NULL, n = NULL) {
  ratio_x_y <- (bbox[4] - bbox[3]) / (bbox[2] - bbox[1])
  
  # Adjust refinement based on aspect ratio
  if (ratio_x_y < 1) {
    if (is.null(m)) m <- round(1 / ratio_x_y)
  } else {
    if (is.null(n)) n <- round(ratio_x_y)
  }
  
  # Ensure minimum of 1
  if (is.null(m)) m <- 1L
  if (is.null(n)) n <- 1L
  m <- max(1L, as.integer(m))
  n <- max(1L, as.integer(n))
  
  list(m = m, n = n)
}

#' Create clipping mask for interpolation grid
#' 
#' @param x Grid x-coordinates
#' @param y Grid y-coordinates
#' @param clip_polygon (`matrix[n,2]|NULL`) Polygon vertices or NULL
#' @return logical matrix or NULL
#' @noRd
.createClippingMask <- function(x, y, clip_polygon) {
  if (is.null(clip_polygon)) {
    return(NULL)
  }
  
  mask <- outer(x, y, inPoly,
                vertx = clip_polygon[, 1],
                verty = clip_polygon[, 2])
  
  return(!as.logical(mask))
}

#' Perform MBA interpolation for single depth slice
#' 
#' @param xypos (`matrix[n,2]`)  Observation coordinates
#' @param values Vector of values at observation points
#' @param bbox_params Bounding box parameters (list with nx, ny, bbox)
#' @param h MBA hierarchy levels
#' @param m Row refinement
#' @param n Column refinement
#' @return list with x, y grid coordinates and z interpolated matrix
#' @noRd
.interpolateSlice <- function(xypos, values, bbox_params, h, m, n) {
  result <- tryCatch({
    suppressWarnings(
      MBA::mba.surf(
        cbind(xypos, values),
        bbox_params$nx,
        bbox_params$ny,
        n = n,
        m = m,
        extend = TRUE,
        h = h,
        b.box = bbox_params$bbox
      )$xyz.est
    )
  }, error = function(e) {
    stop("MBA interpolation failed at depth slice: ", e$message, call. = FALSE)
  })
  
  return(result)
}

#' Compute bounding box and grid dimensions
#' 
#' @param xpos x-coordinates
#' @param ypos y-coordinates
#' @param dx x-resolution
#' @param dy y-resolution
#' @param bufferDist Buffer distance (NULL for auto 5%)
#' @return list with bbox, nx, ny
#' @noRd
getbbox_nx_ny <- function(xpos, ypos, dx, dy, bufferDist = NULL) {
  xpos_rg <- range(xpos, na.rm = TRUE)
  ypos_rg <- range(ypos, na.rm = TRUE)
  
  if (is.null(bufferDist)) {
    bufferDist <- min(diff(xpos_rg) * 0.05, diff(ypos_rg) * 0.05)
  }
  
  if (bufferDist > 0) {
    xpos_rg <- xpos_rg + c(-1, 1) * bufferDist
    ypos_rg <- ypos_rg + c(-1, 1) * bufferDist
  }
  
  bbox <- c(xpos_rg, ypos_rg)
  
  # Define the number of cells
  bbox_dx <- bbox[2] - bbox[1]
  bbox_dy <- bbox[4] - bbox[3]
  nx <- ceiling(bbox_dx / dx)
  ny <- ceiling(bbox_dy / dy)
  
  # Correct bbox so that dx, dy are exact
  Dx <- (nx * dx - bbox_dx) / 2
  Dy <- (ny * dy - bbox_dy) / 2
  bbox[1:2] <- bbox[1:2] + c(-1, 1) * Dx
  bbox[3:4] <- bbox[3:4] + c(-1, 1) * Dy
  
  nx <- nx + 1L
  ny <- ny + 1L
  
  list(bbox = bbox, nx = nx, ny = ny)
}



