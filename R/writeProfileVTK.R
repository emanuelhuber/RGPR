#' Export GPR B-Scan data with vertical traces and meandering trace path to binary VTK (legacy) format
#'
#' Each trace is vertical (along z), but trace origins follow a 2D path in x-y.
#'
#' @param data_matrix Numeric matrix of size (n_depths x n_traces). Each column is a trace amplitude vs depth.
#' @param x Numeric vector of length n_traces: x-coordinate of each trace origin (meandering path).
#' @param y Numeric vector of length n_traces: y-coordinate of each trace origin.
#' @param z0 Numeric vector of length n_traces: base elevation (z-coordinate) of each trace origin.
#' @param depths Numeric vector of length n_depths: depths (positive downward) from each trace origin.
#' @param dsn Character: path to output .vtk file.
#' @export
writeProfileVTK <- function(data_matrix, dsn, x, y, z0, depths) {
  # Validate inputs
  n_depths <- nrow(data_matrix)
  n_traces <- ncol(data_matrix)
  if (length(x)!=n_traces || length(y)!=n_traces || length(z0)!=n_traces) {
    stop("Length of x, y, z0 must match number of columns (traces) in data_matrix")
  }
  if (length(depths)!=n_depths) stop("Length of depths must match number of rows in data_matrix")
  
  # Grid dims: traces along X, single Y, depths along Z
  nx <- n_traces; ny <- 1L; nz <- n_depths
  n_points <- nx * ny * nz
  
  # Open binary VTK file
  con <- file(dsn, "wb"); on.exit(close(con))
  
  # Write header
  writeLines(c(
    "# vtk DataFile Version 3.0",
    "GPR B-Scan Export (vertical traces path-wise)",
    "BINARY",
    "DATASET STRUCTURED_GRID",
    sprintf("DIMENSIONS %d %d %d", nx, ny, nz),
    sprintf("POINTS %d float", n_points)
  ), con)
  
  # Write point coordinates in order: z fastest (depth), then y, then x
  # But since ny=1, loop over k (depth), j=1, i (trace)
  for (k in seq_len(nz)) {
    for (j in seq_len(ny)) {
      for (i in seq_len(nx)) {
        xi <- x[i]
        yi <- y[i]
        zi <- z0[i] + depths[k]
        writeBin(as.numeric(c(xi, yi, zi)), con, size=4, endian="big")
      }
    }
  }
  
  # Write scalar data
  writeLines(c(
    sprintf("POINT_DATA %d", n_points),
    "SCALARS GPR_Amplitude float 1",
    "LOOKUP_TABLE default"
  ), con)
  for (k in seq_len(nz)) {
    # data_matrix[k, ] gives amplitude at depth k for all traces
    writeBin(as.numeric(data_matrix[k, ]), con, size=4, endian="big")
  }
  
  message("VTK file written to: ", dsn)
}





#' Write the GPRsurvey objects in VTK format.
#'
#' @param obj Object of the class `GPR` or `GPRsurvey`
#' @param dsn Filepath (Length-one character vector). If `dsn = NULL`,
#'              the file will be save in the current working directory with
#'              the name of obj (`name(obj)`) with the extension depending 
#'              of `format`.
#' @param overwrite Boolean. If `TRUE` existing files will be overwritten,
#'                  if `FALSE` an error will be thrown if the file(s) 
#'                  already exist(s).
#' @seealso [readGPR()]
#' @name writeVTK
setGeneric("writeVTK", function(obj, dsn = NULL, 
                                overwrite = TRUE){ standardGeneric("writeVTK")})


#' @rdname writeVTK
#' @export
setMethod("writeVTK", "GPR", function(obj, dsn = NULL, 
                                      overwrite = FALSE){
  writeProfileVTK(obj@data, dsn, x = obj@coord[,1], y = obj@coord[,2], z0 = obj@coord[,3], 
                  depths = -obj@z)
})
  
#' @rdname writeVTK
#' @export
setMethod("writeVTK", "GPRsurvey", function(obj, dsn = NULL, 
                                      overwrite = FALSE){

  tst <- sapply(obj@coords, length) > 0
  if(all(!tst)) stop("no coordinates")
  
  obj <- obj[tst]
  dsn_base <- gsub("(.vtk)$", "", dsn)
  
  for(i in seq_along(obj)){
    obj_i <- obj[[i]]
    writeProfileVTK(obj_i@data, paste0(dsn_base, obj_i@name, ".vtk"), 
                    x = obj_i@coord[,1], 
                    y = obj_i@coord[,2], 
                    z0 = obj_i@coord[,3], 
                    depths = -obj_i@z)
  }
  
  # # ---- count points ----
  # n_points <- sum(obj@nx * obj@nz)
  # 
  # 
  # con <- file(dsn, "wb")
  # on.exit(close(con))
  # 
  # # ---- header ----
  # writeLines(c(
  #   "# vtk DataFile Version 3.0",
  #   "Multiple GPR Vertical Profiles",
  #   "BINARY",
  #   "DATASET UNSTRUCTURED_GRID",
  #   sprintf("POINTS %d float", n_points)
  # ), con)
  # 
  # # ---- write coordinates ----
  # for(i in seq_along(obj)) {
  #   data   <- obj[[i]]@data
  #   x      <- obj@coords[[i]][,1]
  #   y      <- obj@coords[[i]][,2]
  #   z0     <- obj@coords[[i]][,3]
  #   depths <- -obj[[i]]@z
  #   
  #   n_depths <- nrow(data)
  #   n_traces <- ncol(data)
  #   
  #   if (length(x)!=n_traces || length(y)!=n_traces || length(z0)!=n_traces)
  #     stop("x, y, z0 length mismatch in one profile")
  #   if (length(depths)!=n_depths)
  #     stop("depths length mismatch in one profile")
  #   
  #   for (k in seq_len(n_depths)) {
  #     for (i in seq_len(n_traces)) {
  #       writeBin(
  #         as.numeric(c(x[i], y[i], z0[i] + depths[k])),
  #         con, size = 4, endian = "big"
  #       )
  #     }
  #   }
  # }
  # 
  # # ---- write cells (each point as a vertex) ----
  # # required for unstructured grids
  # writeLines(
  #   sprintf("CELLS %d %d", n_points, n_points * 2),
  #   con
  # )
  # 
  # for (i in 0:(n_points - 1)) {
  #   writeBin(as.integer(c(1, i)), con, size = 4, endian = "big")
  # }
  # 
  # writeLines(
  #   sprintf("CELL_TYPES %d", n_points),
  #   con
  # )
  # writeBin(rep(1L, n_points), con, size = 4, endian = "big") # VTK_VERTEX = 1
  # 
  # # ---- write scalar data ----
  # writeLines(c(
  #   sprintf("POINT_DATA %d", n_points),
  #   "SCALARS GPR_Amplitude float 1",
  #   "LOOKUP_TABLE default"
  # ), con)
  # 
  # for(i in seq_along(obj)) {
  #   objdata <- obj[[i]]@data
  #   for (k in seq_len(nrow(objdata))) {
  #     writeBin(
  #       as.numeric(objdata[k, ]),
  #       con, size = 4, endian = "big"
  #     )
  #   }
  # }
  # 
  # message("VTK files written to: ", dsn)
})  



