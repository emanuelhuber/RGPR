# Export GPR B-Scan data with vertical traces and meandering trace path to binary VTK (legacy) format

```r
writeProfileVTK(data_matrix, dsn, x, y, z0, depths)
```

## Arguments

- `data_matrix`: Numeric matrix of size (n_depths x n_traces). Each column is a trace amplitude vs depth.
- `dsn`: Character: path to output .vtk file.
- `x`: Numeric vector of length n_traces: x-coordinate of each trace origin (meandering path).
- `y`: Numeric vector of length n_traces: y-coordinate of each trace origin.
- `z0`: Numeric vector of length n_traces: base elevation (z-coordinate) of each trace origin.
- `depths`: Numeric vector of length n_depths: depths (positive downward) from each trace origin.

## Description

Each trace is vertical (along z), but trace origins follow a 2D path in x-y.


