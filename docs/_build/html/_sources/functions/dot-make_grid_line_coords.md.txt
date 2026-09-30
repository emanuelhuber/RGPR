# Construct coordinates for one grid line

```r
.make_grid_line_coords(
  ntr,
  fixed_coordinate,
  start,
  reverse,
  orientation = c("x", "y"),
  end = NULL,
  trace_positions = NULL
)
```

## Arguments

- `ntr`: Number of traces in the line.
- `fixed_coordinate`: Constant grid coordinate.
- `start`: Starting coordinate along the line.
- `reverse`: Whether to reverse the trace direction.
- `orientation`: Line orientation, either `"x"` for a line with constant x or `"y"` for a line with constant y.
- `end`: Optional ending coordinate. If supplied, positions are generated with `seq()`.
- `trace_positions`: Optional original trace-position vector.

## Returns

A numeric matrix with columns `x`, `y`, and `z`.

## Description

Construct coordinates for one grid line


