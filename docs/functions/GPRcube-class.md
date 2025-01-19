class

# Class GPRcube

An S4 class to represent 3D ground-penetrating radar (GPR) data. Array of dimension `n \times m \times p` (`n` samples, `m` traces or A-scans along `x`, and `p` traces or A-scans along `y`), with grid cell sizes `dx`, `dy`, and `dz`. We assume that the unit along y is the same as the unit along x.

## Slots

- **`dx`**: (`numeric[1]`) Grid cell size along x
- **`dy`**: (`numeric[1]`) Grid cell size along y
- **`dz`**: (`numeric[1]`) Grid cell size along z
- **`ylab`**: (`character[1|p]`) Label of `y`.
- **`center`**: (`numeric[3]`) Coordinates of the bottom left grid corner.
- **`rot`**: (`numeric[1]`) Rotation angle