class

# Class GPRslice

An S4 class to represent time/depth slices of ground-penetrating radar (GPR) data. Array of dimension `1 \times m \times p`

(`m` traces or A-scans along `x`, and `p` traces or A-scans along `y`), with grid cell sizes `dx`, `dy`. We assume that the unit along y is the same as the unit along x.