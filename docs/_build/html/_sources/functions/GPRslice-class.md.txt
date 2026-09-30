 class

# Class GPRslice

## Description

An S4 class to represent time/depth slices of ground-penetrating radar (GPR) data. Array of dimension `1 \times m \times p`

(`m` traces or A-scans along `x`, and `p` traces or A-scans along `y`), with grid cell sizes `dx`, `dy`. We assume that the unit along y is the same as the unit along x.

## Details

`GPRslice` has no slots of its own: it's simply a `GPRcube` (see `GPRcube-class`) whose `@z` has length 1 -- the depth/time of that single slice -- and whose validity constraint (`@z` strictly monotonic) is trivially satisfied for a length-1 vector.


