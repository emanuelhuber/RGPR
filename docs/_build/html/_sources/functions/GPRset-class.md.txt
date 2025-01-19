class

# Class GPRset

A S4 class to represent GPR data sets. Array of dimension `n \times m \times p` (`n` samples, `m` traces or A-scans, and `p` set elements). A GPR set is, for example, the multi-dimensional output of a transform function. Examples:

- **Fourier transform**: The GPR data can be decomposed into two element set: phase and amplitude as a function of the frequency.
- **Eigenimage decomposition**: Also called Karhunen-Loeve (KL) transformation. The GPR data is decomposed into a set of eigenimages.

## Slots

- **`y`**: (`numeric[p]`) Values associated with the `p`
       
       elements of the set.
- **`yunit`**: (`character[1|p]`) Unit(s) of set elements.
- **`ylab`**: (`character[1|p]`) Label(s) of set elements.
- **`formula`**: (`expression`) Expression to reconstruct the GPR data (backtransform).