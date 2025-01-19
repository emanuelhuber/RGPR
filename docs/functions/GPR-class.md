class

# Class GPR

An S4 class to represent a ground-penetrating radar (GPR) data.

## Details

Matrix of dimension `n \times m` (`n` samples, `m` traces or A-scans).

## Slots

- **`z0`**: (`numeric[m]`) Time-zero or depth-zero.
- **`time`**: (`numeric[m]`) Recording time of every trace (UTC).
- **`antsep`**: (`numeric[m]`) Antenna separation.
- **`markers`**: (`character[m]`) Fiducial markers associated with the traces.
- **`ann`**: (`character[m]`) Annotations associated with the traces.
- **`coord`**: (`matrix[m,3]`) Trace positions.
- **`rec`**: (`matrix[m,3]`) Receiver positions
- **`trans`**: (`matrix[m,3]`) Transmitter positions
- **`x`**: (`numeric`) Relative trace position
- **`z`**: (`numeric`) Relative sample position
- **`angles`**: (`matrix[m,2]`) Transmitter positions