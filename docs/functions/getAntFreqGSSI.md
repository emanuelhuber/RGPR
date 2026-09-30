# Look up nominal antenna frequency from GSSI antenna name string

```r
getAntFreqGSSI(x)

getAntFreqGSSI(x)
```

## Arguments

- `x`: (`character`) Name(s) of the antenna(e)
- `ant_name`: (`character`) Antenna name string(s) as read from the header.

## Returns

(`numeric`) Frequency in MHz for each element of `ant_name`. `NA` is returned for unrecognised names.

(`list`) List of numeric values corresponding to the frequency/frequencies

## Description

GSSI encodes the antenna model in a 14-character string stored in the file header. This function maps known model strings to their nominal centre frequency in MHz.

Given the antenna name, returns the frequency of GSSI antenna

## References

GSSI SIR-30 / SIR-4000 technical documentation.


