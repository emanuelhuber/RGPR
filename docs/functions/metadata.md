# Meta-data extracted from the GPR data

```r
metadata(x)

## S4 method for signature 'GPR'
metadata(x)
```

## Arguments

- `x`: (`GPR`) An object of the class GPR.

## Returns

(`list`)

Meta-data extracted from the GPR data that depends on the manufacturer file format. All the information that are not used to construct the RGPR objects is still stored as meta-data and can be retrieved with `md()`.

## Details

The element names of the list are given below

- **hd**: Here comes all the information from the raw GPR data file that is not used to construct a RGPR object. It can be retrieved with `metadata(x)$hd`.
- **GPS**: When reading GPR data, the optional GPS data file is red if available and red even if `interpGPS = FALSE`
       
       (see `readGPR()`). If `interpGPS = FALSE`, the (formated) content of the GPS data file is then stored as meta-data and can be retrieved with `metadata(x)$GPS`.
- **clipData**: When reading GPR data, the clipDataped signal values are directly estimated from the bit values and stored as metadata. They can be retrieved with `metadata(x)$clipData`. These clipDataped values can be used directly .

## See Also

`readGPR()`