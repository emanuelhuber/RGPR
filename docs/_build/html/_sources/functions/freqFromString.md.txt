# Extract frequency from string

```r
freqFromString(s)
```

## Arguments

- `s`: (`character`) Character string that may contain an indication of a frequency.

## Returns

(`numeric`) The frequency (`NA` if no frequency value is found)

Extract with regex the antenna frequency in a string

## Examples

```r
s <- "1230 fds 200-MHZ 12.3"
freqFromString(s) 
s <- "1230MLF"
freqFromString(s) 
s <- "D1230MLF"
freqFromString(s)
```