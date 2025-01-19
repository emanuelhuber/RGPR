# Bits to volt conversion

Convert bits to volt values

```r
bits2volt(Vmax = 50, Vmin = 50, nbits = 16)
```

## Arguments

- `Vmax`: (`numeric[1]`) Maximal nominal analog input voltage. If `Vmax = NULL` or `Vmax = FALSE`
    
    it returns `1` (no bytes to volt transformation)
- `Vmin`: (`numeric[1]`) Minimal nominal analog input voltage. If missing, then `Vmin = -Vmax`.
- `nbits`: (`integer[1]`) Number of bits.

Bits to volt conversion

Convert bits to volt values