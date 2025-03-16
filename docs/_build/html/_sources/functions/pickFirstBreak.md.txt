# Time of first wave break

```r
## S4 method for signature 'GPR'
pickFirstBreak(
  x,
  method = c("coppens", "threshold", "MER"),
  thr = NULL,
  w = NULL,
  ns = NULL,
  bet = NULL,
  shorten = TRUE
)
```

## Arguments

- `x`: (`GPR`) An object of the class `GPR
- `method`: (`character[1]) Method to be applied (either`"coppens"`,`"threshold"`or`"MER"`).`"coppens"`corresponds to the modified Coppens method,`"threshold"`to the threshold method, and`"MER"` to the modified energy ratio method.
- `thr`: (`numeric[1]`] Threshold for the signal amplitude (in percent) at which time zero is picked (only for the threshold method). `thr` ranges between 0 (0 percent) and 1 (100 percent).
- `w`: (`numeric[1]`) Length of the short-term leading window in unit of time (only for `method = "coppens"` or `method = "MER"`). Recommended value: about one period of the first-arrival waveform.
- `ns`: (`numeric[1]`] Length of the edge preserving smoothing window in unit of time (only for `method = "coppens"`). Recommended value: between one and two signal periods. When `ns = NULL` the value of `ns` is set to `1.5 * w`.
- `bet`: (`numeric[1]`) Stabilisation constant (only for `method = "coppens"`). Not critical. When `bet = NULL` the value of `bet` is set to 20 percent of the maximal signal amplitude.
- `shorten`: (`logical[1]`) If `TRUE`, each trace is shortened by removing the samples that are `2 \times w` after the maximum value (only for `method = "coppens"` or `method = "MER"`). You may set `shorten = FALSE` if the first wave break occurs after the maximum absolute amplitude time.

## Returns

(`numeric[n]`] The time of the first wave break for every traces in unit of time (`n = ncol(x) =` number of traces).

Pick the time of the first wave break in each trace (trace-by-trace function).

## Details

The Modified Coppens's method (`"coppens"`) computes the energy ratio between a long term window (with increasing length) and a short-term leading window (fixed length). Edge-preserving smoothing is then applied and the first wave break is assigned to the sample in which the derivative of the output is largest.

The modified energy ratio method (`"MER"`) computes the energy ratio between a preceding and trailing windows of equal length. The energy ratio is then multiplied by the absolute values of the trace and the output to the power of three is returned.

In the threshold method (`"threshold"`), the sample before the first sample that is larger than the threshold times the maximum absolute amplitude is picked. Then the time of the first wave break is linearly interpolated between these two samples.

## References

 * Modified Coppens method: Sabbione J.I. and Velis D. (2010) Automatic first-breaks picking: New strategies and algorithms. Geophysics, 75(4): 67-76.`
 * Modified Energy Ratio (MER) method: Han L., Wong J., and John C. (2010) Time picking on noisy microseismograms. In: Proceedings of the GeoCanada 2010 Convention - Working with the Earth, Calgary, AB, Canada, p. 4`

## See Also

`firstBreakToTime0()` to convert time of first wave break into time-zero; `time0()` and `setTime0()` to set time-zero; `estimateTime0()` to estimate first wave break, convert it to time-zero and set time zero (all in one step); `time0Cor()` to shift the traces such that they start at time-zero.