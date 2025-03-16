# Estimate and set time-zero

```r
estimateTime0(
  x,
  method = c("coppens", "threshold", "MER"),
  thr = NULL,
  w = NULL,
  ns = NULL,
  bet = NULL,
  shorten = TRUE,
  c0 = 0.299,
  FUN = NULL,
  ...,
  track = TRUE
)

## S4 method for signature 'GPR'
estimateTime0(
  x,
  method = c("coppens", "threshold", "MER"),
  thr = NULL,
  w = NULL,
  ns = NULL,
  bet = NULL,
  shorten = TRUE,
  c0 = 0.299,
  FUN = NULL,
  ...,
  track = TRUE
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
- `c0`: (`numeric[1]`) Propagation speed of the GPR wave through air in unit of space per unit of time (generally in m/ns).
- `FUN`: (`function()`) A function to apply on the estimated time-zero of every traces (e.g., `mean` or `median` to get set a single time-zero value to the data).
- `...`: Further arguments to be passed to `FUN`.

## Returns

(`GPR`) An object of the class `GPR`.

`estimateTime0` estimates for each trace individually the first wave break, computes the corresponding time-zero knowing the propagation speed of the electromagnetic wave through air and returns an object of the class `GPR` with updated time-zero. It is possible to apply a function provided by the user (e.g., `FUN`) on time-zero (e.g., to set time-zero equal to the average value of the time-zeros computed for every traces; in this case, all traces would have the same time-zero).

## Details

This function is a wrapper for the following commands:

 * `tfb \<- pickFirstBreak(x, ...)`
 * `t0 \<- firstBreakToTime0(x, tfb)`
 * `time0(x) \<- t0` (if `FUN` is not `NULL` else `time0(x) \<- FUN(t0, ...)`)

Modified slots:

 * `time0`: new estimated time-zero.
 * `proc`: updated with function name and arguments.

## Examples

```r
data("frenkeLine00")
x <- frenkeLine00
x1 <- estimateTime0(x, w = 10)
time0(x1)
x2 <- estimateTime0(x, w = 10, FUN = mean)
time0(x2)
```

## See Also

`pickFirstBreak()` to estimate the first wave break; `firstBreakToTime0()` to convert the first wave break into time zero. `time0()` and `setTime0()` to set time-zero; `shiftToTime0()` to shift the traces such that they start at time-zero.