class

# Class GPR

```r
extractPattern(x, pattern, start = 0, stop = -1)
```

## Arguments

- `x`: (`character`) A character vector where matches are sought, or an object which can be coerced by `as.character()`
    
    to a character vector.
- `pattern`: (`character`) String containing a regular expression to be matched (see `grep()`).
- `start`: (`integer`) The first element of the matched string to be extracted (see `substr()`).
- `stop`: (`integer`) The last element of the matched string to be extracted (see `substr()`).

## Returns

(`character`) The matched string

A virtual S4 class to represent a ground-penetrating radar (GPR) data.

Get the match in a string given a regular expression and extract substrings.

## Slots

- **`version`**: (`character[1]`) Version of RGPR.
- **`name`**: (`character[1]`) Name of the GPR data.
- **`path`**: (`character[1]`) File path of the original GPR data.
- **`desc`**: (`character[1]`) Description of the GPR data.
- **`mode`**: (`character[1]`) Survey mode (e.g., `"CO"`, `"CMP"`).
- **`date`**: (`Date(1)`) Date of the survey (class `Date`, e.g., `Sys.Date()`).
- **`freq`**: (`numeric[1]`) GPR antennae frequency (in MHz).
- **`data`**: (`array`) GPR data.
- **`dunit`**: (`character[1]`) GPR data unit.
- **`dlab`**: (`character[1]`) GPR data label.
- **`crs`**: (`character[1]`) Coordinate reference system following the R notation of proj4string from the PROJ.4 library.
- **`spunit`**: (`character[1]`) Spatial units.
- **`xunit`**: (`character[1]`) Unit of `x`.
- **`xlab`**: (`character[1]`) Label of `x`.
- **`zunit`**: (`character[1]`) Unit of `z`.
- **`zlab`**: (`character[1]`) Label of `z`.
- **`vel`**: (`list`) Velocity model.
- **`proc`**: (`list`) Each element corresponds to a processing step applied to the data.
- **`delineations`**: (`list`) Hand-delineated structures.
- **`md`**: (`list`) Additional meta data from GPR file.

## `mode`

survey mode, difference between reflection and CMP and WARR

- **CO**: Common-offset: The trace positions increase from trace to trace. The antenna separation distance is constant. Amplitude = f(depth, position)
- **CMP**: The trace positions, `x@pos`, are constant and equal to zero. The antenna separation distance increases increase from trace to trace. Amplitude = f(depth, antsep)
- **WARR**: The trace positions, `x@pos`, are not constant because while the signal is emitted from the same position, the signal the signal is recorded from different positions. The antenna separation distance increases increase from trace to trace. Is "CMP" a special case of "WARR"? Amplitude = f(depth, antsep)
- **ZOP**: Transillumination survey: Zero-offset profiling
- **MOG**: Transillumination survey: multi-offset profiling

## `data`

- **class `GPR`**: TA `m \times n` numeric matrix consiting of a cross-section of signal amplitudes as a function of the GPR position. The columns of `data` correspond to the GPR traces and the row of `data` to the time/depth samples.
- **class `GPRset`**: 3D data
- **class `GPRcube`**: 3D data

## See Also

`substr()` and `regexpr()`