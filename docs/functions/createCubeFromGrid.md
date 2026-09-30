# Create a GPRcube from a GPR grid (GPRsurvey)

```r
createCubeFromGrid(
  obj,
  dz = NULL,
  direction = c("x", "y"),
  name = "",
  desc = "",
  track = TRUE
)

## S4 method for signature 'GPRsurvey'
createCubeFromGrid(
  obj,
  dz = NULL,
  direction = c("x", "y"),
  name = "",
  desc = "",
  track = TRUE
)
```

## Arguments

- `obj`: (`GPRsurvey`) An object of the class GPRsurvey.
- `dz`: (`NULL|numeric[1]`) Vertical resolution (if NULL, the mean vertical resolution of the data is taken).
- `name`: (`character[1]`) Name of the GPRcube.
- `desc`: (`character[1]`) Description of the GPRcube.
- `track`: (`logical[1]`) Should the processing step be tracked?

## Returns

(`GPRcube`) An object of the class GPRcube.

## Description

Create a GPRcube from a GPR grid (GPRsurvey)


