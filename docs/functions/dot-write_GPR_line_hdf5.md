# Write a single GPR line into an open HDF5 group

```r
.write_GPR_line_hdf5(parent_grp, name, gpr, compress = 0L)
```

## Arguments

- `parent_grp`: An open `hdf5r` group object (the `/lines` group).
- `name`: (`character(1)`) Name for the new sub-group.
- `gpr`: Object of class `GPR`.
- `compress`: (`integer(1)`) gzip level 0-9 for the main data array (and, if large, the coordinate matrix). `0` disables compression.

## Returns

Invisibly returns the created group object.

## Description

Creates `parent_grp/<name>` and fills it with everything needed to fully reconstruct the corresponding `GPR` object later with `.read_GPR_line_hdf5()`:

## Details

```
<name>/
  (attrs: name, date, freq, antsep, mode, crs, dunit, xunit, zunit,
     version, desc, spunit)
  data            -- nz x nx, 64-bit float, chunked, checksummed,
                 optionally gzip-compressed
  z, x, z0        -- axes
  markers         -- character[nx], trimmed and padded/truncated to
                 exactly nx elements (see `.normalizeMarkers()`)
  coords/xyz      -- trace coordinates (if any)
  coords/rec      -- receiver coordinates (if any)
  coords/trans    -- transmitter coordinates (if any)
  vel/v           -- velocity model (if any)
  metadata/...    -- flattened, human-browsable copy of scalar @md entries
  metadata_raw    -- the *entire* @md list, losslessly serialized (see
                 `.h5_write_r_object()`); this is what is actually
                 used to restore @md on read
```

 


