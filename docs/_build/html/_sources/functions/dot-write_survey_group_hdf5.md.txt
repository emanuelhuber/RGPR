# Write the complete `/survey` metadata group (internal)

```r
.write_survey_group_hdf5(h5, obj)
```

## Arguments

- `h5`: Open, writable hdf5r::H5File handle.
- `obj`: Object of class `GPRsurvey`.

## Description

Writes every slot of a `GPRsurvey` object that makes sense to persist into `/survey` of an open HDF5 file, replacing any previous `/survey` group. This is the single source of truth for what a `GPRsurvey` HDF5 file contains at the survey level; `.read_survey_group_hdf5()` is its exact mirror image on the read side.

## Details

Slots intentionally not written here:

 * `@view` -- this describes the **in-memory** object's relationship to its backing file (a lightweight subset view vs. an independent, writable survey), not a property of the file itself. A survey loaded directly from disk with `readGPRsurvey()` is always `view = FALSE`.
 * `@coords`, `@markers` -- these already live per-line under `/lines/\<name\>/coords/xyz` and `/lines/\<name\>/markers`, which is the authoritative copy. Duplicating them at the survey level would risk the two copies drifting apart; `readGPRsurvey()` instead reconstructs `@coords`/`@markers` by reading the (small) per-line datasets directly.


