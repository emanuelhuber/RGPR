# Create an empty GPRsurvey object

```r
GPRsurveyInit(n = 1)
```

## Arguments

- `n`: Integer. Number of profiles to initialize in the survey. Must be greater than 0. Values are rounded and coerced to integer.

## Returns

An object of class `GPRsurvey` with slots initialized to empty values of the appropriate type and length.

## Description

Creates and initializes an empty `GPRsurvey` object containing metadata and placeholders for `n` GPR profiles. The returned object can subsequently be populated with imported or manually created `GPR` datasets.

## Details

The function allocates storage for profile-level metadata including:

 * file paths and profile names,
 * acquisition dates,
 * antenna frequencies,
 * antenna separations,
 * coordinate reference system (CRS),
 * spatial coordinates,
 * profile dimensions and units.

The survey-level fields `path`, `name`, and `desc` are initialized as empty character vectors, while profile-specific information is stored in the corresponding plural slots (`paths`, `names`, `descs`, etc.).

## Examples

```r
## Not run:

# Create an empty survey containing a single profile
s <- GPRsurveyInit()

# Create an empty survey for 10 profiles
s <- GPRsurveyInit(10)

# Check number of allocated profiles
length(s@paths)
## End(Not run)
```

## See Also

`GPRsurvey`, `GPRsurvey`


