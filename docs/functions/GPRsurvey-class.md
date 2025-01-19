class

# GPR survey data

An S4 class to represent a collection of `k` GPR data. An object of the class `GPRsurvey` does only contain the links to the GPR data files as well as information about the GPR data such as coordinates, frequencies, antenna separation etc. The methods of the `GPRsurvey` class exclusively manipulate the trace (A-scan) coordinates. More specifically, the methods currently allow to:

 * display map of the GPR measurement lines that can be superimposed on any raster or vector data,
 * display GPR data in three-dimensional interactive graphics (e.g., fence diagram) with openGL,
 * add or interpolate trace coordinates (the elevation coordinates can be interpolated from raster data),
 * georeference the trace coordinates (e.g., from a local coordinate reference system to a regional reference coordinate system),
 * project the GPR data to any coordinate reference system,
 * estimate the spatial shift between two (parallel)GPR profiles.

## Slots

- **`version`**: (`character[1]`) Version of RGPR.
- **`names`**: (`character[k]`) Names of the GPR data.
- **`paths`**: (`character[k]`) File paths of the original GPR data.
- **`descs`**: (`character[k]`) Descriptions of the GPR data.
- **`modes`**: (`character[k]`) Survey modes of the GPR data (e.g., `"CO"`, `"CMP"`).
- **`dates`**: (`Date[k]`) Date of the GPR data (class `Date`, e.g., `Sys.Date()`).
- **`freqs`**: (`numeric[k]`) Antennae frequency of the GPR data (in MHz).
- **`antseps`**: (`numeric[k]`) Antennae separation of the GPR data
- **`spunit`**: (`character[1]`) Spatial units.
- **`crs`**: (`character[1]`) Coordinate reference system in wkt format (sf package)
- **`coords`**: (`list[k]`) GPR data coordinates
- **`intersections`**: (`list[k]`) Intersections coordinates of the GPR data
- **`markers`**: (`list[k]`) Fiducial markers of the GPR data
- **`nz`**: (`integer[k]`) Number of samples in each GPR data.
- **`zlengths`**: (`numeric[k]`) Range along `z` in each GPR data.
- **`zunits`**: (`character[1]`) Unit of `z`.
- **`nx`**: (`integer[k]`) Number of traces in each GPR data.
- **`xlengths`**: (`numeric[k]`) Length of the GPR data.
- **`transf`**: (`numeric(5)`) Translation vector before rotation, after and rotation angle