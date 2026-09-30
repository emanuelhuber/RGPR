# Topographic Kirchhoff migration

```r
migrate(
  obj,
  type = c("static", "kirchhoff"),
  dz = NULL,
  fdo = NULL,
  x = NULL,
  maxangle = NULL,
  weight = "obliquity",
  normalize = FALSE,
  spreading = TRUE,
  waveletfilter = "halfderiv",
  antialias = FALSE,
  aafactor = 1,
  ...
)

## S4 method for signature 'GPR'
migrate(
  obj,
  type = c("static", "kirchhoff"),
  dz = NULL,
  fdo = NULL,
  x = NULL,
  maxangle = NULL,
  weight = "obliquity",
  normalize = FALSE,
  spreading = TRUE,
  waveletfilter = "halfderiv",
  antialias = FALSE,
  aafactor = 1,
  ...
)
```

## Arguments

- `obj`: An object of class `"GPR"`.
- `type`: Character string specifying the migration method. Currently, `"kirchhoff"` performs topographic Kirchhoff migration.
- `dz`: vertical sampling interval of the migrated image, in metres. The default is `0.25 * obj_dz`, where `obj_dz` is the mean vertical sampling interval.
- `fdo`: Optional positive numeric scalar giving the antenna centre or dominant frequency in MHz. If `NULL`, `obj@freq` is used. The frequency is required when `maxangle = NULL`, because the migration aperture is then calculated from the first Fresnel zone.
- `x`: Optional numeric vector giving the horizontal position, in metres, of every trace. If `NULL`, `obj@x` is used. The vector must have length `ncol(obj@data)` and must contain finite, strictly increasing values.
- `maxangle`: Maximum migration aperture angle in degrees, measured from the vertical. A trace contributes only if both the transmitter-to-pixel and receiver-to-pixel angles do not exceed this value. Use `90` to disable the angle-based aperture restriction.
    
    If `NULL`, the aperture is derived separately for each image point from the first depth-dependent Fresnel zone following Pérez-Gracia et al. (2008). With wavelength `\lambda = 1000 v / f_{do}` and depth `d` below the local ground surface, the Fresnel radius is `r_f = 0.5\sqrt{2\lambda d}`, and the limiting angle is `\arctan(r_f/d)`. The aperture therefore narrows with depth.
- `weight`: Character string specifying the migration weighting:
    
     * `"none"` applies only the spatial quadrature weights.
     * `"obliquity"` additionally applies the symmetric bistatic obliquity factor `\sqrt{\cos(\theta_{tx})\cos(\theta_{rx})}`. This is the default.
     * `"legacy"` applies the obliquity factor and the distance-dependent amplitude decay used by the former RGPR Kirchhoff migration implementation, proportional to `1/\sqrt{2\pi t v}`.
- `normalize`: Logical. If `TRUE`, divide every migrated pixel by the sum of the absolute migration weights contributing to that pixel. This reduces amplitude variations caused by changes in aperture size. This is not a display normalization.
- `spreading`: Logical. If `TRUE`, compensate for two-dimensional geometrical spreading by multiplying each contribution by `\sqrt{d_{tx}d_{rx}}`, where the distances are expressed in metres.
- `waveletfilter`: Character string specifying the wavelet-shaping filter:
    
     * `"none"` does not apply wavelet shaping.
     * `"halfderiv"` applies a half-derivative filter `\sqrt{i\omega}` along the time axis. This converts the three-dimensional point-source response into the two-dimensional line-source response expected by two-dimensional Kirchhoff summation.
- `antialias`: Logical. If `TRUE`, apply an anti-alias triangle filter to each migrated contribution. The filter half-width is based on the travel-time moveout between neighbouring traces.
- `aafactor`: Positive numeric scalar multiplying the anti-alias filter half-width. The default is `1`.
- `...`: Additional arguments passed to the migration implementation. Currently supported arguments include:
    
     * `max_depth`: maximum migration depth below the local ground surface, in metres. If omitted, it is estimated from the time axis and the velocity model.
     * `vel_mode`: travel-time algorithm. One of `"auto"`, `"constant"`, `"layered"`, or `"general"`.
     * `vel_dx`, `vel_dz`: horizontal and vertical spacing, in metres, of the regular velocity grid used by `vel_mode = "general"`.
     * `ray_step`: spacing, in metres, between slowness samples along a ray.
     * `n_ray`: maximum number of samples along each ray leg.

## Returns

The migrated `"GPR"` object. The data matrix is replaced by the migrated image, and its vertical axis is replaced by the migration-depth axis.

## Description

Migrate a two-dimensional GPR profile directly from the acquisition topography onto a regular distance-depth grid. The Kirchhoff migration supports zero-offset, common-offset, and bistatic antenna geometries.

## Details

Migration is performed directly from the acquisition surface. No elevation static correction is applied. This follows the principle of topographic Kirchhoff migration described by Dujardin and Bano (2013).

For every image point `\mathbf{p}`, the two-way travel time associated with trace `j` is calculated as

c("`\n`", "`t_j(\\mathbf{p}) =\n`", "`\\frac{\n`", "`  \\Vert \\mathbf{p} - \\mathbf{s}_j \\Vert +\n`", "`  \\Vert \\mathbf{p} - \\mathbf{r}_j \\Vert\n`", "`}{v},\n`")

where `\mathbf{s}_j` and `\mathbf{r}_j` are the transmitter and receiver coordinates, respectively, and `v` is the electromagnetic-wave velocity.

The main Kirchhoff migration processing steps are:

1. Construct a regular distance-depth output grid.
2. Mask image points situated above the local ground surface or below the specified maximum migration depth.
3. Calculate transmitter-to-pixel and receiver-to-pixel distances.
4. Select traces within the angular or Fresnel migration aperture.
5. Calculate bistatic travel times.
6. Linearly interpolate trace amplitudes at the calculated travel times.
7. Apply spatial-integration, obliquity, and optional spreading weights.
8. Sum the trace contributions into the migrated image.

The implementation assumes a two-dimensional profile, isotropic velocity, straight propagation paths, and coordinates expressed in metres.

With a non-constant velocity model, the travel time of each ray leg is obtained by integrating slowness along the straight segment between the antenna and the image point. Ray bending and refraction are not modelled.

The migrated vertical coordinates are stored as depth below the highest trace elevation. The original acquisition elevations remain available in the coordinate slot, while the migrated traces share the elevation of the highest input trace.

## References

Dujardin, J.-R. and Bano, M. (2013). Topographic migration of GPR data: Examples from Chad and Mongolia. **Comptes Rendus Geoscience**, 345(2), 73–80. tools:::Rd_expr_doi("10.1016/j.crte.2013.01.003")

Pérez-Gracia, V., Di Capua, D., Caselles, O., Rial, F., Lorenzo, H., González-Drigo, R. and Armesto, J. (2008). Horizontal resolution in a non-destructive shallow GPR survey: An experimental evaluation. **NDT & E International**, 41(8), 611–620. tools:::Rd_expr_doi("10.1016/j.ndteint.2008.06.002")


