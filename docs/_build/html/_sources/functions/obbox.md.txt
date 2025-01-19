# Oriented bounding box

## Source

source "whuber" from stackexchange.com, https://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points/181883#181883

```r
obbox(x)

## S4 method for signature 'GPR'
obbox(x)

## S4 method for signature 'GPRsurvey'
obbox(x)

## S4 method for signature 'matrix'
obbox(x)

## S4 method for signature 'sfc'
obbox(x)

## S4 method for signature 'sf'
obbox(x)
```

## Arguments

- `x`: (`GPR class`) An object of the class `GPR`

## Returns

(`matrix[5,2]`) The coordinates of the corners of the oriented bounding box, whereby the last row is identical to the first row. FIXME!!

Returns the oriented bounding box of the trace position of the survey.

## Details

The algorithm you are looking for is known in polygon generalisation as "smallest surrounding rectangle". Compute the convex hull of the cloud. For each edge of the convex hull: compute the edge orientation (with arctan), rotate the convex hull using this orientation in order to compute easily the bounding rectangle area with min/max of x/y of the rotated convex hull, Store the orientation corresponding to the minimum area found, Return the rectangle corresponding to the minimum area found. In 3D, the same applies, except: The convex hull will be a volume, The orientations tested will be the orientations (in 3D) of the convex hull faces.