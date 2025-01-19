# Returns hyperbolas associated with Vrms velociies

```r
CMPhyperbolas(x)

## S4 method for signature 'GPR'
CMPhyperbolas(x)
```

## Arguments

- `x`: (`GPR class`) A CMP object of the class `GPR`

## Returns

(`list`) A list element key `antsep` containing a numeric vector of `n` antenna separation values, and element `twt`

containing a `n \times m` matrix, where `m` is the number of hyperbolas (i.e., the number of velocities).

Returns hyperbolas associated with root-mean-square velocities stored in the CMP data.

## Examples

```r
## Not run:

HPB <- hyperbolaFromVrms(x)
plot(x, barscale = FALSE, main = "CMP")
matplot(HPB$antsep, HPB$twt, type = "l", col = "green", 
        lwd = 2, add = TRUE, lty = 1)
## End(Not run)
```