# Terrain ruggedness index

For a given focal grid cell, the terrain ruggedness index (TRI) is
calculated by taking the square root of the average of the squared
difference between the focal cell's elevation and the elevations of the
8 surrounding cells, or \$\$\sqrt(\sum\_{i = 1}^{8}(m_i - m_0)^2 /
8)\$\$ where \\m_0\\ is the elevation of the focal cell and \\m_i\\ is
the elevation of the *i*th grid cell. Areas that are entirely flat will
have a value of `NA` assigned to them.

By default, TRI is calculated for a 3x3 moving window around each focal
cell. However, you can use a larger-sized window. In this case, the
window size must be an odd number \>= 3, and you must have the `r.tri`
**GRASS** addon installed. If it is not installed, the function will try
to install it.

## Usage

``` r
# S4 method for class 'GRaster'
ruggedness(x, size = 3, exponent = 0)
```

## Arguments

- x:

  A `GRaster`.

- size:

  Integer (default is 3): Size of the moving window. Must be an odd
  integer \>= 3.

- exponent:

  Numeric \>= 0 and \<= 4. Used to reduce the influence of cells farther
  from the focal cell (larger areas can yield noisier results if the
  exponent is small). All cells are weighted equally when
  `exponent = 0`.

## Value

A `GRaster`.

## References

Riley, S.J., DeGloria, S.D., and Elliot, R. 1999. A terrain ruggedness
index that quantifies topographic heterogeneity. *Intermountain Journal
of Sciences* 5:23-27.

## See also

[`terrain()`](https://github.com/adamlilith/fasterRaster/reference/terrain.md),
[`wetness()`](https://github.com/adamlilith/fasterRaster/reference/wetness.md),
[`geomorphons()`](https://github.com/adamlilith/fasterRaster/reference/geomorphons.md)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert to GRaster:
elev <- fast(madElev)

# Topographic wetness index:
twi <- wetness(elev)
names(twi) <- 'TWI'
plot(c(elev, twi))

# Terrain ruggedness index:
tri <- ruggedness(elev)
tri7 <- ruggedness(elev, size = 7)
triSmooth7 <- ruggedness(elev, size = 7, exponent = 4)

tris <- c(elev, tri, tri7, triSmooth7)
names(tris) <- c("elevation", "TRI in 3x3", "TRI in 7x7", "Smoothed TRIin 7x7")
plot(tris)

}
```
