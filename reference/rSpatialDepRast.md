# Create a random raster with or without spatial dependence

`rSpatialDepRast()` creates a raster with random values in cells. Across
the raster, values are approximately normally distributed, though a
raster with a "true" normal distribution can be made with
[`rNormRast()`](https://github.com/adamlilith/fasterRaster/reference/rnormRast.md).
Spatial dependence can be introduced, though all together the values
will still be approximately normally distributed.

## Usage

``` r
# S4 method for class 'GRaster'
rSpatialDepRast(
  x,
  n = 1,
  mu = 0,
  sigma = 1,
  dist = 0,
  exponent = 1,
  delay = 0,
  seed = NULL
)
```

## Arguments

- x:

  A `GRaster`: The output will have the same extent and dimensions as
  this raster.

- n:

  An integer: Number of rasters to generate.

- mu, sigma:

  Numeric: Mean and sample standard deviation of output. If creating
  more than one raster, you can provide one value per raster. If there
  are fewer, they will be recycled.

- dist:

  Numeric: Maximum distance of spatial autocorrelation (in map
  units–typically meters). Default is 0 (no spatial autocorrelation). If
  creating more than one raster, you can provide one value per raster.
  If there are fewer, values will be recycled.

- exponent:

  Numeric \> 0: Distance decay exponent. If creating more than one
  raster, you can provide one value per raster. If there are fewer,
  values will be recycled.

- delay:

  Numeric \>= 0: Values \>0 force the distance decay of similarity to
  remain constant up to this distance. Beyond this distance, the decay
  exponent takes effect. Default is 0. If creating more than one raster,
  you can provide one value per raster. If there are fewer, values will
  be recycled.

- seed:

  Numeric integer or `NULL`: Random seed. If `NULL`, then a random seed
  is used for each raster. If provided, there should be one seed value
  per raster.

## Value

A `GRaster`.

## See also

[`rNormRast()`](https://github.com/adamlilith/fasterRaster/reference/rnormRast.md),
[`fractalRast()`](https://github.com/adamlilith/fasterRaster/reference/fractalRast.md),
[`rUnifRast()`](https://github.com/adamlilith/fasterRaster/reference/runifRast.md),
[`rWalkRast()`](https://github.com/adamlilith/fasterRaster/reference/rWalkRast.md),
**GRASS** manual page for tool `r.random.surface` (see
`grassHelp("r.random.surface")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

### Create a raster with values drawn from a uniform distribution:
unif <- rUnifRast(elev)
plot(unif)

### Create a raster with values drawn from a normal distribution:
norms <- rNormRast(elev, n = 2, mu = c(5, 10), sigma = c(2, 1))
plot(norms)
hist(norms, bins = 100)

# Create a raster with random, seemingly normally-distributed values:
rand <- rSpatialDepRast(elev, dist = 1000)
plot(rand)

# Values appear normal on first inspection:
hist(rand)

# ... but actually are patterned:
hist(rand, bins = 100)

# Create a fractal raster:
fractal <- fractalRast(elev, n = 2, dimension = c(2.1, 2.8))
plot(fractal)
hist(fractal)

### Random walker rasters

# One random walker
walk <- rWalkRast(elev)
plot(walk)

# Random walker with self-avoidance:
walkAvoid <- rWalkRast(elev, steps = 1000, avoid = TRUE, seed = 1)
plot(walkAvoid)

# 10 random walkers:
walk10 <- rWalkRast(elev, n = 10)
plot(walk10)

# 10 random walkers starting in same place:
walkSame10 <- rWalkRast(elev, n = 10, sameStart = TRUE)
plot(walkSame10)


}
```
