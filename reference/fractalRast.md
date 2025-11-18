# Create fractal raster

`fractalRast()` creates a raster with a fractal pattern.

## Usage

``` r
# S4 method for class 'GRaster'
fractalRast(x, n = 1, mu = 0, sigma = 1, dimension = 2.05)
```

## Arguments

- x:

  A `GRaster`. The output will have the same extent and dimensions as
  this raster.

- n:

  A numeric integer: Number of rasters to generate.

- mu, sigma:

  Numeric: Mean and sample standard deviation of output.

- dimension:

  Numeric: Fractal dimension. Must be between 2 and 3.

## Value

A `GRaster`.

## See also

[`rSpatialDepRast()`](https://github.com/adamlilith/fasterRaster/reference/rSpatialDepRast.md),
[`rNormRast()`](https://github.com/adamlilith/fasterRaster/reference/rnormRast.md),
[`rUnifRast()`](https://github.com/adamlilith/fasterRaster/reference/runifRast.md),
[`rWalkRast()`](https://github.com/adamlilith/fasterRaster/reference/rWalkRast.md),
**GRASS** manual page for tool `r.surf.fractal` (see
`grassHelp("r.surf.fractal")`)

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
