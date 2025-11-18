# Center and scale a GRaster, or the opposite

`scale()` and `scalepop()` center and scale layers in a `GRaster` by
subtracting from each raster its mean value (centering), then dividing
by its standard deviation (scaling). This is useful for using the raster
in a linear model, for example, because unscaled predictors can lead to
numerical instability. The `scale()` function uses the sample standard
deviation, and the `scalepop()` function uses the population standard
deviation. For even moderately-sized rasters, the difference between
these two is negligible, but the `scalepop()` function can be much
faster than the `scale()` function.

The `unscale()` function does the opposite of `scale()` and
`scalepop()`: it multiples each layer by a value (presumably, its
standard deviation), and adds another value (presumably, its mean).

## Usage

``` r
# S4 method for class 'GRaster'
scale(x, center = TRUE, scale = TRUE)

# S4 method for class 'GRaster'
scalepop(x, center = TRUE, scale = TRUE)

# S4 method for class 'GRaster'
unscale(x, center = NULL, scale = NULL)
```

## Arguments

- x:

  A `GRaster`.

- center:

  Value depends on the function:

  - `scale()`:

    - Logical: If `TRUE` (default), subtract from each raster layer its
      mean. If `FALSE`, do not.

    - Numeric: A single value, in which case the same value will be used
      across all layers of `x`, or one value per layer in `x`.

  - `unscale()`: Numeric vector or `NULL` (default): This can be a
    single value, which will be recycled if there is more than one layer
    in the raster, or one value per raster layer. If a value is `NA`,
    then no un-centering will be performed on the relevant raster layer.
    If `NULL`, then no un-centering is done.

- scale:

  Value depends on the function:

  - `scale()`:

    - Logical: If `TRUE` (default), divide each raster layer by its
      standard deviation. If `FALSE`, do not.

    - Numeric: A single value, in which case the same value will be used
      across all layers of `x`, or one value per layer in `x`.

  - `unscale()`: Numeric vector or `NULL` (default): This can be a
    single value, which will be recycled if there is more than one layer
    in the raster, or one value per raster layer. If a value is `NA`,
    then no unscaling will be done on the relevant raster layer. If
    `NULL`, then no un-scaling is done.

## Value

All functions return a `GRaster`. The output of `scale()` and
`scalepop()` will have two attributes, "center" and "scale", which have
the means and standard deviations of the original rasters (if `center`
and `scale` are `TRUE`, otherwise, they will be `NA`). These can be
obtained using `attributes(output_raster)$center` and
`attributes(output_raster)$scale`.

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Climate rasters:
madChelsa <- fastData("madChelsa")

# Convert to GRasters:
chelsa <- fast(madChelsa)

### Center and scale rasters
# Scale with using sample SD:
chScaled <- scale(chelsa)
chScaled

# Scale with using population SD:
chScaledPop <- scalepop(chelsa)
chScaledPop

# Means are very close to 0 and SDs to 1:
global(chScaled, c("mean", "sd", "min", "max"))
global(chScaledPop, c("mean", "sd", "min", "max"))

# Get original means and sd's:
centers <- attributes(chScaled)$center
scales <- attributes(chScaled)$scale
centers
scales

### Unscale rasters:
chUnscaled <- unscale(chScaled, center = centers, scale = scales)

# Means and SD are returned to original values:
global(chUnscaled, c("mean", "sd", "min", "max")) # unscaled
global(chelsa, c("mean", "sd", "min", "max")) # original

}
```
