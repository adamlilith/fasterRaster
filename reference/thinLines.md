# Reduce linear features on a raster so linear features are 1 cell wide

The `thinLines()` function attempts to reduce linear features on a
raster to just 1 cell wide. You may need to run `thinLines()` multiple
times on the same raster (or experiment with the `iter` argument) to get
acceptable output. `thinLines()` can be helpful to run on a raster
before using
[`as.lines()`](https://github.com/adamlilith/fasterRaster/reference/as.lines.md).

## Usage

``` r
# S4 method for class 'GRaster'
thinLines(x, iter = 200)
```

## Arguments

- x:

  A `GRaster`.

- iter:

  Numeric integer: Number of iterations (default is 200).

## Value

A `GRaster`.

## See also

[`as.lines()`](https://github.com/adamlilith/fasterRaster/reference/as.lines.md),
**GRASS** manual page for tool `r.thin` (see `grassHelp("r.thin")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation
madElev <- fastData("madElev")

# Convert to GRaster:
elev <- fast(madElev)

# Thin elevation raster:
thinned <- thinLines(elev, iter = 300)
plot(thinned)

# Convert to lines:
rastToLines <- as.lines(thinned)
plot(rastToLines)

# We can clean this:
cleanLines <- fixDangles(x = rastToLines)
plot(rastToLines, col = "red")
plot(cleanLines, add = TRUE)

}
```
