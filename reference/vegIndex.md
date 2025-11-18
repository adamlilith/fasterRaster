# Vegetation indices from surface reflectance

This function calculates one of many types of vegetation indices from a
raster with four bands representing blue (B), green (G), red (R), and
near infrared (NIR), plus possibly channels 5 and 7. The function
requires rasters that represent surface reflectance, so should have
values that fall in the range 0 to 1, unless they are digital number
rasters (e.g., integers in the range 0 to 255). If digital number format
is used, then the `bits` argument should be defined.

## Usage

``` r
# S4 method for class 'GRaster'
vegIndex(
  x,
  index = "NDVI",
  r = NULL,
  g = NULL,
  b = NULL,
  nir = NULL,
  b5 = NULL,
  b7 = NULL,
  soilSlope = NULL,
  soilIntercept = NULL,
  soilNR = 0.08,
  bits = NULL
)
```

## Arguments

- x:

  A `GRaster` with one layer per required band. Values should be between
  0 and 1.

- index:

  Character or character vector: The vegetation index or indices to
  calculate. You can find a list of available indices using
  [fastData("vegIndices")](https://github.com/adamlilith/fasterRaster/reference/fastData.md)
  (also see
  [vegIndices](https://github.com/adamlilith/fasterRaster/reference/vegIndices.md)).
  The first column, "`index`" provides the name of the index, and these
  are the values that this argument will accept (e.g., "NDVI", "EVI2").
  Partial matching is used, and case is ignored. You can also use these
  shortcuts:

  - `"*"`: Calculate *all* indices

  - `"RNIR"`: Calculate all indices that use R and NIR channels (but not
    other channels).

  - `"NotSoil"`: Calculate all indices that use any channels but do not
    require `soilSlope` or `soilIntercept`.

  *Note*: A near-comprehensive table of indices can be found on the
  [Index Database: A Database for Remote Sensing
  Indices](https://www.indexdatabase.de).

- r, g, b, nir:

  Numeric or character: Index or
  [`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md)
  of the layers in `x` that represent the red, green, blue, and near
  infrared channels. Values must be in the range from 0 to 1 or
  integers.

- b5, b7:

  Numeric or character: Index of names of the layers representing bands
  5 and 7. These are used only for GVI and PVI. Values must be in the
  range from 0 to 1 or integers.

- soilSlope, soilIntercept, soilNR:

  Numeric: Values of the soil slope, intercept, and soil noise reduction
  factor (0.08, by default). Used only for calculation of MSAVI.

- bits:

  Either `NULL` (default) or numeric integer or integer with a value of
  7, 8, 10, or 16: If the rasters are represented by integers (so do not
  fall in the range of 0 to 1), then the number of bits can be supplied
  using `bits`. If this is the case, then they will range from 0 to
  2^`n`, where `n` is 7, 8, 10, or 16. If bit rasters are supplied, they
  must be of
  [`datatype()`](https://github.com/adamlilith/fasterRaster/reference/datatype.md)
  "integer". If raster values are in the range from 0 to 1, then `bits`
  should be `NULL` (default).

## Value

A `GRaster`.

## See also

**GRASS** manual page for tool `i.vi` (see `grassHelp("i.vi")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation raster, rivers vector
madLANDSAT <- fastData("madLANDSAT")

# Convert a SpatRaster to a GRaster:
landsat <- fast(madLANDSAT)

# See available vegetation indices:
vegIndices

# Normalized Difference Vegetation Index and Enhanced Vegetation Index:
indices <- c("ndvi", "evi")
vi <- vegIndex(landsat, index = indices, r = 1, b = 3, nir = 4, bits = 8)
plot(vi)

# All indices using R and NIR:
rnir <- vegIndex(landsat, index = "rnir", r = 1, nir = 4, bits = 8)

# Note: Some values are highly skewed
plot(rnir)

}
```
