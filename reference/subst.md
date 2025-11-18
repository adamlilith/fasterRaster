# Replace a specific value(s) in a GRaster

This function replaces one or more user-specified values in a raster
with other values. See
[`classify()`](https://github.com/adamlilith/fasterRaster/reference/classify.md)
for replacing ranges of values.

## Usage

``` r
# S4 method for class 'GRaster'
subst(x, from, to, others = NULL, warn = TRUE)
```

## Arguments

- x:

  A `GRaster`.

- from, to:

  Vectors of numeric or character values. The value(s) in `from` will be
  replaced with the value(s) in `to`. They must be the same length, or,
  if you supply a single value for `to`, then all values in `from` will
  be converted to the same value of `to`. Numeric/integer or character
  vectors can be used:

  - `from` and `to` are numeric or integer vectors: Values in `from`
    will be replaced by their corresponding value in `to`.

  - `from` and `to` are character vectors: You can use a character
    vector for `from` and `to`. In this case, the input raster must be a
    factor (categorical) raster or an integer raster. If `from` is a
    character vector, these levels(categories) will be replaced by the
    levels in `to.` This can add levels to a `GRaster` `to` has labels
    that do not match any existing labels in `from`.

  - `from` is a character vector and `to` is an integer vector: Cells in
    `x` that correspond to the given label will have their values
    replaced by the corresponding value in `to`, and be matched the the
    corresponding label. If no label corresponds to the new value, a new
    level will be created. The input must be a categorical raster.

  - `from` is an integer vector and `to` is a character vector: Cells in
    `x` that have a value in `from` will be replaced by values that
    match the labels in `to`. If the input raster does not have a value
    that corresponds to the given label in `y`, then a new value will be
    created.

- others:

  `NULL` (default), `NA`, numeric, or a character:

  - `NULL` (default): Values that do not appear in `from` will be
    unchanged.

  - `NA`: Values that do not appear in `from` will be set to `NA`.

  - Character: Cells in `x` that do not appear in `to` will be assigned
    to this level. In this case, `x` must be a categorical (factor)
    raster.

- warn:

  Logical: If `TRUE` (default), display a warning when new levels are
  created.

## Value

A `GRaster`.

## See also

[`terra::subst()`](https://rspatial.github.io/terra/reference/subst.html),
[`classify()`](https://github.com/adamlilith/fasterRaster/reference/classify.md)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madCover <- fastData("madCover")

### Substitution within an integer/numeric raster

# Convert SpatRaster to GRaster
elev <- fast(madElev)

# Simple substitution of one value, keeping all other values
newElev <- elev
newElev[newElev == 100] <- -100
newElev[newElev > 500] <- 500
hist(newElev)

# Simple substitution of one value, keeping all other values
substituted <- subst(elev, from = 300, to = -300)
substituteds <- c(elev, substituted)
names(substituteds) <- c("original", "substituted")
plot(substituteds)

# Simple substitution of three values, keeping all other values
substituted <- subst(elev, from = c(299, 300, 301), to = c(-699, -600, -601))
substituteds <- c(elev, substituted)
names(substituteds) <- c("original", "substituted")
plot(substituteds)

# Simple substitution of three values to one other value, retaining remainder
substituted <- subst(elev, from = c(299, 300, 301), to = -1000)
substituteds <- c(elev, substituted)
names(substituteds) <- c("original", "substituted")
plot(substituteds)

# Simple substitution of one value, setting all other values to 100
substituted <- subst(elev, from = 300, to = -300, others = 100)
substituteds <- c(elev, substituted)
names(substituteds) <- c("original", "substituted")
plot(substituteds)

### Substitution within a factor/categorical raster

# Convert a SpatRaster to a GRaster:
cover <- fast(madCover)

cover <- droplevels(cover) # remove unused levels
levels(cover) # levels of "cover"

# Substitute using level name, replace with EXISTING level label
from <- "Mosaic cropland/vegetation"
to <- "Mosaic crops"
categ <- subst(cover, from = from, to = to)
freq(cover) # original frequencies of each land cover class
freq(categ) # note change in frequency of "from" and "to" categories
plot(c(cover, categ))

# Substitute using level name, replace with NEW level label
from <- c("Mosaic crops", "Mosaic cropland/vegetation")
to <- c("Mixed cropland")
categ <- subst(cover, from = from, to = to)
freq(cover) # original frequencies of each land cover class
freq(categ) # note change in frequency of "from" and "to" categories
plot(c(cover, categ))

# Substitute using level name, replace with NEW level label
from <- c("Mosaic crops", "Mosaic cropland/vegetation")
to <- c("Mixed cropland", "Mixed cropland/vegetation")
categ <- subst(cover, from = from, to = to)
freq(cover) # original frequencies of each land cover class
freq(categ) # note change in frequency of "from" and "to" categories
plot(c(cover, categ))

# Substitute using level name, replace with VALUE of an existing label
from <- c("Mosaic crops", "Mosaic cropland/vegetation")
to <- 120
categ <- subst(cover, from = from, to = to)
freq(cover) # original frequencies of each land cover class
freq(categ) # note change in frequency of "from" and "to" categories
plot(c(cover, categ))

# Substitute using level name, replace with new level name, replace all others
from <- c("Mosaic crops", "Mosaic cropland/vegetation")
to <- "Crops"
categ <- subst(cover, from = from, to = to, others = "Other")
freq(cover) # original frequencies of each land cover class
freq(categ) # note change in frequency of "from" and "to" categories
plot(c(cover, categ))

}
```
