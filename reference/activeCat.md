# Get or set the column with category labels in a categorical raster

These functions return or set the column of the labels to be matched to
each value in the raster of a categorical `GRaster` (see
[`vignette("GRasters", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/GRasters.md)).
*Important*: Following
[`terra::activeCat()`](https://rspatial.github.io/terra/reference/activeCat.html),
the first column in the "levels" table is ignored, so an "active
category" value of 1 means the second column is used as labels, a value
of 2 means the third is used, and so on.

- `activeCat()` returns the column of the labels to be matched to each
  value in the raster for a single raster layer.

- `activeCats()` does the same, but for all layers of a `GRaster`.

- `activeCat()<-` sets the column to be used as category labels.

## Usage

``` r
# S4 method for class 'GRaster'
activeCat(x, layer = 1, names = FALSE)

# S4 method for class 'GRaster'
activeCats(x, names = FALSE)

# S4 method for class 'GRaster'
activeCat(x, layer = 1) <- value

# S4 method for class 'GRaster'
activeCat(x, layer = 1) <- value

# S4 method for class 'GRaster'
activeCat(x, layer = 1) <- value
```

## Arguments

- x:

  A categorical `GRaster`.

- layer:

  Numeric, integer, logical, or character: Indicates for which layer(s)
  to get or set the active category column. This can be a number (the
  index of the raster(s)), a logical vector (`TRUE` ==\> get/set the
  active category column, `FALSE` ==\> leave as-is), or a character
  vector (names of layers).

- names:

  Logical: If `TRUE`, display the name(s) of the active column(s). If
  `FALSE` (default), report the index of the active column. Following
  [`terra::activeCat()`](https://rspatial.github.io/terra/reference/activeCat.html),
  the first column in the levels table is ignored. So, an active column
  of "1" means the second column is active. "2" means the third column
  is active, and so on.

- value:

  Numeric, integer, or character. Following
  [`terra::activeCat()`](https://rspatial.github.io/terra/reference/activeCat.html),
  the first column in each levels table is ignored. So, if you want the
  second column to be the category label, use 1. If you want the third
  column, use 2, and so on. You can also specify the active column by
  its column name (though this can't be the first column's name).

## Value

`activeCat()` returns an integer or character of the active column index
or name. `activeCats()` returns a vector of indices or names.
`activeCat()<-` returns a `GRaster`.

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data: Land cover raster
madCover <- fastData("madCover")

# Convert categorical SpatRaster to categorical GRaster:
cover <- fast(madCover)

### Properties of categorical rasters

cover # note categories
is.factor(cover) # Is the raster categorical?
nlevels(cover) # number of levels
levels(cover) # just the value and active column
cats(cover) # all columns
minmax(cover) # min/max values
minmax(cover, levels = TRUE) # min/max categories
catNames(cover) # column names of the levels table
missingCats(cover) # categories in table with no values in raster
freq(cover) # frequency of each category (number of cells)
zonalGeog(cover) # geometric statistics

### Active column

# Which column sets the category labels?
activeCat(cover)
activeCat(cover, names = TRUE)

activeCats(c(cover, cover))

# Choose a different column for category labels:
levels(cover)
activeCat(cover) <- 2
levels(cover)

### Managing levels tables

# Remove unused levels:
nlevels(cover)
cover <- droplevels(cover)
nlevels(cover)

# Re-assign levels:
value <- c(20, 30, 40, 50, 120, 130, 140, 170)
label <- c("Cropland", "Cropland", "Forest", "Forest",
 "Grassland", "Shrubland", "Herbaceous", "Flooded")

newCats <- data.frame(value = value, label = label)

cover <- categories(cover, layer = 1, value = newCats)
cats(cover)

# This is the same as:
levels(cover) <- newCats
cats(cover)

# Are there any values not assigned a category?
missingCats(cover)

# Let's assign a category for value 210 (water):
water <- data.frame(value = 210, label = "Water")
addCats(cover) <- water
levels(cover)

# Add more information to the levels table using merge():
landType <- data.frame(
     Value = c(20, 30, 40, 50, 120),
     Type = c("Irrigated", "Rainfed", "Broadleaf evergreen",
     "Broadleaf deciduous", "Mosaic with forest")
)
cats(cover)
cover <- addCats(cover, landType, merge = TRUE)
cats(cover)

### Logical operations on categorical rasters

cover < "Forest" # 1 for cells with a value < 40, 0 otherwise
cover <= "Forest" # 1 for cells with a value < 120, 0 otherwise
cover == "Forest" # 1 for cells with value of 40-120, 0 otherwise
cover != "Forest" # 1 for cells with value that is not 40-120, 0 otherwise
cover > "Forest" # 1 for cells with a value > 120, 0 otherwise
cover >= "Forest" # 1 for cells with a value >= 120, 0 otherwise

cover %in% c("Cropland", "Forest") # 1 for cropland/forest cells, 0 otherwise

### Combine categories from different rasters

# For the example, will create a second categorical raster fromm elevation.

# Divide elevation raster into "low/medium/high" levels:
madElev <- fastData("madElev")
elev <- fast(madElev)
elev <- project(elev, cover, method = "near") # convert to same CRS
fun <- "= if(madElev < 100, 0, if(madElev < 400, 1, 2))"
elevCat <- app(elev, fun)

levs <- data.frame(
     value = c(0, 1, 2),
     elevation = c("low", "medium", "high")
)
levels(elevCat) <- list(levs)

# Combine levels:
combined <- concats(cover, elevCat)
combined
levels(combined)

# Combine levels, treating value/NA combinations as new categories:
combinedNA <- concats(cover, elevCat, na.rm = FALSE)
combinedNA
levels(combinedNA)

}
```
