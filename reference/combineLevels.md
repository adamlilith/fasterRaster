# Combine levels table from multiple categorical GRasters

This function creates a single "levels" table from the levels tables of
one or more categorical `GRaster`s.

The difference between this function and
[`concats()`](https://github.com/adamlilith/fasterRaster/reference/concats.md)
is that
[`concats()`](https://github.com/adamlilith/fasterRaster/reference/concats.md)
creates a "combined" `GRaster` with a combined levels table, whereas
this one just merges the levels tables.

## Usage

``` r
# S4 method for class 'GRaster'
combineLevels(x, ...)

# S4 method for class 'list'
combineLevels(x, ...)
```

## Arguments

- x:

  A `GRaster` or a `list` of `GRaster`s.

- ...:

  Arguments to pass to
  [`data.table::merge()`](https://rdatatable.gitlab.io/data.table/reference/merge.html).

## Value

A `list` with a "levels" table (a `data.frame` or `data.table`), and the
active category number for the new table. Following
[`terra::activeCat()`](https://rspatial.github.io/terra/reference/activeCat.html),
the number is offset by 1, so a value of 1 indicates that the second
column in the table should be used for the category labels, a value of 2
indicates the third column should be used, and so on.

## See also

[`concats()`](https://github.com/adamlilith/fasterRaster/reference/concats.md),
[terra::concats](https://rspatial.github.io/terra/reference/concats.html),
[`vignette("GRasters", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/GRasters.md)

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
