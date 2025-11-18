# Combine values/categories of multiple GRasters into a single GRaster

This function takes from 2 to 10 integer or categorical (factor)
`GRaster`s and creates a single `GRaster` that has one value per
combination of values in the inputs. For example, say that there were
two input rasters, with values 1 and 2 in the one raster, and 3 and 4 in
the other. If the following combinations of values occurred between the
two rasters, then the output raster would be re-coded with the new
values:

|                 |                 |                 |
|-----------------|-----------------|-----------------|
| `input_raster1` | `input_raster2` | `output_raster` |
| 1               | 3               | 0               |
| 1               | 4               | 1               |
| 2               | 3               | 2               |
| 2               | 4               | 3               |

If the argument `na.rm` is set to `TRUE` (which it is, by default), then
whenever at least one cell has an `NA` value, then the output will also
have an `NA` (i.e., a new category number is not created). However, if
`na.rm` is `FALSE`, then combinations that include an `NA` are assigned
a new category number, unless all values are `NA` (in which case the
output will be `NA`).

The difference between this function and
[`combineLevels()`](https://github.com/adamlilith/fasterRaster/reference/combineLevels.md)
is that this one creates a "combined" `GRaster` with a combined levels
table, whereas
[`combineLevels()`](https://github.com/adamlilith/fasterRaster/reference/combineLevels.md)
just merges the levels tables.

If the inputs are all categorical rasters, then a
[`levels()`](https://github.com/adamlilith/fasterRaster/reference/levels.md)
table will also be returned with the new levels.

## Usage

``` r
# S4 method for class 'GRaster'
concats(x, ..., na.rm = TRUE)
```

## Arguments

- x:

  A `GRaster` with one or more layers, each of which must be have cells
  that represent integers or categories (factors).

- ...:

  Either missing or integer/categorical (factor) `GRaster`s.

- na.rm:

  Logical: If `TRUE` (default), then any combinations that include an
  `NA` cell will result in an `NA` cell in the output.

## Value

A `GRaster`. If the inputs are all categorical (factor) rasters, then a
levels table will also be returned with the new combined levels.

## See also

[`combineLevels()`](https://github.com/adamlilith/fasterRaster/reference/combineLevels.md),
[`terra::concats()`](https://rspatial.github.io/terra/reference/concats.html),
[`vignette("GRasters", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/GRasters.md),
**GRASS** manual page for tool `r.cross` (see `grassHelp("r.cross")`)

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
