# Set and get categories for categorical rasters

`GRaster`s can represent categorical data. Cell values are actually
integers, each corresponding to a category, such as "desert" or
"wetland." A categorical raster is associated with a table that matches
each value to a category name. The table must be `NULL` (i.e., no
categories–so not a categorical raster), or have at least two columns.
The first column must have integers and represent raster values. One or
more subsequent columns must have category labels. The column with these
labels is the "active category".

- `levels()`: Displays the "levels" table of a raster (just the value
  and active category columns).

- `cats()`: Displays the entire "levels" table of a raster.

- `levels()<-`: (Re)assigns the "levels" table to each layer of a
  raster. Assigning a "levels" table to an integer raster makes it a
  categorical raster.

- `categories()`: (Re)assigns the "levels" table to specific layer(s) of
  a raster.

- For a complete list of functions relevant to categorical rasters, see
  \`vignette("GRasters", package = "fasterRaster")).

## Usage

``` r
# S4 method for class 'GRaster'
levels(x)

# S4 method for class 'GRaster'
cats(x, layer = 1:nlyr(x))

# S4 method for class 'GRaster'
categories(x, layer = 1, value, active = 1)

# S4 method for class 'GRaster,data.frame'
levels(x) <- value

# S4 method for class 'GRaster,data.table'
levels(x) <- value

# S4 method for class 'GRaster,GRaster'
levels(x) <- value

# S4 method for class 'GRaster,list'
levels(x) <- value
```

## Arguments

- x:

  A `GRaster`.

- layer:

  Numeric integers, logical vector, or character: For `cats()` and
  `categories()`, this specifies the layer(s)for which to obtain
  level(s).

- value:

  A `data.frame`, `data.table`, a list of `data.frames` or `data.tables`
  with one per raster layer, or a categorical `SpatRaster`. The table's
  first column is the "value" column and must contain numeric values (of
  class `numeric` or `character`). If a `SpatRaster` is supplied, then
  its categories will be transferred to the `GRaster`.

- active:

  An integer or a character: The index or column name of the column used
  for category labels (the "active column"). Following
  [`terra::activeCat()`](https://rspatial.github.io/terra/reference/activeCat.html),
  the first column of the "levels" table is ignored, so a value of 1
  means to use the second column of the table for labels. A value of 2
  means to use the third column, and so on.

## Value

Values returned are:

- `levels()` and `cats()`: A list of `data.frame`s or `data.table`s, one
  per raster layer.

- `levels()<-` and `categories()`: A `GRaster`.

## See also

[`terra::levels()`](https://rspatial.github.io/terra/reference/factors.html),
`levels<-`,
[`terra::cats()`](https://rspatial.github.io/terra/reference/factors.html),
[`terra::categories()`](https://rspatial.github.io/terra/reference/factors.html),
see
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
