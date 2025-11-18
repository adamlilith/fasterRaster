# Spatial bounds of a GRaster or GVector

These functions return the extent of a `GSpatial` object (`GRegions`,
`GRaster`s, and `GVector`s):

- `ext()`: 2-dimensional spatial extent (i.e., westernmost/easternmost
  and southernmost/northernmost coordinates of area represented).  

- `zext()`: Vertical extent (i.e., topmost and bottom-most elevation of
  the volume represented). The vertical extent is not `NA` only if the
  object is 3-dimensional.  

- `W()`, `E()`, `N()`, `S()`: Coordinates of one side of horizontal
  extent.  

- `top()` and `bottom()`: Coordinates of top and bottom of vertical
  extent.  

## Usage

``` r
# S4 method for class 'missing'
ext(x, vector = FALSE)

# S4 method for class 'GSpatial'
ext(x, vector = FALSE)

# S4 method for class 'missing'
zext(x)

# S4 method for class 'GSpatial'
zext(x)

# S4 method for class 'missing'
W(x, char = FALSE)

# S4 method for class 'GSpatial'
W(x, char = FALSE)

# S4 method for class 'missing'
E(x, char = FALSE)

# S4 method for class 'GSpatial'
E(x, char = FALSE)

# S4 method for class 'missing'
N(x, char = FALSE)

# S4 method for class 'GSpatial'
N(x, char = FALSE)

# S4 method for class 'missing'
S(x, char = FALSE)

# S4 method for class 'GSpatial'
S(x, char = FALSE)

# S4 method for class 'missing'
top(x, char = FALSE)

# S4 method for class 'GSpatial'
top(x, char = FALSE)

# S4 method for class 'GSpatial'
bottom(x, char = FALSE)

# S4 method for class 'GSpatial'
bottom(x, char = FALSE)
```

## Arguments

- x:

  An object that inherits from `GSpatial` (i.e., a `GRaster` or
  `GVector`) or missing. If missing, then the horizontal or vertical
  extent of the currently active "region" is returned (see
  [`vignette("regions", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/regions.md)).

- vector:

  Logical: If `FALSE` (default), return a `SpatExtent` object. If
  `TRUE`, return the extent as a named vector.

- char:

  Logical: If `FALSE` (default), return a numeric value. If `TRUE`,
  return as a character.

## Value

The returned values depend on the function:

- `ext()`: A `SpatExtent` object (**terra** package) or a numeric
  vector.

- `zext()`: A numeric vector.

- `W()`, `E()`, `N()`, `S()`, `top()`, and `bottom()`: A numeric value
  or character.

## See also

[`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html),
[`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev")
madForest2000 <- fastData("madForest2000")
madCoast0 <- fastData("madCoast0")
madRivers <- fastData("madRivers")
madDypsis <- fastData("madDypsis")

### GRaster properties

# convert SpatRasters to GRasters
elev <- fast(madElev)
forest <- fast(madForest2000)

# plot
plot(elev)

dim(elev) # rows, columns, depths, layers
nrow(elev) # rows
ncol(elev) # columns
ndepth(elev) # depths
nlyr(elev) # layers

res(elev) # resolution

ncell(elev) # cells
ncell3d(elev) # cells (3D rasters only)

topology(elev) # number of dimensions
is.2d(elev) # is it 2D?
is.3d(elev) # is it 3D?

minmax(elev) # min/max values

# name of object in GRASS
sources(elev)

# "names" of the object
names(elev)

# coordinate reference system
crs(elev)

# extent (bounding box)
ext(elev)

# data type
datatype(elev)

# assigning
copy <- elev
copy[] <- pi # assign all cells to the value of pi
copy

# concatenating multiple GRasters
rasts <- c(elev, forest)
rasts

# adding a raster "in place"
add(rasts) <- ln(elev)
rasts

# subsetting
rasts[[1]]
rasts[["madForest2000"]]

# assigning
rasts[[4]] <- elev > 500

# number of layers
nlyr(rasts)

# names
names(rasts)
names(rasts) <- c("elev_meters", "forest", "ln_elev", "high_elevation")
rasts

### GVector properties

# convert sf vectors to GVectors
coast <- fast(madCoast4)
rivers <- fast(madRivers)
dypsis <- fast(madDypsis)

# extent
ext(rivers)

W(rivers) # western extent
E(rivers) # eastern extent
S(rivers) # southern extent
N(rivers) # northern extent
top(rivers) # top extent (NA for 2D rasters like this one)
bottom(rivers) # bottom extent (NA for 2D rasters like this one)

# coordinate reference system
crs(rivers)
st_crs(rivers)

# column names and data types
names(coast)
datatype(coast)

# name of object in GRASS
sources(rivers)

# points, lines, or polygons?
geomtype(dypsis)
geomtype(rivers)
geomtype(coast)

is.points(dypsis)
is.points(coast)

is.lines(rivers)
is.lines(dypsis)

is.polygons(coast)
is.polygons(dypsis)

# dimensions
nrow(rivers) # how many spatial features
ncol(rivers) # hay many columns in the data frame

# number of geometries and sub-geometries
ngeom(coast)
nsubgeom(coast)

# 2- or 3D
topology(rivers) # dimensionality
is.2d(elev) # is it 2D?
is.3d(elev) # is it 3D?

# Update values from GRASS
# (Reads values from GRASS... will not appear to do anything in this case)
coast <- update(coast)

### operations on GVectors

# convert to data frame
as.data.frame(rivers)
as.data.table(rivers)

# subsetting
rivers[c(1:2, 5)] # select 3 rows/geometries
rivers[-5:-11] # remove rows/geometries 5 through 11
rivers[ , 1] # column 1
rivers[ , "NAM"] # select column
rivers[["NAM"]] # select column
rivers[1, 2:3] # row/geometry 1 and column 2 and 3
rivers[c(TRUE, FALSE)] # select every other geometry (T/F vector is recycled)
rivers[ , c(TRUE, FALSE)] # select every other column (T/F vector is recycled)

# removing data table
noTable <- dropTable(rivers)
noTable
nrow(rivers)
nrow(noTable)

# Refresh values from GRASS
# (Reads values from GRASS... will not appear to do anything in this case
# since the rivers object is up-to-date):
rivers <- update(rivers)

# Concatenating multiple vectors
rivers2 <- rbind(rivers, rivers)
dim(rivers)
dim(rivers2)

}
```
