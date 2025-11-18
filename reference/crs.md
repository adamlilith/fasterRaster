# Coordinate reference system of a GRaster or GVector

Get the coordinate reference system (CRS) of a `GRaster` or `GVector`s,
or from the currently active **GRASS** "project/location" (see
[`vignette("projects_mapsets", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/projects_mapsets.md)):

- `crs()`: Return a WKT string (an object of class `character`).

- `st_crs()`: Return a `crs` object (from the **sf** package).

- `coordRef()`: Return a `list`.

## Usage

``` r
# S4 method for class 'missing'
crs(x)

# S4 method for class 'GLocation'
crs(x)

# S4 method for class 'missing'
st_crs(x, ...)

# S4 method for class 'GLocation'
st_crs(x, ...)

st_crs(x, ...)

# S4 method for class 'missing'
coordRef(x)

# S4 method for class 'GRaster'
coordRef(x)

# S4 method for class 'GVector'
coordRef(x)
```

## Arguments

- x:

  An object that inherits from a `GLocation` (i.e., a `GRaster` or
  `GVector`) or missing. If missing, the coordinate reference system of
  the currently active **GRASS** "project/location" is reported.

- ...:

  Other arguments (generally unused).

## Value

Function `crs()` returns a `character` object, `st_crs()` returns `crs`
object, and `coordRef()` a `list`.

## See also

[`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html),
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madForest2000 <- fastData("madForest2000")

# Convert SpatRasters to GRasters
elev <- fast(madElev)
forest <- fast(madForest2000)

### GRaster properties

# plotting
plot(elev)

# dimensions
dim(elev) # rows, columns, depths, layers
nrow(elev) # rows
ncol(elev) # columns
ndepth(elev) # depths
nlyr(elev) # layers

res(elev) # resolution (2D)
res3d(elev) # resolution (3D)
zres(elev) # vertical resolution
xres(elev) # vertical resolution
yres(elev) # vertical resolution
zres(elev) # vertical resolution (NA because this is a 2D GRaster)

# cell counts
ncell(elev) # cells
ncell3d(elev) # cells (3D rasters only)

# number of NA and non-NA cells
nacell(elev)
nonnacell(elev)

# topology
topology(elev) # number of dimensions
is.2d(elev) # is it 2-dimensional?
is.3d(elev) # is it 3-dimensional?

minmax(elev) # min/max values

# "names" of the object
names(elev)

# coordinate reference system
crs(elev)
st_crs(elev)
coordRef(elev)

# extent (bounding box)
ext(elev)

# vertical extent (not defined for this raster)
zext(elev)

# data type
datatype(elev) # fasterRaster type
datatype(elev, "GRASS") # GRASS type
datatype(elev, "terra") # terra type
datatype(elev, "GDAL") # GDAL type

is.integer(elev)
is.float(elev)
is.double(elev)
is.factor(elev)

# convert data type
as.int(elev) # integer; note that "elev" is already of type "integer"
as.float(elev) # floating-precision
as.doub(elev) # double-precision

# assigning
pie <- elev
pie[] <- pi # assign all cells to the value of pi
pie

# concatenating multiple GRasters
rasts <- c(elev, forest)
rasts

# subsetting
rasts[[1]]
rasts[["madForest2000"]]

# replacing
rasts[[2]] <- 2 * forest
rasts

# adding layers
rasts[[3]] <- elev > 500 # add a layer
rasts <- c(rasts, sqrt(elev)) # add another
add(rasts) <- ln(elev)
rasts

# names
names(rasts)
names(rasts) <- c("elev_meters", "2_x_forest", "high_elevation", "sqrt_elev", "ln_elev")
rasts

# remove a layer
rasts[["2_x_forest"]] <- NULL
rasts

# number of layers
nlyr(rasts)

# correlation and covariance matrices
madLANDSAT <- fastData("madLANDSAT")
landsat <- fast(madLANDSAT) # projects matrix
layerCor(landsat) # correlation
layerCor(landsat, fun = 'cov') # covariance

}
```
