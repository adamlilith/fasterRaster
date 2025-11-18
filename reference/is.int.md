# Data type of a raster

In **fasterRaster**, rasters can have three data types: "factor"
(categorical rasters), "integer" (integers), "float" (floating point
values, accurate to the 6th to 9th decimal places), and "double"
(double-precision values, accurate to the 15th to 17th decimal places).
The type of raster can be checked with:

- `is.factor()`: The raster will have integer values and categories
  matched to the integers (see levels()).

- `is.int()`: Are values integers? Note that `is.int()` will return
  `FALSE` for categorical rasters, even though cell values are
  technically integers.

- `is.cell()`: Are values integers (`TRUE` for `integer` and categorical
  rasters).

- `is.float()`: Are values floating-point precision?

- `is.doub()`: Are values double-floating point precision?

## Usage

``` r
# S4 method for class 'GRaster'
is.int(x)

# S4 method for class 'GRaster'
is.cell(x)

# S4 method for class 'GRaster'
is.float(x)

# S4 method for class 'GRaster'
is.doub(x)

# S4 method for class 'GRaster'
is.factor(x)
```

## Arguments

- x:

  A `GRaster`.

## Value

Logical.

## See also

[`datatype()`](https://github.com/adamlilith/fasterRaster/reference/datatype.md),
[`terra::datatype()`](https://rspatial.github.io/terra/reference/datatype.html),
[`as.int()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md),
[`as.float()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md),
[`as.doub()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md),
`is.factor()`,
[`vignette("GRasters", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/GRasters.md)

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
