# Convert a GRaster to a SpatRaster

The **fasterRaster** version of the `rast()` function converts a
`GRaster` to a `SpatRaster` (from the **terra** package).

## Usage

``` r
# S4 method for class 'GRaster'
rast(x, mm = FALSE, ...)
```

## Arguments

- x:

  A `GRaster`.

- mm:

  Logical: If `TRUE`, call
  [`terra::setMinMax()`](https://rspatial.github.io/terra/reference/minmax.html)
  on the raster to ensure it has metadata on the minimum and maximum
  values. For large rasters, this can take a long time, so the default
  value of `mm` is `FALSE`.

- ...:

  Additional arguments to send to
  [`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md).
  These are typically unneeded, though `bigTiff` may be of use if the
  raster is large, and supplying `datatype` can speed conversion of
  large rasters. See
  [`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md).

## Value

A `SpatRaster` (**terra** package).

## See also

[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)

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
