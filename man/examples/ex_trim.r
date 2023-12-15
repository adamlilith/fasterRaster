if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert SpatRaster to a GRaster:
elev <- fast(madElev)

# Trim NA rows/columns:
trimmedElev <- trim(elev)
dim(elev)
dim(trimmedElev)

# Trim a "stack" of rasters. We will artificially add NA rows and columns to
# one raster to demonstrate how the trim() function removes only rows/columns
# that are NA for *all* rasters.
elevNAs <- elev

fun <- " = if(col() > ncols() - 200, null(), madElev)"
elevNAs <- app(elevNAs, fun)

# Notice raster is "narrower" because we added NA columns
plot(elevNAs)

elevs <- c(elev, elevNAs)
trimmedElevs <- trim(elevs)
trimmedElevNAs <- trim(elevNAs)

dim(elevs)
dim(trimmedElevNAs)
dim(trimmedElevs)

}
