if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Find clumps based on exact values. This will appear as a gradient because
# most cells are assigned to a group of 1 cell.
exact <- clump(elev)
plot(exact)

# Clump based on approximate values:
approx <- clump(elev, minDiff = 0.0075)
plot(approx)

# Clump based on approximate values with minimum clump size
# (this can take a minute or two):
approx20 <- clump(elev, minDiff = 0.005, minClumpSize = 20)
plot(approx20)

approx
approx20

}
