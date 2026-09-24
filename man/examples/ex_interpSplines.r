if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster and points at which Dypsis have been collected:
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster and sf to GVector:
elev <- fast(madElev)

# Random points:
rands <- spatSample(elev, 30, as.points = TRUE, seed = 1)
rands <- rands[complete.cases(rands)] # remove NAs

# For expediency, we will also increase the size of spline steps in the
# x- and y-dimensions:
xlength <- 10 * xres(elev)
ylength <- 10 * yres(elev)

# Interpolate to a raster (need to increase size of spline length first):
interpElev <- interpSplines(rands, y = elev, field = "madElev",
lambda = 0.01, xlength = xlength, ylength = ylength)

# Plot:
oldpar <- par(mfrow = c(1, 2))
plot(elev, main = "Observed")
plot(rands, pch = 1, add = TRUE)

elevRange <- c(minmax(elev))
plot(interpElev, main = "Interpolated", range = elevRange)
plot(rands, pch = 1, add = TRUE)

par(oldpar)

### Find optimal lambda using cross-validation (takes a while):
lambdas <- interpSplines(rands, y = elev, field = "madElev",
xlength = xlength, ylength = ylength, interpolate = FALSE)

lambdas

}
