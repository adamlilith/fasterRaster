if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster and points at which Dypsis have been collected:
madElev <- fastData("madElev")
madDypsis <- fastData("madDypsis")

# Convert a SpatRaster to a GRaster and sf to GVector:
elev <- fast(madElev)
dypsis <- fast(madDypsis)

# We have an elevation raster but will pretend we don't and interpolate
# the value of elevation associated with each point.

# Extract elevation values from elevation raster:
dypsisElev <- extract(elev, dypsis)

# Add column to dypsis:
dypsis <- colbind(dypsis, dypsisElev)

# For expediency, we will also increase the size of spline steps in the
# x- and y-dimensions:
xlength <- 10 * xres(elev)
ylength <- 10 * yres(elev)

# Interpolate to a raster (need to increase size of spline length first):
interpElev <- interpSplines(dypsis, y = elev, field = "madElev",
lambda = 0.01, xlength = xlength, ylength = ylength)

# Plot:
oldpar <- par(mfrow = c(1, 2))
plot(elev, main = "Observed")
plot(dypsis, pch = 1, add = TRUE)

plot(interpElev, main = "Interpolated")
plot(dypsis, pch = 1, add = TRUE)

par(oldpar)

### Find optimal lambda using cross-validation (takes a while):
lambdas <- interpSplines(dypsis, y = elev, field = "madElev",
xlength = xlength, ylength = ylength, interpolate = FALSE)

lambdas

}
