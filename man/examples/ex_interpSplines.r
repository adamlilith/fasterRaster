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

# We will interpolate the value of collection year in the "year" field of dypsis. But first, we need to remove the NA value.
dypsisNoNA <- dypsis[!is.na(dypsis$year)]

# For this example, we also need to increase the size of spline steps in the
# x- and y-dimensions:
xlength <- 10 * xres(elev)
ylength <- 10 * yres(elev)

# Interpolate to a raster (need to increase size of spline length first):
interpRast <- interpSplines(dypsisNoNA, y = elev, field = "year",
lambda = 0.01, xlength = xlength, ylength = ylength)

plot(interpRast)
plot(dypsisNoNA, add = TRUE)
dypsisNoNA$year

# Find optimal lambda using cross-validation (takes a while):
lambdas <- interpSplines(dypsisNoNA, y = elev, field = "year",
xlength = xlength, ylength = ylength, interpolate = FALSE)

lambdas

}
