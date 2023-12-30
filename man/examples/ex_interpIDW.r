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

# Interpolate:
idw <- interpIDW(dypsisNoNA, elev, field = "year")

dypsisNoNA$year
plot(idw)
plot(dypsisNoNA, add = TRUE)

}
