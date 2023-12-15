if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster, plant specimen collections, rivers vector,
# outline of area vector
madElev <- fastData("madElev")
madDypsis <- fastData("madDypsis")

# Convert to fasterRaster format:
elev <- fast(madElev)
dypsis <- fast(madDypsis)

# Kernel density estimation:
kde <- kernel(dypsis, elev)
plot(kde)
plot(dypsis, add = TRUE, pch = 1)

}
