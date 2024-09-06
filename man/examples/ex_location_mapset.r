if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madChelsa <- fastData("madChelsa")
madChelsa1 <- madChelsa[[1]]

# Convert SpatRasters to GRasters.
# Each raster has a different CRS so will be put into a different location.
elev <- fast(madElev)
chelsa1 <- fast(madChelsa1)

# Name of the currently active location
.location()
.location(elev)
.location(chelsa1)

# Mapsets are always "PERMANENT" in fasterRaster
.mapset()
.mapset(elev)
.mapset(chelsa1)

# Meta-data on all available GRASS Locations
.locations()

}
