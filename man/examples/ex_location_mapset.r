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

# All available GRASS locations
.locations()

# Find location of an object among all active locations
.locationFind(elev)
.locationFind(chelsa1)
.locationFind(chelsa1, return = "index")
.locationFind(chelsa1, return = "crs")

# Switch between locations
.locationRestore(elev)
.locationRestore(chelsa1)

loc <- .location(elev)
.locationRestore(loc)

# We could use .locationDelete(elev) to delete
# the location where "elev" is stored.

# Mapsets are always "PERMANENT" in fasterRaster
.mapset()
.mapset(elev)
.mapset(chelsa1)

}
