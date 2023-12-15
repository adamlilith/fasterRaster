if (grassStarted()) {

# Setup
library(terra)

# Elevation
madElev <- fastData("madElev")

# Convert SpatRaster to GRaster:
elev <- fast(madElev)

# To speed things up, first group cells of similar value:
elevClumps <- clump(elev, minDiff = 0.0115)

# Convert to polygons:
rastToPolys <- as.polygons(elevClumps)
plot(rastToPolys)

}
