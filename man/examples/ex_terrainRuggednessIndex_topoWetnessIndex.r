if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert to GRaster:
elev <- fast(madElev)

# Terrain ruggedness index:
tri <- terrainRuggednessIndex(elev)
plot(c(elev, tri))

# Topographic wetness index:
twi <- topoWetnessIndex(elev)
plot(c(elev, twi))

}
