if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster, outline of a part of Madagascar, and rivers vector:
madElev <- fastData("madElev")
madCoast0 <- fastData("madCoast0")
madRivers <- fastData("madRivers")

# Convert to GRaster and GVectors:
elev <- fast(madElev)
coast <- fast(madCoast0)
rivers <- fast(madRivers)

# For this example, we will first crop to a small extent.
river <- rivers[1]
elevCrop <- crop(elev, river)
elevPoints <- as.points(elevCrop)
elevPoints

plot(elevCrop)
plot(elevPoints, pch = '.', add = TRUE)

# Extract points from vectors:
coastPoints <- as.points(coast)
riversPoints <- as.points(rivers)

plot(coast)
plot(coastPoints, add = TRUE)

plot(rivers, col = "blue", add = TRUE)
plot(riversPoints, col = "blue", add = TRUE)

}
