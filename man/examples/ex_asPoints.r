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

# For speed, we will first crop the raster to a small extent:
elevCrop <- crop(elev, c(2530000, 2532000, -1722000, -1720000))
elevPoints <- as.points(elevCrop)
elevPoints

plot(elevCrop)
plot(elevPoints, pch = 4, cex = 0.7, add = TRUE)

# Extract points from vectors:
coastPoints <- as.points(coast)
riversPoints <- as.points(rivers)

plot(coast)
plot(coastPoints, pch = 4, cex = 0.7, add = TRUE)

plot(rivers, col = "blue", add = TRUE)
plot(riversPoints, col = "blue", pch = 1, cex = 0.7, add = TRUE)

}
