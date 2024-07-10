if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Create spatially exclusive tiles:
exclusive <- tiles(elev, n = 2, verbose = TRUE)

startpar <- par(mfrow = c(2, 3))
plot(elev, main = "Original")

for (i in seq_along(exclusive)) {
	plot(exclusive[[i]], ext = elev, main = paste("Tile", i))
}
par(startpar)

# Create tiles that overlap:
overlaps <- tiles(elev, n = 2, overlap = 200, verbose = TRUE)

startpar <- par(mfrow = c(2, 3))
plot(elev, main = "Original")

for (i in seq_along(overlaps)) {
	plot(overlaps[[i]], ext = elev, main = paste("Tile", i))
}
par(startpar)

}
