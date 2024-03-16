if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Cell size, no masking, single layer
cs1 <- cellSize(elev)
plot(cs1)

# Cell size, with masking, single layer
cs2 <- cellSize(elev, mask = TRUE)
plot(cs2)

# Cell size, no masking, multilayer
elev2 <- c(elev, log(elev - 200))
cs3 <- cellSize(elev2)
plot(cs3)

# Cell size, masking by 1st layer, multilayer (ignores subsequent layers)
cs4 <- cellSize(elev2, mask = TRUE)
plot(cs4)

# Cell size, masking by each layer, multilayer
cs5 <- cellSize(elev2, mask = TRUE, lyrs = TRUE)
plot(cs5)

}
