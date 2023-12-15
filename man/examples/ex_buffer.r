if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster, rivers vector
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")

# Convert a SpatRaster to a GRaster, and sf to a GVector
elev <- fast(madElev)
rivers <- fast(madRivers)

### Buffer a raster by a given distance:
buffByDist <- buffer(elev, width = 2000) # 2000-m buffer
plot(buffByDist, legend = FALSE)
plot(madElev, add = TRUE)

### Buffer a raster by a given number of cells:
buffByCells <- buffer(elev, width = 20.01, unit = "cells") # 20-cell buffer
plot(buffByCells)
plot(madElev, add = TRUE)

### Buffer a vector:
buffRivers <- buffer(rivers, width = 2000) # 2000-m buffer
plot(buffRivers)
plot(st_geometry(madRivers), col = "blue", add = TRUE)

}
