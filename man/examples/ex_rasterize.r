if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster, outline of a part of Madagascar, and rivers vector:
madElev <- fastData("madElev") # raster
madDypsis <- fastData("madDypsis") # points vector
madRivers <- fastData("madRivers") # lines vector
madCoast4 <- fastData("madCoast4") # polygons vector

# Convert to GRaster and GVectors:
elev <- fast(madElev)
dypsis <- fast(madDypsis)
coast <- fast(madCoast4)
rivers <- fast(madRivers)

# Convert points, line, and polygons vectors to rasters:
points <- rasterize(dypsis, elev)
lines <- rasterize(rivers, elev)
polys <- rasterize(coast4, elev)

plot(points)
plot(lines)
plot(polys)

# Change background value:
polys <- rasterize(coast4, elev, background = -1)
plot(polys)

# By geometry onto one rasters:
polys <- rasterize(coast4, elev, byGeom = TRUE, collapse = TRUE)
plot(polys)

# By geometry, separate rasters:
polys <- rasterize(coast4, elev, byGeom = TRUE, collapse = FALSE)
plot(polys)

}
