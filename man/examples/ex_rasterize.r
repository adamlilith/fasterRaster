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
coast4 <- fast(madCoast4)
rivers <- fast(madRivers)

# Convert points, line, and polygons vectors to rasters:
points <- rasterize(dypsis, elev)
lines <- rasterize(rivers, elev)
polys <- rasterize(coast4, elev)
communes <- rasterize(coast4, elev, field = "NAME_4")

plot(points)
plot(lines)
plot(polys)
plot(communes)

# Make one layer per river:
byRiver <- rasterize(rivers, elev, field = "NAM", by = "NAM")
plot(byRiver)

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
