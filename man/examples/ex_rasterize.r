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
plot(points)

lines <- rasterize(rivers, elev)
plot(lines)

polys <- rasterize(coast4, elev)
plot(polys)

communes <- rasterize(coast4, elev, field = "NAME_4")
plot(communes)

# Change background value:
polysNeg1 <- rasterize(coast4, elev, background = -1)
plot(polysNeg1)

# Make one layer per river:
byRiver <- rasterize(rivers, elev, field = "NAM", by = "NAM")
plot(byRiver)

}
