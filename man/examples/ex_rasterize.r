\dontrun{
# NB This example is in a "dontrun{}" block because it requires users to have
# GRASS GIS Version 8+ installed on their system.

# IMPORTANT #1: If you already have a GRASS session started, you will need to
# run the line below and the last line in this example to work with it again.
# If you have not started a GRASS session, you can skip this step and go to
# step #2.
opts. <- getFastOptions()

# IMPORTANT #2: Select the appropriate line below and change as necessary to
# where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.3.app/Contents/Resources" # Mac
grassDir <- "C:/Program Files/GRASS GIS 8.3" # Windows
grassDir <- "/usr/local/grass" # Linux

# Setup
library(sf)
library(terra)

# Elevation raster, outline of a part of Madagascar, and rivers vector:
madElev <- fastData("madElev") # raster
madDypsis <- fastData("madDypsis") # points vector
madRivers <- fastData("madRivers") # lines vector
madCoast4 <- fastData("madCoast4") # polygons vector

# Start GRASS session for examples only:
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

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

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
