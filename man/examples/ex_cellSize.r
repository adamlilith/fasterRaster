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
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Start GRASS session for examples only:
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

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

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
