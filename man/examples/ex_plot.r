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

# Example data
madElev <- fastData("madElev") # elevation raster
madLANDSAT <- fastData("madLANDSAT") # multi-layer raster
madRivers <- fastData("madRivers") # lines vector

# Start GRASS session for examples only:
faster(x = madLANDSAT, grassDir = grassDir,
workDir = tempdir(), location = "examples", overwrite = TRUE)

# Convert SpatRaster to GRaster and SpatVector to GVector
elev <- fast(madElev)
rivers <- fast(madRivers)
land <- fast(madLANDSAT, checkCRS = FALSE) # NB Need checkCRS = FALSE for this
# raster... We know it has the same CRS as the location, so this is OK, but
# checkCRS = TRUE (default behavior) does not work.

# Plot:
plot(elev)
plot(rivers, add = TRUE)

# Histograms:
hist(elev)
hist(land)

# Plot surface reflectance in RGB:
plotRGB(land, 3, 2, 1) # "natural" color
plotRGB(land, 4, 1, 2, stretch = "lin") # emphasize near-infrared (vegetation)

# Make composite map from RGB layers and plot in grayscale:
comp <- compositeRGB(r = land[[3]], g = land[[2]], b = land[[1]])
grays <- paste0("gray", 0:100)
plot(comp, col = grays)

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
