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

# Elevation raster
madElev <- fastData("madElev")

# Start GRASS session for examples only:
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Find clumps based on exact values. This will appear as a gradient because
# most cells are assigned to a group of 1 cell.
exact <- clump(elev)
plot(exact)

# Clump based on approximate values:
approx <- clump(elev, minDiff = 0.0075)
plot(approx)

# Clump based on approximate values with minimum clump size
# (this can take a minute or two):
approx20 <- clump(elev, minDiff = 0.005, minClumpSize = 20)
plot(approx20)

approx
approx20

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession("examples")

}
