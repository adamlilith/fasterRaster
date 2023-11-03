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

# Elevation
madElev <- fastData("madElev")

# Start GRASS session for examples only:
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert to GRaster:
elev <- fast(madElev)

# Thin elevation raster. In this case, we need to run 300 thinning iterations
# for the function to reduce the raster to linear features.
thinned <- thinLines(elev, iter = 300)
plot(thinned)

# Convert to lines:
rastToLines <- as.lines(thinned)
plot(rastToLines)

# We can clean this:
cleanLines <- cleanGeom(x = rastToLines, method = "removeDangles")
plot(rastToLines, col = "red")
plot(cleanLines, add = TRUE)

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
