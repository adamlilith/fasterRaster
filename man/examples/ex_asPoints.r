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

# setup
library(sf)
library(terra)

# elevation, outline of a portion of eastern Madagascar, and rivers vector
madElev <- fastData("madElev")
madCoast0 <- fastData("madCoast0")
madRivers <- fastData("madRivers")

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert to GVector:
elev <- fast(madElev)
coast <- fast(madCoast0)
rivers <- fast(madRivers)

# Extract points from raster. To make this fast, we will first crop the raster
# to a small extent defined by a single river.
river <- rivers[1]
elevCrop <- crop(elev, river)
elevPoints <- as.points(elevCrop)
elevPoints

plot(elevCrop)
plot(elevPoints, pch = '.', add = TRUE)

# Extract points from vectors:
coastPoints <- as.points(coast)
riversPoints <- as.points(rivers)

plot(coast)
plot(coastPoints, add = TRUE)

plot(rivers, col = "blue", add = TRUE)
plot(riversPoints, col = "blue", add = TRUE)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession("examples")

}
