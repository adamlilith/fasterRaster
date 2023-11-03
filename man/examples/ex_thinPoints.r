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

# Elevation and points
madElev <- fastData("madElev")
madDypsis <- fastData("madDypsis")

# Start GRASS session for examples only:
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert to fasterRaster formats:
elev <- fast(madElev)
dypsis <- fast(madDypsis)

# Aggregate cells of the raster so they are bigger (otherwise, in this
# example, we'd have no cells with > 1 point):
elevAgg <- aggregate(elev, 32)

# Remove all but one or two points per cell
thin1 <- thinPoints(dypsis, elevAgg, n = 1)
thin2 <- thinPoints(dypsis, elevAgg, n = 2)

# Plot
plot(elevAgg)
plot(dypsis, add = TRUE)
plot(thin2, col = "yellow", add = TRUE)
plot(thin1, col = "red", add = TRUE)
legend(
   "bottomright",
   legend = c("In original & thin 1 & 2",
     "In just thin 1 & 2", "In just thin 1"),
   pch = 16,
   col = c("black", "yellow", "red"),
   bg = "white",
   xpd = NA
)

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
