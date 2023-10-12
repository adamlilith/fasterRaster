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

# Example data: elevation raster and points vector
madElev <- fastData("madElev")
madCover <- fastData("madCover")
madCoast4 <- fastData("madCoast4")
madDypsis <- fastData("madDypsis")

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert to fasterRaster formats:
elev <- fast(madElev)
cover <- fast(madCover)
coast <- fast(madCoast4)
dypsis <- fast(madDypsis)

# Get values of elevation at points where Dypsis species are located:
extract(elev, dypsis, xy = TRUE)

# Extract from categorical raster
vals <- extract(cover, dypsis)
vals

cats <- extract(cover, dypsis, cats = TRUE)
cats

# Extract from a vector:
tab <- extract(coast, dypsis, xy = TRUE)
tab

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
