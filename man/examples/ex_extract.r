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
madElev <- fastData("madElev") # raster
madCover <- fastData("madCover") # categorical raster
madDypsis <- fastData("madDypsis") # points vector
madRivers <- fastData("madRivers") # lines vector
madCoast4 <- fastData("madCoast4") # polygons vector

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert to fasterRaster formats:
elev <- fast(madElev) # raster
cover <- fast(madCover, method = "near", warn = FALSE) # categorical raster
dypsis <- fast(madDypsis) # points vector
rivers <- fast(madRivers) # lines vector
coast <- fast(madCoast4) # polygons vector

# Get values of elevation at points where Dypsis species are located:
extract(elev, dypsis, xy = TRUE)

# Extract from categorical raster at points:
extract(cover, dypsis)
extract(cover, dypsis, cats = TRUE)

# Extract and summarize values on a raster across polygons:
extract(elev, coast, fun = c("sum", "mean", "countNonNA"), overlap = FALSE)

# Extract and summarize values on a raster across lines:
extract(elev, rivers, fun = c("sum", "mean", "countNonNA"), overlap = FALSE)

# Extract from a polygons vector at a points vector:
table <- extract(coast, dypsis, xy = TRUE)
head(table) # first 3 are outside polygons vector, next 3 are inside

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
