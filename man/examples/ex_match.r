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

# Example data: Elevation and land cover rasters
madElev <- fastData("madElev")
madCover <- fastData("madCover")

# Start GRASS session for examples only
faster(x = madCover, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

### match() with an integer raster:
###################################

elev <- fast(madElev)

# Cells in elevation raster replaced with index in which they appear
# in the table:
table <- c(1, 100, 200, 300, 400, 500)
elevIndex <- match(elev, table)
elevIndexNeg <- match(elev, table, nomatch = -1)

plot(c(elevIndex, elevIndexNeg))

### Using %in% and %notin% on an integer GRaster:
#################################################

elev <- fast(madElev)
table <- c(1, 100, 200, 300, 400, 500)

ins <- elev %in% table
notins <- elev %notin% table

plot(c(ins, notins))

### match() with a categorical raster:
######################################

cover <- fast(madCover)
cover <- droplevels(cover)
levels(cover)

forestLabels <- c(
   "Sparse broadleaved evergreen/semi-deciduous forest",
   "Broadleaved deciduous forest",
   "Grassland with mosaic forest",
   "Flooded forest"
)

forestClasses <- match(cover, forestLabels)
plot(forestClasses)

forestNoMatch <- match(cover, forestLabels, nomatch = -1)
plot(forestNoMatch)

### Using %in% and %notin% on a categorical GRaster:
####################################################

cover <- fast(madCover)
cover <- droplevels(cover)
levels(cover)

forestLabels <- c(
   "Sparse broadleaved evergreen/semi-deciduous forest",
   "Broadleaved deciduous forest",
   "Grassland with mosaic forest",
   "Flooded forest"
)

forest <- cover %in% forestLabels
plot(forest)

notForest <- cover %notin% forestLabels
plot(notForest)

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
