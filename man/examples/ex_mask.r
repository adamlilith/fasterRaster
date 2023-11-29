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

# example data
madElev <- fastData("madElev") # raster
madForest <- fastData("madForest2000") # raster
madCoast <- fastData("madCoast4") # vector

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# convert to GRasters and GVectors
elev <- fast(madElev)
forest <- fast(madForest)
coast <- fast(madCoast)

ant <- coast[coast$NAME_4 == "Antanambe"]

# Mask by a raster or  vector:
maskByRast <- mask(elev, forest)
plot(c(forest, maskByRast))

maskByVect <- mask(elev, ant)
plot(maskByVect)
plot(ant, add = TRUE)

# Mask by a raster or vector, but invert mask:
maskByRastInvert <- mask(elev, forest, inverse = TRUE)
plot(c(forest, maskByRastInvert))

maskByVectInvert <- mask(elev, ant, inverse = TRUE)
plot(maskByVectInvert)
plot(ant, add = TRUE)

# Mask by a raster, but use custom values for the mask:
maskByRastCustomMask <- mask(elev, elev, maskvalues = 1:20)
plot(c(elev <= 20, maskByRastCustomMask))

# Mask by a raster or vector, but force masked values to a custom value:
byRastCustomUpdate <- mask(elev, forest, updatevalue = 7)
plot(byRastCustomUpdate)

byVectCustomUpdate <- mask(elev, ant, updatevalue = 7)
plot(byVectCustomUpdate)

# Mask by a raster, inverse, custom values, and custom update:
byRastAll <-
   mask(elev, elev, inverse = TRUE, maskvalues = 1:20, updatevalue = 7)

plot(c(elev, byRastAll))

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
