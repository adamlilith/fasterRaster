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
grassDir <- 'C:/Program Files/GRASS GIS 8.3' # Windows
grassDir <- '/usr/local/grass' # Linux

# setup
library(sf)
library(terra)

# example data
madElev <- fastData('madElev') # raster
madForest <- fastData('madForest2000') # raster
madCoast <- fastData('madCoast4') # vector
madAnt <- madCoast[madCoast$NAME_4 == 'Antanambe', ]

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = 'examples') # line only needed for examples

# convert to GRasters and GVectors
elev <- fast(madElev)
forest <- fast(madForest)
ant <- fast(madAnt)

# Mask by a raster or  vector:
byRast <- mask(elev, forest)
plot(byRast)

byVect <- mask(elev, ant)
plot(byVect)

# Mask by a raster or vector, but invert mask:
byRastInvert <- mask(elev, forest, inverse = TRUE)
plot(byRastInvert)

byVectInvert <- mask(elev, ant, inverse = TRUE)
plot(byVectInvert)

# Mask by a raster, but use custom values for the mask:
byRastCustomMask <- mask(elev, elev, maskvalues = 1:20)
plot(byRastCustomMask)

# Mask by a raster or vector, but force masked values to a custom value:
byRastCustomUpdate <- mask(elev, forest, updatevalue = 7)
plot(byRastCustomUpdate)

byVectCustomUpdate <- mask(elev, ant, updatevalue = 7)
plot(byVectCustomUpdate)

# Mask by a raster, inverse, custom values, and custom update:
byVectAll <-
   mask(elev, forest, inverse = TRUE, maskvalues = 1:20, updatevalue = 7)
plot(byVectAll)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
