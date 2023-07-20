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

# Random cells in non-NA cells:
rand <- spatSample(elev, 10000)
plot(rand)
nonnacell(rand)

# Use custom values for the mask:
randCustomMask <- spatSample(elev, 10000, maskvalues = 1:20)
plot(randCustomMask)

# Force selected values to a custom value:
randCustomUpdate <- spatSample(elev, 10000, updatevalue = 7)
plot(randCustomUpdate)

# Custom values for mask and set selected cells to custom value:
randAll <- spatSample(elev, 10000, maskvalues = 1:20, updatevalue = 7)
plot(randAll)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
