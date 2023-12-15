if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev") # raster
madForest <- fastData("madForest2000") # raster
madCoast <- fastData("madCoast4") # vector

# Convert to GRasters and GVectors
elev <- fast(madElev)
forest <- fast(madForest)
coast <- fast(madCoast)

ant <- coast[coast$NAME_4 == "Antanambe"] # subset

# Random cells in non-NA cells:
rand <- sampleRast(elev, 10000)
plot(rand)
nonnacell(rand)

# Use custom values for the mask:
randCustomMask <- sampleRast(elev, 10000, maskvalues = 1:20)
plot(randCustomMask)

# Force selected values to a custom value:
randCustomUpdate <- sampleRast(elev, 10000, updatevalue = 7)
plot(randCustomUpdate)

# Custom values for mask and set selected cells to custom value:
randAll <- sampleRast(elev, 10000, maskvalues = 1:20, updatevalue = 7)
plot(randAll)

}
