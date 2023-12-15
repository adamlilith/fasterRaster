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

}
