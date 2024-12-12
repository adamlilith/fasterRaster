if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev") # raster

# Convert to GRasters and GVectors
elev <- fast(madElev)

### spatSample()

# Random points as data.frame or data.table:
randVals <- spatSample(elev, size = 20, values = TRUE)
randVals

# Random points as a points GVector:
randPoints <- spatSample(elev, size = 20, as.points = TRUE)
randPoints
plot(elev)
plot(randPoints, add = TRUE)

# Random points in a select area:
madCoast <- fastData("madCoast4") # vector
coast <- fast(madCoast)
ant <- coast[coast$NAME_4 == "Antanambe"] # subset

restrictedPoints <- spatSample(elev, size = 20, as.points = TRUE,
   strata = ant)

plot(elev)
plot(ant, add = TRUE)
plot(restrictedPoints, add = TRUE) # note 20 points for entire geometry

# Random points, one set per subgeometry:
stratifiedPoints <- spatSample(elev, size = 20, as.points = TRUE,
   strata = ant, byStratum = TRUE)

plot(elev)
plot(ant, add = TRUE)
plot(stratifiedPoints, pch = 21, bg = "red", add = TRUE) # note 20 points per subgeometry

# Random categories:
madCover <- fastData("madCover") # raster
cover <- fast(madCover)

randCover <- spatSample(cover, size = 20, values = TRUE,
     cat = TRUE, xy = TRUE)
randCover

### sampleRast()

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
