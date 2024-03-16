if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev") # raster
madCover <- fastData("madCover") # raster
madCoast <- fastData("madCoast4") # vector
madRivers <- fastData("madRivers") # vector

# Convert to GRasters and GVectors
elev <- fast(madElev)
cover <- fast(madCover, warn = FALSE)
coast <- fast(madCoast)
rivers <- fast(madRivers)

# Random points:
randVals <- spatSample(elev, size = 20, values = TRUE)
randVals

randPoints <- spatSample(elev, size = 20, as.points = TRUE)
randPoints
plot(elev)
plot(randPoints, add = TRUE)

# Random categories
randCover <- spatSample(cover, size = 20, values = TRUE,
     cat = TRUE, xy = TRUE)
randCover

}
