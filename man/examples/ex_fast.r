if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev") # integer raster
madCover <- fastData("madCover") # categorical raster
madCoast4 <- fastData("madCoast4") # polygons vector
madRivers <- fastData("madRivers") # lines vector
madDypsis <- fastData("madDypsis") # points vector

### Create GRasters from SpatRasters
####################################

# Create an integer raster
class(madElev)
elev <- fast(madElev)
elev

# Create a categorical raster
class(madCover)
cover <- fast(madCover)
madCover
levels(madCover) # category levels

# Create a GRaster from a file on disk
rastFile <- system.file("extdata", "madForest2000.tif", package = "fasterRaster")
forest2000 <- fast(rastFile)
forest2000

### Create GVectors
###################

# Create a GVector from an sf vector
class(madCoast4)
coast4 <- fast(madCoast4)
coast4

# Create a GVector from a SpatVector
madRivers <- vect(madRivers)
class(madRivers)
rivers <- fast(madRivers)
rivers

# Create a GVector from a vector on disk
vectFile <- system.file("extdata/shapes", "madCoast.shp",
   package = "fasterRaster")
coast0 <- fast(vectFile)
coast0

# Import only Dypsis occurrences in a restricted area
ant <- coast4[coast4$NAME_4 == "Antanambe"]
dypsisRestrict <- fast(madDypsis, extent = ant)
dypsis <- fast(madDypsis)

plot(coast4)
plot(ant, col = "gray80", add = TRUE)
plot(dypsis, add = TRUE)
plot(dypsisRestrict, col = "red", add = TRUE)

}
