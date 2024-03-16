if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)
elev

# Convert an sf vector to a GVector
rivers <- fast(madRivers)
rivers

}
