if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev")
madCoast4 <- fastData("madCoast4")
madCoast4 <- vect(madCoast4)

# For the example, crop the elevation raster to two communes
madAnt <- madCoast4[madCoast4$NAME_4 == "Antanambe", ]
madMan <- madCoast4[madCoast4$NAME_4 == "Manompana", ]

elevAnt <- crop(madElev, madAnt)
elevMan <- crop(madElev, madMan)

plot(madElev)
plot(elevAnt, col = "red", legend = FALSE, add = TRUE)
plot(elevMan, col = "blue", legend = FALSE, add = TRUE)

# Convert a SpatRaster to a GRaster
ant <- fast(elevAnt)
man <- fast(elevMan)

# merge
antMan <- merge(ant, man)
plot(antMan, main = "Antman!")

}
