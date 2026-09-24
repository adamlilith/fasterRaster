if (grassStarted()) {

# Setup
library(terra)

### Mask layer-by-layer

# Elevation raster
madElev <- fastData("madElev")
madForest2000 <- fastData("madForest2000")

# Convert SpatRasters to GRasters:
elev <- fast(madElev)
forest2000 <- fast(madForest2000)

# Make a random layer and stack it with elevation:
x <- c(elev, forest2000)

# Put NAs in cells where any layer has an NA:
masked <- na.omit(x, verbose = TRUE)
plot(masked)

}
