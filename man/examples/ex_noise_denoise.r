if (grassStarted()) {

# Setup
library(terra)

# Climate raster:
madChelsa <- fastData("madChelsa")

# Convert a SpatRaster to a GRaster:
chelsa <- fast(madChelsa)

# Generate raster with layers representing principal component predictions:
pcRast <- princomp(chelsa, scale = TRUE)
plot(pcRast)

# Get information on the PCA:
prinComp <- pcs(pcRast)

prinComp
summary(prinComp)
plot(prinComp)

}
