if (grassStarted()) {

# Setup
library(terra)

# Example data
madChelsa <- fastData("madChelsa")

# Convert a SpatRaster to a GRaster:
chelsa <- fast(madChelsa)

# Correlation
layerCor(chelsa, "cor")

# Covariance
layerCor(chelsa, "cov")

}
