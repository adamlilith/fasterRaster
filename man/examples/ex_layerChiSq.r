if (grassStarted()) {

# Setup
library(terra)

# Example data
madChelsa <- fastData("madChelsa")

# Convert SpatRaster to GRaster:
chelsa <- fast(madChelsa)

# Force the values to be integers (required by layerChiSq()):
chelsaInteger <- as.int(chelsa)

# Chi-squared test
layerChiSq(chelsaInteger, verbose = TRUE)

}
