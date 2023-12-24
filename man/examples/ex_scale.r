if (grassStarted()) {

# Setup
library(terra)

# Climate rasters:
madChelsa <- fastData("madChelsa")

# Convert to GRasters:
chelsa <- fast(madChelsa)

# Scale rasters:
chScaled <- scale(chelsa)
chScaled

# Get original means and sd's:
attributes(chScaled)$center
attributes(chScaled)$scale

# Means and SD are now ~ 0 and 1, respectively:
global(chScaled, c("mean", "sd"))

}
