if (grassStarted()) {

# Setup
library(terra)

# Climate rasters:
madChelsa <- fastData("madChelsa")

# Convert to GRasters:
chelsa <- fast(madChelsa)

### Scale rasters:
chScaled <- scale(chelsa)
chScaled

# Means are very close to 0 and SDs to 1:
global(chScaled, c("mean", "sd"))

# Get original means and sd's:
centers <- attributes(chScaled)$center
scales <- attributes(chScaled)$scale
centers
scales

### Unscale rasters:
chUnscaled <- unscale(chScaled, center = centers, scale = scales)

# Means and SD are returned to original values:
global(chUnscaled, c("mean", "sd"))

}
