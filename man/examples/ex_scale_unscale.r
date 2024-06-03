if (grassStarted()) {

# Setup
library(terra)

# Climate rasters:
madChelsa <- fastData("madChelsa")

# Convert to GRasters:
chelsa <- fast(madChelsa)

### Center and scale rasters
# Scale with using sample SD:
chScaled <- scale(chelsa)
chScaled

# Scale with using sample SD:
chScaledPop <- scalepop(chelsa)
chScaledPop

# Means are very close to 0 and SDs to 1:
global(chScaled, c("mean", "sd", "min", "max"))
global(chScaledPop, c("mean", "sd", "min", "max"))

# Get original means and sd's:
centers <- attributes(chScaled)$center
scales <- attributes(chScaled)$scale
centers
scales

### Unscale rasters:
chUnscaled <- unscale(chScaled, center = centers, scale = scales)

# Means and SD are returned to original values:
global(chUnscaled, c("mean", "sd", "min", "max")) # unscaled
global(chelsa, c("mean", "sd", "min", "max")) # original

}
