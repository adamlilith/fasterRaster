if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Calculate contour lines:
conts <- as.contour(elev, nlevels = 10)

plot(madElev)
plot(conts, add = TRUE)

}
