if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Convert non-NA to 1, NA cells remain NA
elevMask <- maskNA(elev)
elevMask
plot(c(elev, elevMask))

# Convert NA to 1, non-NA cells become NA
elevInvertMask <- maskNA(elev, invert = TRUE)
elevInvertMask
plot(c(elev, elevInvertMask))

# Convert NA to 200, non-NA cells keep their values
elevInvertRetain <- maskNA(elev, value = 200, invert = TRUE, retain = TRUE)
elevInvertRetain
plot(c(elev, elevInvertRetain))

}
