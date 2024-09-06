if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)

# Calculate all topographic metrics
topos <- terrain(elev, v = "*")
topos

plot(topos)

# Calculate a hillshade raster
hs <- hillshade(elev)
plot(hs)

}
