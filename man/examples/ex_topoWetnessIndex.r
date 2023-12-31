if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert to GRaster:
elev <- fast(madElev)

# Topographic wetness index:
tpi <- topoWetnessIndex(elev)

plot(c(elev, tpi))

}
