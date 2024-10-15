if (grassStarted()) {

# Setup
library(terra)

# Elevation and land cover raster
madElev <- fastData("madElev") # integer raster
madCover <- fastData("madCover") # categorical raster

# Convert to GRasters
elev <- fast(madElev)
cover <- fast(madCover)

# Subset elevation raster to just a few values to make example faster:
elevSubset <- elev[elev <= 3]
segregate(elevSubset)
segregate(elevSubset, keep = TRUE, other = -1)

# Segregate the factor raster
segregate(cover)

classes <- c("Grassland with mosaic forest", "Mosaic cropland/vegetation")
seg <- segregate(cover, classes = classes)
plot(seg)

}
