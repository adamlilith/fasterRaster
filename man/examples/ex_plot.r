if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev") # elevation raster
madLANDSAT <- fastData("madLANDSAT") # multi-layer raster
madRivers <- fastData("madRivers") # lines vector

# Convert SpatRaster to GRaster and SpatVector to GVector
elev <- fast(madElev)
rivers <- fast(madRivers)
land <- fast(madLANDSAT, checkCRS = FALSE) # NB Need checkCRS = FALSE for this
# raster... We know it has the same CRS as the location, so this is OK, but
# checkCRS = TRUE (default behavior) does not work.

# Plot:
plot(elev)
plot(rivers, add = TRUE)

# Histograms:
hist(elev)
hist(land)

# Plot surface reflectance in RGB:
plotRGB(land, 3, 2, 1) # "natural" color
plotRGB(land, 4, 1, 2, stretch = "lin") # emphasize near-infrared (vegetation)

# Make composite map from RGB layers and plot in grayscale:
comp <- compositeRGB(r = land[[3]], g = land[[2]], b = land[[1]])
grays <- paste0("gray", 0:100)
plot(comp, col = grays)

}
