if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert to GRaster:
elev <- fast(madElev)

# Topographic wetness index:
twi <- wetness(elev)
names(twi) <- 'TWI'
plot(c(elev, twi))

# Terrain ruggedness index:
tri <- ruggedness(elev)
tri7 <- ruggedness(elev, size = 7)
triSmooth7 <- ruggedness(elev, size = 7, exponent = 4)

tris <- c(elev, tri, tri7, triSmooth7)
names(tris) <- c("elevation", "TRI in 3x3", "TRI in 7x7", "Smoothed TRIin 7x7")
plot(tris)

}
