if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert to GRaster:
elev <- fast(madElev)

# Sine waves:
waves <- sineRast(elev, ns = 2, ew = 1)
plot(waves)

# Sine waves with and without north-south offset:
noOffsets <- sineRast(elev, ns = 1, ew = 1)
offsets <- sineRast(elev, ns = 1, ew = 1, nsOffset = 0.25)
offs <- c(noOffsets, offsets)
names(offs) <- c("no offset", "offset")
plot(offs)

# Masking:
madCoast4 <- fastData("madCoast4")
coast4 <- fast(madCoast4, verbose = FALSE)

masked <- sineRast(elev, mask = coast4)
plot(masked)

}
