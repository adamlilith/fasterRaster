if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev") # raster
madCover <- fastData("madCover") # categorical raster

# Convert to GRasters
elev <- fast(madElev) # integer raster
cover <- fast(madCover) # categorical raster

# Frequencies of integer raster values
f1 <- freq(elev)
print(f1) # have to do this sometimes if output is a data table

# Frequencies of categorical raster values
f2 <- freq(cover)
print(f2) # have to do this sometimes if output is a data table

# Frequencies of given values
f3 <- freq(elev, value = 4)
print(f3) # have to do this sometimes if output is a data table

# When a GRaster has non-integer values, they will be binned:
f4 <- freq(elev + 0.1, bins = 10)
print(f4)

# Calculate cross frequencies between rasters... both need to be integer.
elevWgs84 <- project(elev, cover)
elevClasses <- clump(elevWgs84, minDiff = 0.13) # bin elevations
names(elevClasses) <- 'elevClass'
f5 <- crossFreq(c(elevClasses, cover), na.rm = FALSE)
print(f5) # have to do this sometimes if output is a data table

}
