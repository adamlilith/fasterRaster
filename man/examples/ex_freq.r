if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev") # raster
madCover <- fastData("madCover") # categorical raster

# Convert to GRasters
elev <- fast(madElev) # raster
cover <- fast(madCover) # categorical raster

# Frequencies of integer raster values
f <- freq(elev)
print(f) # have to do this sometimes if output is a data table

# Frequencies of categorical raster values
f <- freq(cover)
print(f) # have to do this sometimes if output is a data table

# Frequencies of given values
f <- freq(elev, value = 1)
print(f) # have to do this sometimes if output is a data table

# When a GRaster has non-integer values, they will be binned:
f <- freq(elev + 0.1, bins = 10)
print(f)

}
