if (grassStarted()) {

# Setup
library(terra)

# Elevation
madElev <- fastData("madElev")

# Convert to GRaster:
elev <- fast(madElev)

# Thin elevation raster. In this case, we need to run 300 thinning iterations
# for the function to reduce the raster to linear features.
thinned <- thinLines(elev, iter = 300)
plot(thinned)

# Convert to lines:
rastToLines <- as.lines(thinned)
plot(rastToLines)

# We can clean this:
cleanLines <- cleanGeom(x = rastToLines, method = "removeDangles")
plot(rastToLines, col = "red")
plot(cleanLines, add = TRUE)

}
