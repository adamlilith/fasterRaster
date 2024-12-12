if (grassStarted()) {

# Setup
library(terra)

# Elevation
madElev <- fastData("madElev")

# Convert to GRaster:
elev <- fast(madElev)

# Thin elevation raster:
thinned <- thinLines(elev, iter = 300)
plot(thinned)

# Convert to lines:
rastToLines <- as.lines(thinned)
plot(rastToLines)

# We can clean this:
cleanLines <- fixDangles(x = rastToLines)
plot(rastToLines, col = "red")
plot(cleanLines, add = TRUE)

}
