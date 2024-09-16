if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Geomorphons:
geos <- geomorphons(elev)
geos
levels(geos) # levels
freq(geos) # frequencies

col <- c("gray90", "red", "orange", "blue", "green", "pink", "firebrick",
  "purple", "gray50", "black")
plot(geos, col = col)

}
