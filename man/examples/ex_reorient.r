### Re-orient numeric values:
facings <- c(0, 90, 180, 270, 360)
reorient(facings)

if (grassStarted()) {

### Re-orient a GRaster:

# Setup
library(terra)
madElev <- fastData("madElev")
elev <- fast(madElev)

# Calculate aspect in degrees, using north orientation:
aspect <- terrain(elev, "aspect")

# Remove aspects < 0 (used to denote aspect of flat areas):
aspect[aspect < 0] <- NA

# Re-orient:
aspectEast <- reorient(aspect)

# Plot:
aspects <- c(aspect, aspectEast)
names(aspects) <- c("north_orientation", "east_orientation")
plot(aspects)


}
