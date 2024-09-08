
### Re-orient numeric values:
facings <- c(0, 90, 180, 270, 360)
reorient(facings)

# Re-reorienting returns the same values:
reorient(reorient(facings))

if (grassStarted()) {

### Re-orient a GRaster:

# Setup
library(terra)
madElev <- fastData("madElev")
elev <- fast(madElev)

# Calculate aspect in degrees, using north orientation:
aspectNorth <- terrain(elev, "aspect")

# Re-orient to east-facing:
aspectEast <- reorient(aspectNorth)

# Re-reorienting is the same, to within rounding error:
aspectNorth - reorient(reorient(aspectNorth))

# Plot:
aspects <- c(aspectNorth, aspectEast)
names(aspects) <- c("north_orientation", "east_orientation")
plot(aspects)


}
