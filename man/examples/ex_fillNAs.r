if (grassStarted()) {

# Setup
library(terra)

# Elevation raster:
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

### Fill NAs:
biline <- interp <- fillNAs(elev)
bicube <- interp <- fillNAs(elev, method = "bicubic")
rst <- interp <- fillNAs(elev, method = "rst")

maps <- c(elev, biline, bicube, rst)
names(maps) <- c("original", "bilinear", "bicubic", "RST")
plot(maps)

### Constrain interpolated values to > 0
constrained <- fillNAs(elev, min = 0)

# Compare unconstrained and constrained:
minmax(biline)
minmax(constrained)

### Interpolate to only first 10 cells away from non-NA cells:
restrained <- fillNAs(elev, cells = 10)

maps <- c(elev, restrained)
names(maps) <- c("Original", "within 10 cells")
plot(maps)

}
