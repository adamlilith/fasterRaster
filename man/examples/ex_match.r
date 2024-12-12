if (grassStarted()) {

# Setup
library(terra)

# Example data: Elevation and land cover rasters
madElev <- fastData("madElev")
madCover <- fastData("madCover")

### match() with an integer raster:

elev <- fast(madElev)

# Cells in elevation raster replaced with index in which they appear
# in the table:
table <- c(10, 20, 30, 40, 50)
elevIndex <- match(elev, table)
elevIndexNeg <- match(elev, table, nomatch = -100)

plot(c(elevIndex, elevIndexNeg))

### Using %in% and %notin% on an integer GRaster:

elev <- fast(madElev)
table <- c(10, 20, 30, 40, 50)

ins <- elev %in% table
notins <- elev %notin% table

plot(c(ins, notins))

### match() with a categorical raster:

cover <- fast(madCover)
cover <- droplevels(cover)
levels(cover)

forestLabels <- c(
   "Sparse broadleaved evergreen/semi-deciduous forest",
   "Broadleaved deciduous forest",
   "Grassland with mosaic forest",
   "Flooded forest"
)

forestClasses <- match(cover, forestLabels)
plot(forestClasses)
levels(forestClasses)

forestNoMatch <- match(cover, forestLabels, nomatch = -100)
plot(forestNoMatch)
levels(forestNoMatch)

### Using %in% and %notin% on a categorical GRaster:

cover <- fast(madCover)
cover <- droplevels(cover)
levels(cover)

forestLabels <- c(
   "Sparse broadleaved evergreen/semi-deciduous forest",
   "Broadleaved deciduous forest",
   "Grassland with mosaic forest",
   "Flooded forest"
)

forest <- cover %in% forestLabels
plot(forest)

notForest <- cover %notin% forestLabels
plot(notForest)

}
