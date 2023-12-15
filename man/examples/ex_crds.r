if (grassStarted()) {

# Setup
library(terra)

# Plant specimens (points), elevation (raster)
madDypsis <- fastData("madDypsis")
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster, and sf to a GVector
dypsis <- fast(madDypsis)
elev <- fast(madElev)

### Get coordinates:
dypsisPoints <- crds(dypsis)
elevPoints <- crds(elev)

head(dypsisPoints)
head(elevPoints)

}
