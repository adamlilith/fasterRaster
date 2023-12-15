if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert SpatRaster to GRaster
elev <- fast(madElev)

### Save GRaster to disk (using temporary file)
filename <- tempfile()
filename <- paste0(filename, ".tif")
writeRaster(elev, filename)

### Load raster from disk
elev2 <- fast(filename)
elev2

}
