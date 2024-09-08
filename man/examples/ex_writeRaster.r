if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madChelsa <- fastData("madChelsa")

### What raster formats can we attempt to write?
writeRaster()

### Save GRaster to disk (using temporary file)
elev <- fast(madElev)
filename <- tempfile(fileext = ".tif")
writeRaster(elev, filename)

# Load raster from disk
elev2 <- fast(filename)
elev2

### Save multi-layer GRaster to disk in one file (using temporary file)
chelsa <- fast(madChelsa)
filename <- tempfile(fileext = ".tif")
writeRaster(chelsa, filename)

# Load raster from disk
chelsa2 <- fast(filename)
chelsa2

### Save multi-layer GRaster to disk layer-by-layer (using temporary file)
chelsa <- fast(madChelsa)
filename <- tempfile(fileext = ".tif")
writeRaster(chelsa, filename, byLayer = TRUE)

# Load one of the rasters from disk
filename2 <- sub(filename, pattern = ".tif", replacement = "_bio1.tif")
chelsaBio1 <- fast(filename2)
chelsaBio1

}
