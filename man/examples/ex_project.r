if (grassStarted()) {

### Setup for all examples
##########################

library(sf)
library(terra)

# Climate raster, elevation raster, rivers vector
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")
madChelsa <- fastData("madChelsa")

# Convert objects into fasterRaster formats
chelsa <- fast(madChelsa)
elev <- fast(madElev)
rivers <- fast(madRivers)

### Project raster without resampling
elevWGS84 <- project(elev, crs(chelsa))
elevWGS84

### Project raster and resample to resolution of another raster
elevWGS84Resamp <- project(elev, chelsa)
elevWGS84Resamp

res(elevWGS84)
res(elevWGS84Resamp)
res(chelsa)

### Project vector
riversWGS84 <- project(rivers, chelsa)
riversWGS84
cat(crs(rivers)) # using "cat()" to make it look nice
cat(crs(riversWGS84))

}
