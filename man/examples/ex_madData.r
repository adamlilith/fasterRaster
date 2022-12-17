
### vector data
library(sf)

# for vector data, we can use data(*) or fasterData(*):
data(madCoast0) # same as next line
madCoast <- fasterData('madCoast0') # same as previous
plot(madCoast0)

madCoast <- fasterData('madCoast4')
plot(madCoast4, add = TRUE)

madRivers <- fasterData('madRivers')
plot(madRivers, add = TRUE)

### raster data
library(terra)

# for raster data, we can get the file directly or using fasterData(*):
rastFile <- system.file('extdata/madElev.tif'), package='fasterRaster')
madElev <- terra::rast(rastFile)

madElev <- fasterData('madElev') # same as previus two lines
plot(madElev)

madForest2000 <- fasterData('madForest2000')
plot(madForest2000, col = 'forestgreen', add = TRUE)

madForest2014 <- fasterData('madForest2014')
plot(madForest2014, col = 'green', add = TRUE)
