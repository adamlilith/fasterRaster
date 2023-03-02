
### vector data
library(sf)

# for vector data, we can use data(*) or fastData(*):
data(madCoast0) # same as next line
madCoast <- fastData('madCoast0') # same as previous
plot(st_geometry(madCoast0))

madCoast <- fastData('madCoast4')
plot(st_geometry(madCoast4), add = TRUE)

madRivers <- fastData('madRivers')
plot(st_geometry(madRivers), col='cornflowerblue', lwd=3, add = TRUE)

### raster data
library(terra)

# for raster data, we can get the file directly or using fastData(*):
rastFile <- system.file('extdata/madElev.tif'), package='fasterRaster')
madElev <- terra::rast(rastFile)

madElev <- fastData('madElev') # same as previus two lines
plot(madElev)

madForest2000 <- fastData('madForest2000')
plot(madForest2000, col = 'forestgreen', legend = FALSE, add = TRUE)

madForest2014 <- fastData('madForest2014')
plot(madForest2014, col = 'green', legend = FALSE, add = TRUE)
