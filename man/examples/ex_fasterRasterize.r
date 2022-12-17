\donttest{
# change this according to where GRASS is installed on your system
# grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
# grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(sf)
library(terra)

madCoast <- fasterData('madCoast0')
madForest2000 <- fasterData('madForest2000')

# could also use rasterize() or mask() from the raster package which may
# be faster in this example
madMask <- fasterRasterize(vect=madCoast0, rast=madForest2000, grassDir=grassDir)

# terra
# madMask <- rasterize(madCoast0, madForest2000)
# madMask <- mask(madForest2000, madCoast0)

plot(madMask, main='Portion of Eastern Madagascar')
plot(madCoast0, add=TRUE)

}
