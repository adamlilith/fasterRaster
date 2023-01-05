\dontrun{

library(terra)

# change this to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

madForest2000 <- fasterData('madForest2000')

# notice that values are unprojected (WGS84), even though the input raster
# is projected
unMasked <- fasterLongLatRasts(rast=madForest2000, mask=FALSE,
grassDir=grassDir)
plot(unMasked)

masked <- fasterLongLatRasts(rast=madForest2000, grassDir=grassDir)
plot(masked)

}
