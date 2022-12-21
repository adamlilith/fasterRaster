\dontrun{

# change this to where GRASS is installed on your system
# grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
# grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(terra)

madElev <- fasterData('madElev')
madForest2000 <- fasterData('madForest2000')

# must first project elevation raster
madElev_albers <- fasterProjectRast(madElev, template=madForest2000,
grassDir=grassDir)

twi <- fasterTopidx(madElev_albers, grassDir=grassDir)

par(mfrow=c(1, 2))
plot(madElev_albers, main='Elevation (m)')
plot(twi, main='TWI')

}


