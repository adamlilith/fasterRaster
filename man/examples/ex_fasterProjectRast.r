\donttest{

library(terra)

# change this according to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(terra)
madElev <- fasterData('madElev')
madForest2000 <- fasterData('madForest2000')

crs(madElev)
crs(madForest2000)

elevResamp <- fasterProjectRast(rast=madElev,
template=madForest2000, grassDir=grassDir)

oldPar <- par(mfrow=c(1, 2))
plot(madElev, main='Original')
plot(elevResamp, main='Resampled')
par(oldPar)

}
