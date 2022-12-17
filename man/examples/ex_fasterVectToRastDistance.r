\donttest{

# change this according to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(sf)
library(terra)

madForest2000 <- fasterData('madForest2000')
madRivers <- fasterData('madRivers')

distToRiver <- fasterVectToRastDistance(rast=madForest2000,
vect=madRivers, grassDir=grassDir)

plot(distToRiver, main='Distance to Rivers (m)')
plot(madRivers, col='blue', add=TRUE)

}
