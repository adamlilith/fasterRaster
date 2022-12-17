\donttest{

# change this according to where GRASS is installed on your system
# grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
# grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(terra)
madForest2000 <- fasterData('madForest2000')

# could also use distance() function from the raster package which is
# slower in this example
distToForest <- fasterRastDistance(rast=madForest2000,
fillNAs=TRUE, grassDir=grassDir)

# terra
# distToForest <- distance(madForest2000)

par(mfrow=c(1, 2))
plot(madForest2000, 'Forest', col='forestgreen')
plot(distToForest, main='Distance to Forest (m)')

}
