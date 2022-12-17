\donttest{

# change this according to where GRASS is installed on your system
# grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
# grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(terra)

madForest2000 <- fasterData('madForest2000')

# could also use rasterToPolygons() which is
# probably faster in this example
forestPolyGrass <- fasterVectorize(rast=madForest2000,
	vectType='area', grassDir=grassDir)

forestPolyTerra <- rasterToPolygons(madForest2000, dissolve=TRUE)

par(mfrow=c(1, 3))
plot(madForest2000, main='Forest as Raster', col='forestgreen')
plot(forestPolyGrass, main='Forest as Polygon', col='forestgreen', border=NA)
plot(forestPolyTerra, main='Forest as Polygon', col='forestgreen', border=NA)

}
