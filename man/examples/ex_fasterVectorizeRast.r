\dontrun{

# change this according to where GRASS is installed on your system
# grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
# grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(terra)

madForest2000 <- fasterData('madForest2000')

# You could also use as.polygons() from terra which is
# faster in this example.
forestPolyGrass <- fasterVectorizeRast(rast=madForest2000,
	vectType='area', grassDir=grassDir)

forestPolyTerra <- as.polygons(madForest2000, dissolve=TRUE)

}
