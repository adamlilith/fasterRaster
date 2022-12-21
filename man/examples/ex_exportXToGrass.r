
\dontrun{

library(sf)
library(terra)

# change this to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

# start a GRASS session
madForest2000 <- fasterData('madForest2000')
initGrass(madForest2000, inRastName = 'madForest2000', grassDir = grassDir)

# export a raster
madElev <- fasterData('madElev')
exportRastToGrass(madElev, inRastName = 'madElev')

# export a vector
madCoast0 <- fasterData('madCoast0')
exportVectToGrass(madCoast0, inVectName = 'madCoast0')

}
