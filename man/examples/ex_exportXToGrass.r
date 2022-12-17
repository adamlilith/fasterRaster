
\donttest{

library(sf)
library(terra)

# start a GRASS session
madForest2000 <- fasterData('madForest2000')
initGrass(madForest2000, rastName = 'madForest2000', grassDir = grassDir)

# export a raster
madElev <- fasterData('madElev')
exportRastToGrass(madElev, grassName = 'madElev', grassDir = grassDir)

# export a vector
madCoast0 <- fasterData('madCoast0')
exportVectToGrass(madCoast0, grassName = 'madCoast0', grassDir = grassDir)

}
