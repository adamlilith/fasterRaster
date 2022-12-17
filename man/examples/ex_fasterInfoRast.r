\donttest{

library(sf)
library(terra)

# start a GRASS session with a raster and get information on it
madElev <- fasterData('madElev')
initGrass(madElev, rastName = 'madElev')
fasterInfoRast('madElev')

# start a GRASS session with a vector and get information on it
madCoast0 <- fasterData('madCoast0')
exportVectToGrass(madCoast0, vectName = 'madCoast0')
fasterInfoVect('madCoast0')

}

