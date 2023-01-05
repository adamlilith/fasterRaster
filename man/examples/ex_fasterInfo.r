\dontrun{

library(sf)
library(terra)

# start a GRASS session with a raster and get information on it
madElev <- fasterData('madElev')
initGrass(madElev, inRastName = 'madElev')
fasterInfoRast('madElev')

# export a vector and get information on it
madCoast0 <- fasterData('madCoast0')
exportVectToGrass(madCoast0, inVectName = 'madCoast0')
fasterInfoVect('madCoast0')

# information on the coordinate rafeternec system
fasterCRS()

# names of all rasters and vectors in the GRASS session
fasterLs()

# remove raster
fasterRm('madElev')
fasterLs()

# remove vector
fasterRm('madCoast0')
fasterLs()

}

