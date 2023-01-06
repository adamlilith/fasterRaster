\dontrun{

library(sf)
library(terra)

# start a GRASS session with a raster
madElev <- fasterData('madElev')
initGrass(madElev, inRastName = 'madElev')

# import a vector
madCoast0 <- fasterData('madCoast0')
exportVectToGrass(madCoast0, inVectName = 'madCoast0')

# names of all rasters and vectors in the GRASS session
fasterLs()

# rename raster
fasterRename('madElev', 'elevation')

# remove raster
fasterRm('elevation')
fasterLs()

# remove vector
fasterRm('madCoast0')
fasterLs()

}

