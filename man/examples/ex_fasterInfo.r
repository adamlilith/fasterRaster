\dontrun{

library(sf)
library(terra)

# start a GRASS session with a raster
madElev <- fasterData('madElev')
initGrass(madElev, inRastName = 'madElev')

# import a vector
madCoast0 <- fasterData('madCoast0')
exportVectToGrass(madCoast0, inVectName = 'madCoast0')

# get information on the raster and vector
fasterInfoRast('madElev')
fasterInfoVect('madCoast0')

# information on the coordinate reference system
fasterCRS() # "plain" format
fasterCRS(TRUE) # nice for the eyes

# spatial extent of the GRASS session
fasterExt() # returns a SpatExtent object
fasterExt(FALSE) # returns a vector

# spatial resolution
fasterRes()

# names of all rasters and vectors in the GRASS session
fasterLs()

}

