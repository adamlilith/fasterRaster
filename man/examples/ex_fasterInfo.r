\dontrun{

library(sf)
library(terra)

# start a GRASS session with a raster
madElev <- fasterData('madElev')
initGrass(madElev, inRastName = 'madElev')

# import a vector
madRivers <- fasterData('madRivers')
fasterVect(madRivers, inVectName = 'madRivers')

# what's in the GRASS session?
fasterLs('rasters') # what rasters?
fasterLs('vectors') # what vectors?
fasterLs() # what rasters and vectors?

# does a particular object exist in the session?
fasterExists('madElev')
fasterExists('madElev', 'raster')
fasterExists('madElev', 'vector')
fasterExists(c('madElev', 'something'))

# information on the raster and vector
fasterInfo('madElev')
fasterInfo('madCoast0')
fasterInfo() # everything

# information on the coordinate reference system of the GRASS session
fasterCRS() # "plain" format
fasterCRS(TRUE) # nice for the eyes

# spatial extent of the GRASS session
fasterExt('madElev') # extent of madElev raster
fasterExt('madRivers') # extent of madRivers vector
fasterExt() # maximum extent of all spatial objects

}
