\dontrun{

# IMPORTANT: These function use the "location", "restartGrass", and
# "warn" arguments to avoid interfering with an existing GRASS session.
# WHEN YOU ARE DONE WITH THE EXAMPLES, run this line to revert to your
# active GRASS session:
# startFaster(location='default') # change "location" if not "default"

# IMPORTANT: Change this to where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

library(sf)
library(terra)

# start a GRASS session with a raster
madElev <- fasterData('madElev')

startFaster(madElev, inRastName = 'madElev',
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# import a vector
madRivers <- fasterData('madRivers')

fasterVect(madRivers, inVectName = 'madRivers',
location='examples') # line for examples only

# what's in the GRASS session?
fasterLs() # what rasters and vectors?
fasterLs('rasters') # rasters only
fasterLs('vectors') # vectors only

# does a particular object exist in the session?
fasterExists('madElev')
fasterExists('madElev', 'raster')
fasterExists('madElev', 'vector')
fasterExists(c('madElev', 'something'))

# information on the raster and vector
fasterInfo('madElev')
fasterInfo('madRivers')
fasterInfo() # everything

# piecemeal information on the raster
fasterGlobal('madElev', 'nonNA') # number of non-NA cells
fasterGlobal('madElev', 'NA') # number of NA cells
fasterGlobal('madElev', 'sum') # sum
fasterGlobal('madElev', 'min') # min value
fasterGlobal('madElev', 'max') # max value
fasterGlobal('madElev', 'mean') # mean value
fasterGlobal('madElev', 'cv') # coefficeint of variation
fasterGlobal('madElev', 'meanAbs') # mean absolute deviation

fasterRes('madElev') # resolution

fasterDim('madElev') # rows and columns
fasterNrow('madElev') # rows
fasterNcol('madElev') # columns
fasterNcell('madElev') # number or cells

# information on the coordinate reference system of the GRASS session
fasterCRS() # "plain" format
fasterCRS(TRUE) # nice for the eyes

# spatial extent of the GRASS session
fasterExt('madElev') # extent of madElev raster
fasterExt('madRivers') # extent of madRivers vector
fasterExt() # maximum extent of all spatial objects

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
