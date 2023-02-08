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

# load rasters
madElev <- fasterData('madElev')
madForest2000 <- fasterData('madForest2000')

# load vectors
madCoast0 <- fasterData('madCoast0') # get vector

# start a GRASS session with a raster
startFaster(madElev,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# export another raster
fasterRast(madForest2000)

# export a vector
fasterVect(madCoast0, inVectName = 'madCoast0')

# names of all rasters and vectors in the GRASS session
fasterLs()

# rename raster
fasterRename('madElev', 'elevation')
fasterLs()

# copy raster
fasterCopy('elevation', 'elevationCopy')
fasterLs()

# remove raster
fasterRm('elevation')
fasterLs()

# remove vector
fasterRm('madCoast0')
fasterLs()

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}

