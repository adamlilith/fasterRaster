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

### Start GRASS session with a raster.
madElev <- fasterData('madElev')

startFaster(rast=madElev, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

### Start GRASS session with a vector.
madCoast0 <- fasterData('madCoast0')

startFaster(vect=madCoast0, inVectName='madCoast', grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# What's in this session?
fasterLs()

### Start a different GRASS session then swicth back to the
# "examples" session created above.
# Normally, skip the third line. This is used just for examples.
startFaster(vect=madCoast0, inVectName='madCoast0_new', grassDir=grassDir,
location='new', replace=TRUE, restartGrass=TRUE, warn=FALSE)

# What's in this session?
fasterLs()

# Switch to "default" location.
startFaster(location='examples', grassDir=grassDir)

# What's in this session?
fasterLs()

}

