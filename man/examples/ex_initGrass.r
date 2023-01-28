\dontrun{

# IMPORTANT: These function use the "location", "restartGrass", and
# "warn" arguments to avoid interfering with an existing GRASS session.
# WHEN YOU ARE DONE WITH THE EXAMPLES, run this line to revert to your
# active GRASS session:
# initGrass(location='default') # change "location" if not "default"

# IMPORTANT: Change this to where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac
grassDir <- '/usr/local/grass' # example for Linux
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC

library(sf)
library(terra)

### Start GRASS session with a raster.
# Normally, skip the second line. This is used just for examples.
madElev <- fasterData('madElev')

initGrass(rast=madElev, grassDir=grassDir,
location='examples', replace=TRUE, restartGrass=TRUE, warn=FALSE) 

### Start GRASS session with a vector.
# Normally, skip the second line. This is used just for examples.
madCoast0 <- fasterData('madCoast0')

initGrass(vect=madCoast0, inVectName='madCoast', grassDir=grassDir,
location='examples', replace=TRUE, restartGrass=TRUE, warn=FALSE) 

# What's in this session?
fasterLs()

### Start a different GRASS session then swicth back to the
# "examples" session created above.
# Normally, skip the third line. This is used just for examples.
initGrass(vect=madCoast0, location='new',
inVectName='madCoast0_new', grassDir=grassDir,
replace=TRUE, restartGrass=TRUE, warn=FALSE)

# What's in this session?
fasterLs()

# Switch to "default" location.
initGrass(location='examples', grassDir=grassDir)

# What's in this session?
fasterLs()

}

