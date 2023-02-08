\dontrun{
# IMPORTANT: These function use the "location", "restartGrass", and
# "warn" arguments to avoid interfering with an existing GRASS session.
# WHEN YOU ARE DONE WITH THE EXAMPLES, run this line to revert to your
# active GRASS session:
# initGrass(location='default') # change "location" if not "default"

# IMPORTANT: Change this to where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

library(sf)

### save a vector to disk from GRASS

# start GRASS session with a vector
madCoast0 <- fasterData('madCoast0')

startFaster(vect=madCoast0, inVectName='madCoast0', grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# save the vector to disk (using temporary folder here)
filename <- paste0(tempdir(), '/madCoast0.gpkg')

# Write vector, but do not return it to R:
fasterWriteVector('madCoast0', filename)

# Write vector, and return to R:
mc <- fasterWriteVector('madCoast0', filename) # 2

# Just return vector to R:
mc <- vectFromGrass('madCoast0')
mc <- rgrass::read_VECT('madCoast0', flags='quiet')

### save in other formats
# comma-seperated value (table only)
filename <- paste0(tempfile(), '/madCoast.csv')
fasterWriteVector('madCoast0', filename, format='CSV', overwrite=TRUE)

# KML for use in Google Earth
filename <- paste0(tempfile(), '/madCoast.kml')
fasterWriteVector('madCoast0', filename, format='KML', overwrite=TRUE)

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
