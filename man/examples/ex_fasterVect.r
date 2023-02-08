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

library(terra)

### Initiate a GRASS session using a vector in R

### start a GRASS session and export a vector
madCoast0 <- fasterData('madCoast0') # vector

# start the GRASS session
startFaster(vect=madCoast0, inVectName='madCoast0', grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# What's there?
fasterLs()

### export a vector to GRASS
madCoast4 <- fasterData('madCoast4') # vector

fasterVect(madCoast4, inVectName='madCoast4')
fasterLs()

# re-export the vector and overwrite
fasterVect(madCoast4, inVectName='madCoast4', replace=TRUE)

# re-export the vector but use a different name
fasterVect(madCoast4, inVectName='madCoast4prime')
fasterLs()

# export a vector from a file
vectFile <- system.file('extdata', '/shapes/madCoast.shp',
package='fasterRaster')

fasterVect(vectFile, inVectName='vectFromFile')
fasterLs()

# export multiple vectors (actually, same one, but different names)
fasterVect(
   c(vectFile, vectFile),
   inVectName=c('vectFromFile1', 'vectFromFile2')
)
fasterLs()

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
