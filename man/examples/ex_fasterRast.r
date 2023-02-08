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

### Initiate a GRASS session using a raster in R

### start a GRASS session and export a raster
madForest2000 <- fasterData('madForest2000') # raster

# start the GRASS session
startFaster(madForest2000, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# What's there?
fasterLs()

### export a raster to GRASS
madElev <- fasterData('madElev') # raster
fasterRast(madElev)
fasterLs()

# re-export the raster and overwrite
fasterRast(madElev, replace=TRUE)

# re-export the raster but use a different name
fasterRast(madElev, inRastName='madElev2')
fasterLs()

# export a raster from a file
rastFile <- system.file('extdata', 'madForest2014.tif',
package='fasterRaster')

fasterRast(rastFile)
fasterLs()

# export more than one raster from a file
rastFile1 <- system.file('extdata', 'madElevAnt.tif',
package='fasterRaster')

rastFile2 <- system.file('extdata', 'madElevMan.tif',
package='fasterRaster')

fasterRast(c(rastFile1, rastFile2), replace=TRUE)
fasterLs()

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
