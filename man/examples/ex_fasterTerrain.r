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

# elevation raster
madElev <- fasterData('madElev')

# calculate all topographic indices
topos <- fasterTerrain(madElev, v='*', grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(topos)

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
