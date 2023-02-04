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
library(terra)

# data
madElev <- fasterData('madElev')

fract22 <- fasterFractalRast(rast=madElev, dimension=2.2,
outGrassName='fract2.2', grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

fract27 <- fasterFractalRast(rast='madElev', dimension=2.7,
outGrassName='fract2.7', grassDir=grassDir,
location='examples') # line for examples only

plot(c(fract22, fract27), main=c('D = 2.2', 'D = 2.7'))

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
initGrass(location='default')

}
