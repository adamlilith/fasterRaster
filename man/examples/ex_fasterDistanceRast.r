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

# get elevation raster
madElev <- fasterData('madElev')

### distance to forest
distToLand <- fasterDistanceRast(madElev, nearestValue=TRUE,
grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(c(madElev, distToLand))

# distance to coast
distToCoast <- fasterDistanceRast('madElev', fillNA=FALSE,
grassDir=grassDir)

plot(distToCoast, main='Distance to coast (m)')

### compare to terra's distance()
terraToLand <- distance(madElev)
terraToLand - distToLand[['distanceRast']]
plot(c(terraToLand, distToLand[['distanceRast']]))
plot(terraToLand - distToLand[['distanceRast']])

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
