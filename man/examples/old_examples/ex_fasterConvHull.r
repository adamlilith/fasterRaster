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

madDypsis <- fasterData('madDypsis')

# minimum convex hull
mch <- fasterConvHull(madDypsis, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(madElev)
plot(st_geometry(madDypsis), add=TRUE)
plot(mch, add=TRUE)

# Revert back to original GRASS session.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
