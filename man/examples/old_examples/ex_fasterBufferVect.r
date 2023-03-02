\dontrun{

# IMPORTANT: These function use the "location", "restartGrass", and
# "warn" arguments to avoid interfering with an existing GRASS session.
# WHEN YOU ARE DONE WITH THE EXAMPLES, run this line to revert to your
# active GRASS session:
# fastStarter(location='default') # change "location" if not "default"

# IMPORTANT: Change this to where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

library(sf)
library(terra)

madRivers <- fasterData('madRivers')

riverBuff <- fasterBufferVect(madRivers, width=2000, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

terraBuff <- buffer(vect(madRivers), 2000)

plot(riverBuff)
plot(st_geometry(madRivers), col='blue', add=TRUE)

plot(terraBuff)
plot(st_geometry(madRivers), col='blue', add=TRUE)

}
