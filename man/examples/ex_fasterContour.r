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

madElev <- fasterData('madElev')

# create contours
levels <- c(100, 200, 300, 400, 500)

conts <- fasterContour(madElev, levels=levels, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(madElev)
plot(conts, add=TRUE)

# Revert back to original GRASS session.
# Change to your working location if not "default" (it usually is).
initGrass(location='default')

}
