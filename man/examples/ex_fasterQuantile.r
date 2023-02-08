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

# get raster
madElev <- fasterData('madElev')

probs <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)

quants <- fasterQuantile(madElev, probs=probs, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

quants

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
