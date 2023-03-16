# Examples with scalar and scalar vector:
fasterConvertDegree(0)
fasterConvertDegree(seq(0, 360, by=90))

# Examples with a raster
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

madElev <- fasterData('madElev')

# calculate slope with east = 0 (default is for north = 0)
aspEastIs0 <- fasterTerrain(madElev, v='aspect',
northIs0=FALSE, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# convert to north = 0
aspNorthIs0 <- fasterConvertDegree(aspEastIs0, grassDir=grassDir)

plot(c(aspEastIs0, aspNorthIs0), main=c('east = 0', 'north = 0'))

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
