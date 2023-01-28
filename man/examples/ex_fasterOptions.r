\dontrun{

# IMPORTANT: These function use the "location", "restartGrass", and
# "warn" arguments to avoid interfering with an existing GRASS session.
# WHEN YOU ARE DONE WITH THE EXAMPLES, run this line to revert to your
# active GRASS session:
# initGrass(location='default') # change "location" if not "default"

# For fasterRaster functions below, this will be used to set up a location
# that does not interfere with any locations you may be using. Usually, you
# do not need to use the argument "inits".
inits <- list(location='examples', restartGrass=TRUE, warn=FALSE)

# IMPORTANT: Change this to where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac
grassDir <- '/usr/local/grass' # example for Linux
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC

library(sf)
library(terra)

# get raster data
madElev <- fasterData('madElev')

### Calling a function without setting options:

# calculate contours... note the number of arguments we need to define
# (some are using their defaults here, so we normally could just use those)
levels <- seq(100, 500, by=100)
conts <- fasterContour(madElev, levels=levels,
grassDir=grassDir, replace=FALSE, grassToR=TRUE, outVectClass='SpatVector',
inits=inits)









}
