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

# These are a contrived examples, as we will project a vector to a different
# coordinate reference system (in R), then reproject it back to the original
# using GRASS.
madElev <- fasterData('madElev') # raster

madCoast0 <- fasterData('madCoast0') # vector to project
wgs84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84'
wgs84Vect <- st_transform(madCoast0, wgs84)

### project a vector using fasterProjectVect()
madProj <- fasterProjectVect2(wgs84Vect, madElev, grassDir=grassDir,
fromLocation='exampleFrom', location='examples',
restartGrass=TRUE, warn=FALSE) # last two lines for examples only

crs(madProj)

### project using a pre-existing GRASS location

# get vector and project to WGS84
madCoast0 <- fasterData('madCoast0')
wgs84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84'
wgs84Vect <- st_transform(madCoast0, wgs84)

# create the "examples" location
startFaster(madElev, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

fasterCRS() # coordinate reference system of the "examples" location

# import vector into it
madProj <- fasterProjectVect2(wgs84Vect, template=NULL, grassDir=grassDir,
fromLocation='exampleFrom2',
location='examples') # line for examples only

crs(madProj)

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
