\dontrun{

# IMPORTANT #1: If you already have a GRASS session started, you will need to
# run the line below and the last line in this example to work with it again.
opts. <- getFastOptions()

# IMPORTANT #2: Select the appropriate line below and change as necessary to
# where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

# setup
library(terra)

# example data
madElev <- fastData('madElev')
madForest2000 <- fastData('madForest2000')

# start GRASS session for examples only
wd <- forwardSlash(tempdir()) # only for examples

fastStart(crs = madElev, grassDir = grassDir,
workDir = wd, location = 'examples') # line only needed for examples

# convert SpatRasters to GRasters
me <- fast(madElev)
for00 <- fast(madForest2000)

# GRaster properties
ext(me) # extent
dim(me) # rows and columns
ncell(me) # cells
nrow(me) # rows
ncol(me) # columns
crs(me) # coordinate reference system
res(me) # resolution
minmax(me) # min/max values
location(me) # GRASS location
mapset(me) # GRASS mapset

topology(me) # number of dimensions

stack <- c(me, for00)
stack

names(stack) # names
nlyr(stack) # number of layers

# Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
