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
library(sf)
library(terra)

# example data
madElev <- fastData('madElev')
madRivers <- fastData('madRivers')

# start GRASS session for examples only
fastStart(grassDir = grassDir, crs = madElev,
workDir = rightSlash(tempdir()), location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster and a SpatVector to a GVector
me <- fast(madElev)
mr <- fast(madRivers)

# see that these are in GRASS
fasterRaster:::.ls()

# remove the GRaster and GVector from GRASS
rm(me, mr)

# NOTE! The `mr` and `me` objects still appear to be in R!
me
mr

# Revert back to original GRASS session if needed.
fastRestore(opts.)

}