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
madElevFile <- system.file('extdata', 'madElev.tif', package='fasterRaster')
madRivers <- fastData('madRivers')
madRiversFile <- system.file('extdata/shapes/madCoast.shp',
     package='fasterRaster')


# start GRASS session for examples only
fastStart(grassDir = grassDir, crs = madElev,
workDir = rightSlash(tempdir()), location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster
me <- fast(madElev)
me

# create a GRaster directly from a GeoTIFF file
mef <- fast(madElevFile)
mef

# convert an sf vector to a GVector
mr <- fast(madRivers)
mr

# create a GVector directly from a shapefile
mrf <- fast(madRiversFile)
mrf

# Revert back to original GRASS session if needed.
fastRestore(opts.)

}
