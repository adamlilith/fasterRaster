\dontrun{
# NB This example is in a "dontrun{}" block because it requires users to have
# GRASS GIS Version 8+ installed on their system.

# IMPORTANT #1: If you already have a GRASS session started, you will need to
# run the line below and the last line in this example to work with it again.
# If you have not started a GRASS session, you can skip this step and go to
# step #2.
opts. <- getFastOptions()

# IMPORTANT #2: Select the appropriate line below and change as necessary to
# where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

# setup
library(sf)
library(terra)

# elevation raster, climate raster, rivers vector
madElev <- fastData('madElev')
madChelsa <- fastData('madChelsa')
madRivers <- fastData('madRivers')

# project rivers to WGS84 using terra (for this example)
# (The CHELSA raster stack is already in WGS84.)
madRivers <- vect(madRivers)
madRivers <- project(madRivers, madChelsa)

# start GRASS session for examples only
wd <- forwardSlash(tempdir())

# set up "to" location
faster(crs = madElev, grassDir = grassDir,
workDir = wd, location = 'exampleTo') # line only needed for examples

elev <- fast(madElev)

# set up "from" location and put a raster and vector in it
faster(crs = madChelsa, grassDir = grassDir,
workDir = wd, location = 'exampleFrom') # line only needed for examples

chelsa <- fast(madChelsa)
rivers <- fast(madRivers)

# re-activate the "exampleTo" location
fastRestore(location='exampleTo')

# project vector into the "exampleTo" location
riversProj <- project(rivers)
rivers
riversProj

# project raster into the "exampleTo" location but do not resample
chelsaProjSameRes <- project(chelsa)

# Project raster into the "exampleTo" location and resample to match template.
# This can take a while...
chelsaProjNewRes <- project(chelsa, elev)

# compare outcomes
chelsaProjSameRes
chelsaProjNewRes

# We get something a bit different if we resample after projecting:
chelsaResampAfterProj <- resample(chelsaProjSameRes, elev)

chelsaProjNewRes
chelsaResampAfterProj

# We can also project rasters/vectors with a different CRS than the current
# "location" by importing them from disk using fast():
vectFile <- system.file('extdata', 'shapes/madCoast.shp', package='fasterRaster')
coastProj <- fast(vectFile)
coastProj

rastFile <- system.file('extdata', 'madChelsa.tif', package='fasterRaster')
chelsaProjFromFile <- fast(rastFile, method='bilinear')
chelsaProjFromFile

# Projecting from file or from location to location yields same results:
chelsaProjSameRes
chelsaProjFromFile

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('exampleFrom')
removeSession('exampleTo')

}
