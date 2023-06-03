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

# plant specimens (points), elevation (raster)
madDypsis <- fastData('madDypsis')
madRivers <- fastData('madRivers')
madCoast4 <- fastData('madCoast4')
madElev <- fastData('madElev')

# start GRASS session for examples only
faster(crs = madDypsis, grassDir = grassDir,
workDir = tempdir(), location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster, and sf to a GVector
dypsis <- fast(madDypsis)
rivers <- fast(madRivers)
coast <- fast(madCoast4)
elev <- fast(madElev)

### get coordinates
dypsisPoints <- crds(dypsis)
riversPoints <- crds(rivers)
coastPoints <- crds(coast)
elevPoints <- crds(elev)

head(dypsisPoints)
head(riversPoints)
head(coastPoints)
head(elevPoints)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
