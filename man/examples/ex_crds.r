\dontrun{
# NB This example is in a "dontrun{}" block because it requires users to have
# GRASS GIS Version 8+ installed on their system.

# IMPORTANT #1: If you already have a GRASS session started, you will need to
# run the line below and the last line in this example to work with it again.
# If you have not started a GRASS session, you can skip this step and go to
# step #2.

# IMPORTANT #2: Select the appropriate line below and change as necessary to
# where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

# setup
library(sf)

# plant specimens (points), rivers (lines), communes (polygons)
madDypsis <- fastData('madDypsis')
madRivers <- fastData('madRivers')
madCoast4 <- fastData('madCoast4')

# start GRASS session for examples only
wd <- forwardSlash(tempdir())

fastStart(crs = madDypsis, grassDir = grassDir,
workDir = wd, location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster, and sf to a GVector
dypsis <- fast(madDypsis)
rivers <- fast(madRivers)
coast <- fast(madCoast4)

### get coordinates
dypPoints <- crds(dypsis)


# IMPORTANT #3: Revert back to original GRASS session if needed.
sessionRestore(opts.)
removeSession('examples')

}
