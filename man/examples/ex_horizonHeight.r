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
library(terra)

# example data
madElev <- fastData('madElev')

# start GRASS session for examples only
faster(grassDir = grassDir, crs = madElev,
workDir = tempdir(), location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster
elev <- fast(madElev)

# calculate horizon height in north and east directions
hhNorthEast <- horizonHeight(elev, directions = c(0, 90))

# calculate horizon height in east and north directions
hhEastNorth <- horizonHeight(elev, directions = c(0, 90), northIs0 = FALSE)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
