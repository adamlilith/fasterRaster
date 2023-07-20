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
grassDir <- "/Applications/GRASS-8.3.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.3' # Windows
grassDir <- '/usr/local/grass' # Linux

# setup
library(sf)

# example vectors
madDypsis <- fastData('madDypsis') # points
madCoast4 <- fastData('madCoast4') # polygons

# initiate GRASS session
faster(x = madDypsis, grassDir = grassDir,
workDir = tempdir(), location = 'examples') # line only needed for examples

# convert sf vectors to GVectors
dypsis <- fast(madDypsis)
coast <- fast(madCoast4)

# Delaunay triangulation
dypsisDel <- delaunay(dypsis)

# Voronoi tessellation
dypsisVor <- voronoi(dypsis)
coastVor <- voronoi(coast)


# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
