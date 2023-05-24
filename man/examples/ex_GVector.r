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

# sample data raster
madRivers <- fastData('madRivers')

# start GRASS session for examples only
wd <- forwardSlash(tempdir()) # only for examples

faster(crs = madRivers, grassDir = grassDir,
workDir = wd, location = 'examples') # line only needed for examples

# example data
madCoast4 <- fastData('madCoast4')
madRivers <- fastData('madRivers')
madDypsis <- fastData('madDypsis')

# convert SpatVectors to GVectors
coast <- fast(madCoast4, 'coast')
rivers <- fast(madRivers, 'rivers')
dypsis <- fast(madDypsis, 'dypsis')

# GVector properties
ext(rivers) # extent
crs(rivers) # coordinate reference system

# column names and data types
names(coast)
datatype(coast)

# session information
location(rivers) # GRASS location
mapset(rivers) # GRASS mapset

# points, lines, or polygons?
geomtype(dypsis)
geomtype(rivers)
geomtype(coast)

is.points(dypsis)
is.points(coast)

is.lines(rivers)
is.lines(dypsis)

is.polygons(coast)
is.polygons(dypsis)

# number of dimensions
topology(rivers)

### ADVANCED
# We can send a `SpatRaster` or `sf` vector to an open GRASS connection
# without making it a `GVector`.

vectToGrass(madRivers, gn = 'rivers_vect1')

# This is nearly the same as:
madRiversVect <- vect(madRivers)
rgrass::write_VECT(madRiversVect, vname = 'rivers_vect2')

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
