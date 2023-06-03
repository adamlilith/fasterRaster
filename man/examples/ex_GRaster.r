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
madForest2000 <- fastData('madForest2000')

# start GRASS session for examples only
faster(crs = madElev, grassDir = grassDir,
workDir = tempdir(), location = 'examples') # line only needed for examples

# convert SpatRasters to GRasters
elev <- fast(madElev)
forest <- fast(madForest2000)

### GRaster properties

# dimensions
dim(elev) # rows, columns, depths, layers
nrow(elev) # rows
ncol(elev) # columns
ndepth(elev) # depths
nlyr(elev) # layers

res(elev) # resolution (2D)
res3d(elev) # resolution (3D)
zres(elev) # vertical resolution

# cell counts
ncell(elev) # cells
ncell3d(elev) # cells (3D rasters only)

# topology
topology(elev) # number of dimensions
is.2d(elev) # is it 2-dimensional?
is.3d(elev) # is it 3-dimensional?

minmax(elev) # min/max values

# information on the GRASS session in which the GRaster is located
location(elev) # location
mapset(elev) # mapset

# "gnames" of the object (its name in GRASS)
gnames(elev)

# coordinate reference system
crs(elev)
st_crs(elev)

# extent (bounding box)
ext(elev)
st_bbox(elev)

# vertical extent (not defined for this raster)
zext(elev)

# data type
datatype(elev)

# convert data type
as.cell(elev) # integer; note that "elev" is already of type "CELL"
as.fcell(elev) # floating-precision
as.dcell(elev) # double-precision

# concatenating multiple GRasters
rasts <- c(elev, forest)
rasts

# number of layers
nlyr(rasts)

# names
names(rasts)
names(rasts) <- c('elev_meters', 'forest')
rasts

# cell frequencies
freq(elev)
freq(2 * elev)
freq(rasts)
freq(rasts, value = 1)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
