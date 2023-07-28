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
grassDir <- "C:/Program Files/GRASS GIS 8.3" # Windows
grassDir <- "/usr/local/grass" # Linux

# setup
library(terra)

# example data
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")

# set up one location
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Importing a raster sets the region to match its extent and dimensions.
elev <- fast(madElev)
elev
region()

# Importing a vector does not change the region.
dypsis <- fast(madDypsis)
region()

# report information on current region
(start <- region())
regionDim()
regionExt()
regionRes()

dim()
nrow()
ncol()
ndepth()
ncell()
ncell3d()
nlyr()

ext()
west()
east()
south()
north()

res()
res3d()
xres()
yres()
zres()

# coarsen grid... sets region to match aggregated raster"s extent and dimensions
aggElev <- aggregate(elev, 2) 

# reset region dimensions
dim()
regionDim(elev, respect = "resolution")
dim()

# reset region extent
ext()
regionExt(dypsis, respect="resolution")
ext()

# reset region resolution
res()
regionRes(elev, respect="extent")
res()

# reset region
(region(start))

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession("examples")

}
