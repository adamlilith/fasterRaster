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

# set up one location
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples1") # line only needed for examples

# convert a SpatRaster to a GRaster using the first location
me1 <- fast(madElev)

# set up another location
faster(crs = madElev, grassDir = grassDir,
workDir = wd, location = "examples2") # line only needed for examples

# convert a SpatRaster to a GRaster using the second location
me2 <- fast(madElev)

location(me1)
location(me2)

# What GRASS location and mapset are we currently in?
location()
mapset()

# Meta-data on all active GRASS Locations
locations()

# What coordinate reference system?
crs()

# We cannot do any work on me1 because we"re in the "examples2" location. We
# need to switch back to "examples1":
restoreSession(location = "examples1")

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples1")
removeSession("examples2")

}
