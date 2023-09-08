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

# start GRASS session for examples only
faster(grassDir = grassDir, crs = madElev,
workDir = tempdir(), location = "examples") # line only needed for examples

# convert a SpatRaster to a GRaster
elev <- fast(madElev)

### stretch based on user-defined range

#  fasterRaster
fr <- stretch(elev, smin=1, smax=100)
fr

# terra
tr <- stretch(madElev, smin = 1, smax = 100)
tr

# compare fasterRaster to terra output
fr <- rast(fr)
fr <- extend(fr, tr)
fr - tr

### stretch values in a certain quantile range

#  fasterRaster
fr <- stretch(elev, minq = 0.25, maxq = 0.75)
fr

# terra
tr <- stretch(madElev, minq = 0.25, maxq = 0.75)
tr

# compare fasterRaster to terra output
fr <- rast(fr)
fr <- extend(fr, tr)
fr - tr

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
