\dontrun{

# IMPORTANT #1: If you already have a GRASS session started, you will need to
# run the line below and the last line in this example to work with it again.

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
fastStart(grassDir = grassDir, crs = madElev,
workDir = rightSlash(tempdir()), location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster
me <- fast(madElev)

# calculate contour lines
conts <- as.contour(me, nlevels=10)

plot(madElev)
plot(conts, add=TRUE)

# Revert back to original GRASS session if needed.
fastRestore(opts.)

# If you didn't run the very first line in this example, you can probably
# return to your previous GRASS session using:
# fastRestore(location='default')

}
