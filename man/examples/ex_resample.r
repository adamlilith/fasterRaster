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
library(terra)

# elevation raster, climate raster, rivers vector
madElev <- fastData('madElev')

# start GRASS session for examples only
wd <- forwardSlash(tempdir())

# initiate GRASS session
faster(crs = madElev, grassDir = grassDir,
workDir = wd, location = 'examples') # line only needed for examples

elev <- fast(madElev)

# resample raster to 120 x 120 m
elevResamp <- resample(elev, c(120, 120), method='bilinear')
elev
elevResamp

# IMPORTANT #3: Revert back to original GRASS session if needed.
sessionRestore(opts.)
removeSession('exampleFrom')
removeSession('exampleTo')

}
