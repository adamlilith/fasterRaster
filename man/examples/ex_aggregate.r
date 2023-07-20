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
library(terra)

# example data
madElev <- fastData('madElev')

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster
elev <- fast(madElev)

### aggregate by same factor in 2 dimensions
# fasterRaster
agg2 <- aggregate(elev, 2, 'mean')
agg2

# terra
agg2terra <- aggregate(madElev, 2, 'mean')
agg2terra

# compare fasterRaster and terra... should be the same
agg2 <- rast(agg2)
agg2 <- extend(agg2, agg2terra)
agg2 - agg2terra

### aggregate by a factor of 2.5 in 2 dimensions
# fasterRaster
agg2.9 <- aggregate(elev, 2.9, 'mean')
agg2.9

# terra
agg2.9terra <- aggregate(madElev, 2.9, 'mean')
agg2.9terra

# compare fasterRaster and terra... should be different
res(agg2.9)
res(agg2.9terra) # terra rounds aggregation factor down
2 * res(madElev) # original resolution multipled by 2

# aggregate by different factor in 2 dimensions
agg2x3 <- aggregate(elev, c(2, 3), 'mean')
agg2x3

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
