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
library(terra)

# example data
madElev <- fastData('madElev')
madCoast4 <- fastData('madCoast4')
madCoast4 <- vect(madCoast4)

# for the example, crop the elevation raster to two communes
madAnt <- madCoast4[madCoast4$NAME_4 == 'Antanambe', ]
madMan <- madCoast4[madCoast4$NAME_4 == 'Manompana', ]

elevAnt <- crop(madElev, madAnt)
elevMan <- crop(madElev, madMan)

plot(madElev)
plot(elevAnt, col='red', legend=FALSE, add=TRUE)
plot(elevMan, col='blue', legend = FALSE, add=TRUE)

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster
ant <- fast(elevAnt)
man <- fast(elevMan)

# merge
antMan <- merge(ant, man)
plot(antMan, main='Antman!')

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
