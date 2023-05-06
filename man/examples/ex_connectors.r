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
library(sf)

# rivers vector and locations of Dypsis plants
madRivers <- fastData('madRivers')
madDypsis <- fastData('madDypsis')

# start GRASS session for examples only
wd <- forwardSlash(tempdir())

fastStart(crs = madRivers, grassDir = grassDir,
workDir = wd, location = 'examples') # line only needed for examples

# convert to GVectors
rivers <- fast(madRivers)
dypsis <- fast(madDypsis)

### connections from each point to nearest river
consFromDypsis <- connectors(dypsis, rivers)
consFromDypsisVect <- vect(consFromDypsis)

plot(st_geometry(madDypsis))
plot(st_geometry(madRivers), col = 'blue', add = TRUE)
plot(consFromDypsisVect, add = TRUE)

### connections from each river to nearest point
consFromRivers <- connectors(rivers, dypsis)
consFromRiversVect <- vect(consFromRivers)

plot(st_geometry(madDypsis))
plot(st_geometry(madRivers), col = 'blue', add = TRUE)
plot(consFromRiversVect, add = TRUE)

# IMPORTANT #3: Revert back to original GRASS session if needed.
sessionRestore(opts.)
removeSession('examples')

}
