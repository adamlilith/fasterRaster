\dontrun{

# IMPORTANT: These function use the "location", "restartGrass", and
# "warn" arguments to avoid interfering with an existing GRASS session.
# WHEN YOU ARE DONE WITH THE EXAMPLES, run this line to revert to your
# active GRASS session:
# startFaster(location='default') # change "location" if not "default"

# IMPORTANT: Change this to where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

library(sf)
library(terra)

# forest cover raster
madForest2000 <- fasterData('madForest2000')

# default is to produce output like terra
defaultBuff <- fasterBufferRast(madForest2000, width=2000, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

terraBuff <- buffer(madForest2000, width=2000)

plot(c(terraBuff, defaultBuff))

# GRASS default output
grassBuff <- fasterBufferRast(madForest2000, width=2000, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples

plot(grassBuff, col=c('gray', 'forestgreen'))
legend('topright', legend=c('buffered', 'buffer'),
fill=c('gray', 'forestgreen'))

# just the buffer output
buffOnly <- fasterBufferRast(madForest2000, width=2000, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(buffOnly, col='forestgreen')
legend('topright', legend=c('buffer only'),
fill='forestgreen')

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
