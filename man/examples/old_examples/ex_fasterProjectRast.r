\dontrun{

# IMPORTANT: These function use the "location", "restartGrass", and
# "warn" arguments to avoid interfering with an existing GRASS session.
# WHEN YOU ARE DONE WITH THE EXAMPLES, run this line to revert to your
# active GRASS session:
# initGrass(location='default') # change "location" if not "default"

# IMPORTANT: Change this to where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

library(sf)
library(terra)

# These examples project and resample a "stack" of 4 rasters (madChelsa).

### project a raster using fasterProjectRast()
madChelsa <- fasterData('madChelsa') # unprojected
madElev <- fasterData('madElev') # projected

# Coordinate reference systems are different!
crs(madChelsa)
crs(madElev)

# project
chelsaProj <- fasterProjectRast(madChelsa, madElev, grassDir=grassDir,
fromLocation='exampleFrom', location='examples',
restartGrass=TRUE, warn=FALSE) # last two line for examples only

# CRSs are now the same!
crs(madElev)
crs(madChelsaProj)

# Results are numerically somewhat different from terra's project() function.
terraProj <- project(madChelsa, madElev)
minmax(terraProj)
minmax(chelsaProj)

# extents are different at the decimal place
ext(terraProj)
ext(chelsaProj)

# ... so we can't do like: c(terraProj, chelsaProj) # produces error

# BTW, in this example, projecting madElev to the CRS of madChelsa
# produces a raster with similar discrepancies between values but
# with the same exact extent.

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
