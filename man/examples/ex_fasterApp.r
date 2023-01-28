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

# NOTE: Some operations do not work using the "madElev" raster as-is
# because it was saved with values in interger form. To address this,
# we modify the raster so it does not have the integer type. The operation
# below changes the type of the raster's values without changing the
# values.

# change data type to float
madElev <- fasterData('madElev')
madElev <- madElev + 0.1
madElev <- madElev - 0.1

expression <- '= madElev^2'
madElevSq <- fasterApp(madElev, expression=expression, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(c(madElev, madElevSq))

# We can also force the value type in GRASS using float():
madElev <- fasterData('madElev') # refresh

expression <- '= float((madElev)^2)'
madElevSq <- fasterApp(madElev, expression=expression, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples

plot(c(madElev, madElevSq))

# force all values to 17
expression <- '= 17'
rast17 <- fasterApp(madElev, expression=expression, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(c(madElev, rast17))

# low-pass filter
madElev <- madElev + 0.1
madElev <- madElev - 0.1

expression <- '= (madElev[-1, -1] + madElev[-1, 0] + madElev[-1, 1] +
madElev[0, -1] + madElev[0, 0] + madElev[0, 1] + madElev[1, -1] +
madElev[1, 0] + madElev[1, 1]) / 9'

expression <- gsub('[\r\n]', '', expression) # remove line breaks

lp <- fasterApp(madElev, expression=expression, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(c(madElev, lp))

# high-pass filter
expression <- '= -0.7 * madElev[-1, -1] -1 * madElev[-1, 0] -
0.7 * madElev[-1, 1] -1 * madElev[0, -1] + 6.8 * madElev[0, 0] -
1 * madElev[0, 1] -0.7 * madElev[1, -1] -1 * madElev[1, 0] -
0.7 * madElev[1, 1]'

expression <- gsub('[\r\n]', '', expression) # remove line breaks

hp <- fasterApp(madElev, expression=expression, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(c(madElev, hp))

# Using multi-layer rasters:
madChelsa <- fasterData('madChelsa')
expression <- '= bio1 / bio12'

div <- fasterApp(madChelsa, expression=expression, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(div)

# Revert back to original GRASS session.
# Change to your working location if not "default" (it usually is).
initGrass(location='default')

}
