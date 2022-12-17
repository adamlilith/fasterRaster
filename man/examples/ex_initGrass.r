\donttest{

# change this according to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

# start GRASS session with a raster
library(terra)
madElev <- fasterData('madElev')
input <- initGrass(rast=madElev, grassDir=grassDir)

# start GRASS session with a vector
madCoast <- fasterData('madCoast0')
input <- initGrass(vect=madCoast0, grassDir=grassDir)

}

