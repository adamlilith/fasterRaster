\dontrun{

# change this to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(terra)
madElev <- fasterData('madElev')

elevHeight_deg <- fasterHorizon(madElev, units='degrees', grassDir=grassDir)

plot(elevHeight_deg)

}
