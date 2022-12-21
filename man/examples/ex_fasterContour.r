\dontrun{

library(terra)

# change this to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

madElev <- fasterData('madElev')
conts <- fasterContour(madElev, levels=c(100, 200, 300, 400, 500),
grassDir=grassDir)

plot(madElev)
plot(conts, add=TRUE)

}

