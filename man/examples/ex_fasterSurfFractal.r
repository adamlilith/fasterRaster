\dontrun{

# change this to where GRASS is installed on your system
# grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
# grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(terra)

madElev <- fasterData('madElev')

fract1 <- fasterSurfFractal(rast=madElev, dimension=2.1, grassDir=grassDir)
fract2 <- fasterSurfFractal(rast='rast', dimension=2.9, grassDir=grassDir)

plot(stack(fract1, fract2))

}
