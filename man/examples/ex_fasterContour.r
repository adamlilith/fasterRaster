\donttest{

library(terra)

# change this to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

madElev <- fasterData('madElev')

conts1 <- fasterContour(madElev, grassDir=grassDir)
conts2 <- fasterContour(madElev, levels=c(0, 50, 100, 500),
grassDir=grassDir)

conts3 <- fasterContour(madElev, minlevel=0, maxlevel=500, step=250,
grassDir=grassDir)

oldPar <- par(mfrow=c(1, 3))
plot(madElev)
plot(conts1, add=TRUE)
plot(madElev)
plot(conts2, add=TRUE)
plot(madElev)
plot(conts3, add=TRUE)
par(oldPar)

}

