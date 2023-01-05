\dontrun{

library(terra)

# change this according to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(terra)

madChelsa <- fasterData('madChelsa') # unprojected
madElev <- fasterData('madElev') # projected

# coordinate reference systems are different!
cat(crs(madChelsa), '\n')
cat(crs(madElev), '\n')

madChelsa <- subset(madChelsa, 1:3) # subset to speed this up

madChelsaProj <- fasterProjectRast(rast=madChelsa,
template=madElev, grassDir=grassDir)

oldPar <- par(mfrow=c(1, 2))
plot(madChelsa[[1]], main='Original')
plot(madChelsaProj[[1]], main='Resampled')
par(oldPar)

}
