\dontrun{

# change this according to where GRASS is installed on your system
# grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
# grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(terra)
madElev <- fasterData('madElev')

probs <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
quants <- fasterQuantile(rast=madElev, probs=probs, grassDir=grassDir)
quants

# in this case, the raster package is faster:
quantile(madElev, probs=probs, na.rm=TRUE)

}
