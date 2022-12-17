\donttest{

# change this according to where GRASS is installed on your system
# grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
# grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(terra)

madElev <- fasterData('madElev')

# could also use terrain() which may be faster
# in this example
topo <- fasterTerrain(rast=madElev, slope=TRUE, aspect=TRUE,
grassDir=grassDir)

# terrain function from the raster package... much slower in this example
slp <- terrain(elev, opt='slope', unit='degrees')
asp <- terrain(elev, opt='aspect', unit='degrees')
topos <- stack(slp, asp)
names(topos) <- c('slope', 'aspect')
plot(topos)

}
