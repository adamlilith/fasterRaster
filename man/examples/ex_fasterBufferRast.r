\dontrun{

library(terra)

# change this to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

madForest2000 <- fasterData('madForest2000')

rastBuff <- fasterBufferRast(rast=madForest2000, width=2,
units='kilometers', grassDir=grassDir)

plot(rastBuff, col=c('green', 'gray'))
legend('topright', legend=c('forestgreen', 'buffer'),
fill=c('green', 'gray'))

}
