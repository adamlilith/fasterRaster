\dontrun{

library(terra)

# change this to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

madForest2000 <- fasterData('madForest2000')

# default is same as terra's buffer() function
rastBuff <- fasterBufferRast(rast=madForest2000, width=2,
units='kilometers', grassDir=grassDir)

plot(rastBuff, col='forestgreen')
legend('topright', legend=c('buffered + buffer'),
fill='forestgreen')

# GRASS default
rastBuffGrass <- fasterBufferRast(rast=madForest2000, width=2,
units='kilometers', out='grass', grassDir=grassDir)

plot(rastBuffGrass, col=c('gray', 'forestgreen'))
legend('topright', legend=c('buffered', 'buffer'),
fill=c('gray', 'forestgreen'))

# just the buffer
rastBuffBuff <- fasterBufferRast(rast=madForest2000, width=2,
units='kilometers', out='buffer', grassDir=grassDir)

plot(rastBuffBuff, col='forestgreen')
legend('topright', legend=c('buffer only'),
fill='forestgreen')

}
