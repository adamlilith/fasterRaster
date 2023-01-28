\dontrun{

library(sf)
library(terra)

madElevAnt <- fasterData('madElevAnt')
madElevMan <- fasterData('madElevMan')
madCoast0 <- fasterData('madCoast0')

# make different color palettes for each raster so we can differentiate them
antCol <- colorRampPalette(c('lightblue', 'darkblue'))
manCol <- colorRampPalette(c('pink', 'darkred'))

antCol <- antCol(10)
manCol <- manCol(10)

plot(st_geometry(madCoast0))
plot(madElevAnt, col=antCol, add=TRUE, legend=FALSE)
plot(madElevMan, col=manCol, add=TRUE, legend=FALSE)

inRastName <- c('ant', 'man') # cool!
combo <- fasterMosaic(madElevAnt, madElevMan, inRastName=inRastName, cores=2)

plot(combo)

}
