\dontrun{

library(sf)
library(terra)

# We will illustrate mosaicking by breaking apart a raster
# then rejoining it.

madElev <- fasterData('madElev')
madCoast4 <- fasterData('madCoast4')

# crop elevation raster to each Commune
ant <- madCoast4[madCoast4$NAME_4 == 'Antanambe', ]
man <- madCoast4[madCoast4$NAME_4 == 'Manompana', ]

plot(st_geometry(ant), border='blue')
plot(st_geometry(man), add=TRUE, border='red')

# split raster
antElev <- crop(madElev, ant)
manElev <- crop(madElev, man)

names(antElev) <- 'antElev'
names(manElev) <- 'manElev'

# make color patelletes for each raster so we can differentiate them
antCol <- colorRampPalette(c('lightblue', 'darkblue'))
manCol <- colorRampPalette(c('pink', 'darkred'))

antCol <- antCol(10)
manCol <- manCol(10)

plot(st_geometry(madCoast4))
plot(antElev, col=antCol, add=TRUE, legend=FALSE)
plot(manElev, col=manCol, add=TRUE, legend=FALSE)

inRastName <- c('ant', 'man')
combo <- fasterMosaic(antElev, manElev, inRastName=inRastName, cores=2)

plot(combo)

}
