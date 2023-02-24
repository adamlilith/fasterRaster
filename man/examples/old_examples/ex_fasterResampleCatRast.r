\dontrun{

# change this according to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(terra)

# Import a categorical raster (forest cover). Note that this also reprojects
# the forest raster.

madForest2000 <- fasterData('madForest2000')
madChelsa <- fasterData('madChelsa')


# new resolution is user-defined (1000 x 1000 m)
coarseResamp <- fasterResampleCatRast(madForest2000, ewres=1000, nsres=1000,
grassDir=grassDir)

res(madForest2000)
res(coarseResamp)

oldPar <- par(mfrow=c(1, 2))
plot(madForest2000, col='forestgreen', main='Original, 30-m resolution')
plot(coarseResamp, col='forestgreen', main='Resampled, 1-km resolution')
par(oldPar) # revert to previous plot settings

# new resolution is from another raster
!!! FIX ME!!!
fineResamp <- fasterResampleCatRast(madForest2000, template=madChelsa,
grassDir=grassDir)

res(madForest2000)
res(fineResamp)

oldPar <- par(mfrow=c(1, 2))
plot(madForest2000, col='forestgreen', main='Original, 30-m resolution')
plot(fineResamp, col='forestgreen', main='Resampled, ~1-km resolution')
par(oldPar) # revert to previous plot settings

}

