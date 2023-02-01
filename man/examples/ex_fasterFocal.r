\dontrun{

# IMPORTANT: These function use the "location", "restartGrass", and
# "warn" arguments to avoid interfering with an existing GRASS session.
# WHEN YOU ARE DONE WITH THE EXAMPLES, run this line to revert to your
# active GRASS session:
# initGrass(location='default') # change "location" if not "default"

# IMPORTANT: Change this to where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

library(sf)
library(terra)

madElev <- fasterData('madElev')

### mean of square neighborhood with all cells of equal weight
# Package terra is faster for small neighborhoods, but fasterRaster is faster
# for large neighborhoods (w > ~100 in this example) if cores is ~4 or more.

square <- fasterFocal(madElev, w=3, fun='mean', cores=2, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

terraSquare <- terra::focal(madElev, w=3, fun='mean', na.rm=TRUE)

# same!
global(square, 'sum', na.rm=TRUE) - global(terraSquare, 'sum', na.rm=TRUE)


### maximum of square neighborhood with all cells of equal weight with a mask
# Using a mask replaces non-NA values in the mask with new values. Cells
# with NA retain their original value from the input raster.
madForest2000 <- fasterData('madForest2000')

maxNoMask <- fasterFocal(madElev, w=5, fun='maximum',
cores=2, grassDir=grassDir, outGrassName='focalUnMasked',
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

maxMasked <- fasterFocal('madElev', w=5, fun='maximum', mask=madForest2000,
cores=2, grassDir=grassDir, outGrassName='focalMasked',
location='examples') # line for examples only

plot(maxNoMask - maxMasked)


### focal mean of circular neighborhood (with all cells of equal weight)
circle <- fasterFocal(madElev, w=3, fun='max', circle=TRUE, cores=2,
outGrassName='circleFocal', grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

square <- fasterFocal('madElev', w=3, fun='max', cores=2, 
outGrassName='squareFocal', grassDir=grassDir,
location='examples')

plot(square - circle)


### focal mean of square neighborhood with cells of arbitrary weight
w <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow=3)
w

wgt <- fasterFocal(madElev, w=w, fun='mean', cores=2, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

terraWgt <- terra::focal(madElev, w=w, fun='mean', na.rm=TRUE)

# same
global(wgt, 'sum', na.rm=TRUE) - global(terraWgt, 'sum', na.rm=TRUE)

### population standard deviation vs standard deviation
popSd <- fasterFocal(madElev, w=3, fun='popSd', cores=2,
outGrassName='popSdFocal', grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

sampSd <- fasterFocal('madElev', w=3, fun='sd', cores=2,
outGrassName='sampSdFocal', grassDir=grassDir,
location='examples') # line for examples only

plot(sampSd - popSd)

# vs terra's default sample sd
terraSampSd <- terra::focal(madElev, w=3, fun='sd', na.rm=TRUE)

plot(sampSd - terraSampSd)

### sum with weighting function
gaussFocal <- fasterFocalDecay(madElev, w=3, fun='sum',
weightFx='gaussian', weightFactor=1, cores=2,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

### standard deviations
sdFocalFR <- fasterFocal(madElev, w=3, fun='sd', cores=2)
sdFocalTerra <- focal(madElev, w=3, fun='sd', na.rm=TRUE)
# also see focalCpp() in terra package

# different!
sdFocalFR
sdFocalTerra

# why different? Because GRASS uses population SD and terra the sample SD.
madElevMat <- as.matrix(madElev, wide = TRUE)
nr <- nrow(madElevMat)
nc <- ncol(madElevMat)

sdMat1 <- sdMat2 <- sdMat3 <- NA * madElevMat
for (row in 2:(nr - 1)) {
  for (col in 2:(nc - 1)) {
    neigh <- madElevMat[(row - 1):(row + 1), (col - 1):(col + 1)]
    numer <- sqrt( sum((neigh - mean(neigh, na.rm=T))^2) )

    denom1 <- sqrt(sum(!is.na(neigh)))
    denom2 <- sqrt(sum(!is.na(neigh)) - 1)

    if (!is.na(denom2)) if (denom2 == 0) denom2 <- NA

    sdMat1[row, col] <- numer / denom1
    sdMat2[row, col] <- numer / denom2
    sdMat3[row, col] <- sd(neigh, na.rm=TRUE)
  }
}

sdRast1 <- rast(sdMat1, crs=crs(madElev), extent=ext(madElev))
sdRast2 <- rast(sdMat2, crs=crs(madElev), extent=ext(madElev))
sdRast3 <- rast(sdMat3, crs=crs(madElev), extent=ext(madElev))

sdRast1 # fasterRaster/ GRASS
sdRast2 # terra
sdRast3 # base R

}
