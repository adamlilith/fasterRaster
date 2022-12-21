\dontrun{

library(terra)

### mean
madElev <- fasterData('madElev')
meanFocalFR <- fasterFocal(madElev, w=3, fun='mean', cores=2)
meanFocalTerra <- terra::focal(madElev, w=3, fun='mean', na.rm=TRUE)

# same!
meanFocalFR
meanFocalTerra

### mean with circular neighborhood
meanCircleFR <- fasterFocal(madElev, w=3, fun='mean', circle=TRUE,
outGrassName='circle', cores=2)

w <- matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0), nrow=3)
meanCircleTerra <- terra::focal(madElev, w=w, fun='mean', na.rm=TRUE)

# same!
meanCircleFR
meanCircleTerra

### using weights matrix
w <- matrix(c(0.5, 1, 0.5, 1, 2, 1, 0.5, 1, 0.5), nrow=3)
w <- w / sum(w) # normalize
meanWeightsFR <- fasterFocal(madElev, w=w, fun='sum', cores=2)
meanWeightsTerra <- terra::focal(madElev, w=w, fun='sum', na.rm=TRUE)

# same!
meanWeightsFR
meanWeightsTerra

# Using a "high-pass" weights matrix:
w <- matrix(c(-1, -1, -1, -1, 8, -1, -1, 1, -1) / 9, nrow=3)
hpFilter <- fasterFocal(madElev, w=w, fun='sum', cores=2)

### sum with mask
mask <- fasterData('madForest2000')
maskFocal <- fasterFocal(madElev, w=3, fun='sum', mask=mask, cores=2)

### sum with weighting function
gaussFocal <- fasterFocal(madElev, w=3, fun='sum', weightFx='gaussian',
weightFactor=1, cores=2)

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
