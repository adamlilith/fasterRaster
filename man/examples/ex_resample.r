if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")
elev <- fast(madElev)

### Resample raster to 120 x 120 m
elev120 <- resample(elev, c(120, 120), method="bilinear")
elev
elev120

### Resample using another raster as a template
###############################################

template <- aggregate(elev, 4)

nearest <- resample(elev, template, method = "nearest")

bilinear <- resample(elev, template, method = "bilinear")
bilinearNoFB <- resample(elev, template, method = "bilinear", fallback = FALSE)

bicubic <- resample(elev, template, method = "bicubic")
bicubicNoFB <- resample(elev, template, method = "bicubic", fallback = FALSE)

lanczos <- resample(elev, template, method = "lanczos")
lanczosNoFB <- resample(elev, template, method = "lanczos", fallback = FALSE)

# rasters resampled without fallback have fewer non-NA cells
resampled <- c(nearest, bilinear, bilinearNoFB, bicubic, bicubicNoFB, lanczos,
    lanczosNoFB)
names(resampled) <- c("nearest", "bilinear", "bilinearNoFB", "bicubic",
    "bicubicNoFB", "lanczos", "lanczosNoFB")
ones <- resampled * 0 + 1
global(ones, "sum") # number of non-NA cells
global(resampled, c("mean", "sd", "min", "max")) # other statistics

# Compare fallback to no fallback
frLanczos <- rast(lanczos)
frLanczosNoFB <- rast(lanczosNoFB)

plot(frLanczos, col = "red",
    main = "Red: Cells in fallback not non-fallback", legend = FALSE)
plot(frLanczosNoFB, add=TRUE)

# Compare fasterRaster with terra
coarserTerra <- aggregate(madElev, 4)
terraLanczos <- resample(madElev, coarserTerra, method = "lanczos")

frLanczos <- extend(frLanczos, terraLanczos)
frLanczosNoFB <- extend(frLanczosNoFB, terraLanczos)

frLanczos - terraLanczos
frLanczosNoFB - terraLanczos

plot(frLanczos - terraLanczos, main = "Difference")
plot(frLanczosNoFB - terraLanczos, main = "Difference")

plot(terraLanczos, col = "red",
    main = "Red: Cells in terra not in FR", legend = FALSE)
plot(frLanczos, add=TRUE)

plot(frLanczos, col = "red",
    main = "Red: Cells in FR not in terra", legend = FALSE)
plot(terraLanczos, add=TRUE)

}
