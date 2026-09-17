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

template <- aggregate(elev, 4)

nearest <- resample(elev, template, method = "nearest")

bilinear <- resample(elev, template, method = "bilinear")
bilinearNoFB <- resample(elev, template, method = "bilinear", fallback = FALSE)

bicubic <- resample(elev, template, method = "bicubic")
bicubicNoFB <- resample(elev, template, method = "bicubic", fallback = FALSE)

# lanczos <- resample(elev, template, method = "lanczos")
# lanczosNoFB <- resample(elev, template, method = "lanczos", fallback = FALSE)

# rasters resampled without fallback have fewer non-NA cells
resampled <- c(nearest, bilinear, bilinearNoFB, bicubic, bicubicNoFB)
names(resampled) <- c("nearest", "bilinear", "bilinearNoFB", "bicubic",
    "bicubicNoFB")
ones <- resampled * 0 + 1
nonnacell(ones) # number of non-NA cells
global(resampled, c("mean", "sd", "min", "max")) # other statistics

# Compare fasterRaster with terra
terraTemplate <- aggregate(madElev, 4)
terraBicubic <- resample(madElev, terraTemplate, method = "cubic")

frBicubicNoFB <- extend(bicubicNoFB, terraTemplate)

terraBicubic <- fast(terraBicubic)
delta <- frBicubicNoFB - terraBicubic

plot(terraBicubic, col = 'red', legend = FALSE)
plot(delta, add = TRUE)

}
