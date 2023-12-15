if (grassStarted()) {

# Setup
library(terra)

# Climate raster:
madChelsa <- fastData("madChelsa")

# Convert a SpatRaster to a GRaster:
chelsa <- fast(madChelsa)

### Denoise:
quiet <- denoise(chelsa, scale = TRUE)

compare1 <- c(chelsa[["bio1"]], quiet[["bio1"]])
plot(compare1)

compare2 <- c(chelsa[["bio7"]], quiet[["bio7"]])
plot(compare2)

### Noise:
loud <- noise(chelsa, scale = TRUE)

compare1 <- c(chelsa[["bio1"]], loud[["bio1"]])
plot(compare1)

compare2 <- c(chelsa[["bio7"]], loud[["bio7"]])
plot(compare2)

}
