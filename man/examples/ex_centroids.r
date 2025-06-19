if (grassStarted()) {

# Setup
library(sf)
library(terra)

### Points, line, and polygon centroids

# Point centroids:
madDypsis <- fastData("madDypsis")
dypsis <- fast(madDypsis)

dypMean <- centroids(dypsis)
dypMedian <- centroids(dypsis, method = "median")
dypPMedian <- centroids(dypsis, method = "pmedian")

plot(dypsis)
plot(dypMean, col = "red", add = TRUE)
plot(dypMedian, col = "green", pch = 2, add = TRUE)
plot(dypPMedian, col = "blue", pch = 3, add = TRUE)
legend("bottomright",
   legend = c("mean", "median", "pmedian"),
   col = c("red", "green", "blue"),
   pch = c(16, 2, 3),
   xpd = NA
)

# Line centroids:
madRivers <- fastData("madRivers")
rivers <- fast(madRivers)

riversMid <- centroids(rivers)
riversMean <- centroids(rivers, method = "mean")
riversMedian <- centroids(rivers, method = "median")

plot(rivers)
plot(riversMid, col = "red", add = TRUE)
plot(riversMean, col = "green", pch = 2, add = TRUE)
plot(riversMedian, col = "blue", pch = 3, add = TRUE)
legend("bottomright",
   legend = c("mid", "mean", "median"),
   col = c("red", "green", "blue"),
   pch = c(16, 2, 3),
   xpd = NA
)

# Polygon centroids:
madCoast4 <- fastData("madCoast4")
coast4 <- fast(madCoast4)

coastMean <- centroids(coast4)
coastMedian <- centroids(coast4, method = "median")
coastBMedian <- centroids(coast4, method = "bmedian")

plot(coast4)
plot(coastMean, col = "red", add = TRUE)
plot(coastMedian, col = "green", pch = 2, add = TRUE)
plot(coastBMedian, col = "blue", pch = 3, add = TRUE)
legend("bottomright",
   legend = c("mean", "median", "bmedian"),
   col = c("red", "green", "blue"),
   pch = c(16, 2, 1),
   xpd = NA
)

### Centroids of integer GRaster "clumps"

# Load elevation raster
madElev <- fastData("madElev")
elev <- fast(madElev)

# Create clumps of similarly-valued cells
clumps <- clump(elev, minDiff = 0.01, minClumpSize = 1000)

# Centroids:
clumpCents <- centroids(clumps)
clumpCents

plot(clumps)
plot(clumpCents, add = TRUE)


}
