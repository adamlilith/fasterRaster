if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Points, lines, and polygons
madDypsis <- fastData("madDypsis")
madRivers <- fastData("madRivers")
madCoast4 <- fastData("madCoast4")

# Convert to  GVectors:
dypsis <- fast(madDypsis)
rivers <- fast(madRivers)
coast4 <- fast(madCoast4)

# Point centroids:
dypMean <- centroids(dypsis, fail = FALSE)
dypMedian <- centroids(dypsis, method = "median", fail = FALSE)
dypPMedian <- centroids(dypsis, method = "pmedian", fail = FALSE)

plot(dypsis)
plot(dypMean, col = "red", add = TRUE)
plot(dypMedian, col = "green", pch = 2, add = TRUE)
plot(dypPMedian, col = "orange", pch = 1, add = TRUE)
legend("bottomright",
   legend = c("mean", "median", "pmedian"),
   col = c("red", "green", "orange"),
   pch = c(16, 2, 1),
   xpd = NA
)

# Line centroids:
riversMid <- centroids(rivers, fail = FALSE)
riversMean <- centroids(rivers, method = "mean", fail = FALSE)
riversMedian <- centroids(rivers, method = "median", fail = FALSE)

plot(rivers)
plot(riversMid, col = "red", add = TRUE)
plot(riversMean, col = "green", pch = 2, add = TRUE)
plot(riversMedian, col = "orange", pch = 1, add = TRUE)
legend("bottomright",
   legend = c("mid", "mean", "median"),
   col = c("red", "green", "orange"),
   pch = c(16, 2, 1),
   xpd = NA
)

# Polygon centroids:
coast4Mean <- centroids(coast4, fail = FALSE)
coast4Median <- centroids(coast4, method = "median", fail = FALSE)
coast4BMedian <- centroids(coast4, method = "bmedian", fail = FALSE)

plot(coast4)
plot(coast4Mean, col = "red", add = TRUE)
plot(coast4Median, col = "green", pch = 2, add = TRUE)
plot(coast4Median, col = "orange", pch = 1, add = TRUE)
legend("bottomright",
   legend = c("mean", "median", "bmedian"),
   col = c("red", "green", "orange"),
   pch = c(16, 2, 1),
   xpd = NA
)

}
