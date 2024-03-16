if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Focal calculations:
sums <- focal(elev, fun = "sum")
means <- focal(elev, fun = "mean")

# Focal calculations on a circular window:
sds <- focal(elev, fun = "sd") # square
sdsCircle <- focal(elev, fun = "sd", circle = TRUE) # circle

sds
sdsCircle

plot(sds - sdsCircle)

# Focal calculations with user-defined weights:
w <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1), ncol = 3)
w
sumsWeighted <- focal(elev, fun = "sum", w = w)

s <- c(sums, sumsWeighted)
minmax(s)

}
