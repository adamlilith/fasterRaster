if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madChelsa <- fastData("madChelsa")

# Convert a SpatRaster to a GRaster
chelsa <- fast(madChelsa)
chelsa # 4 layers

# Central tendency
mean(chelsa)
mmode(chelsa)
median(chelsa)

# Statistics
nunique(chelsa)
sum(chelsa)
count(chelsa)
min(chelsa)
max(chelsa)
range(chelsa)
skewness(chelsa)
kurtosis(chelsa)

stdev(chelsa)
stdev(chelsa, pop = FALSE)
var(chelsa)
varpop(chelsa)

# Which layers have maximum/minimum?
which.min(chelsa)
which.max(chelsa)

# Regression
# Note the intercept is different for fasterRaster::regress().
regress(chelsa)
regress(madChelsa)

# Note: To get quantiles for each layer, use
# global(x, "quantile", probs = 0.2).
quantile(chelsa, 0.1)

# NAs
madForest2000 <- fastData("madForest2000")
forest2000 <- fast(madForest2000)
forest2000 <- project(forest2000, chelsa, method = "near")

chelsaForest <- c(chelsa, forest2000)

nas <- anyNA(chelsaForest)
plot(nas)

allNas <- allNA(chelsaForest)
plot(allNas)

}
