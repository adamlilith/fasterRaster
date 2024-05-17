if (grassStarted()) {

# Setup
library(sf)
library(terra)

### This example creates a simple model of Dypsis distribution using
# elevation, distance to forest, and land cover class.

# Elevation raster, forest cover in year 2000, land cover class, and
# points where Dypsis plants have been collected
madElev <- fastData("madElev")
madForest2000 <- fastData("madForest2000")
madCover <- fastData("madCover")
madDypsis <- fastData("madDypsis")

# Convert SpatRasters to GRasters and sf vector to GVector:
elev <- fast(madElev)
forest <- fast(madForest2000)
dypsis <- fast(madDypsis)
cover <- fast(madCover)

# Distance to forest
distToForest <- distance(forest, unit = "km")
distToForest <- log1p(distToForest) # log(x + 1) of distance
names(distToForest) <- "distToForest"

# "Stack" elevation and forest cover
land <- c(elev, distToForest)
plot(land)

# Scale continuous predictors to mean of 0 and sd of 1
landScaled <- scale(land)
names(landScaled) <- c("elevation", "distToForest")

# Project land cover raster
coverProj <- project(cover, landScaled)

# Combine continuous/categorical data
covariateRasters <- c(landScaled, coverProj)

### Make model of Dypsis locations:
# Extract elevation and forest cover at Dypsis locations
presEnv <- extract(covariateRasters, dypsis, cats = TRUE)
presEnv$presBg <- 1
head(presEnv)

# Extract elevation and forest cover at background 2000 sites:
bgEnv <- spatSample(covariateRasters, size = 3000, values = TRUE, cats = TRUE)
bgEnv <- bgEnv[stats::complete.cases(bgEnv), ]
bgEnv <- bgEnv[1:2000, ]
bgEnv$presBg <- 0
head(bgEnv)

# Combine presence and background data:
env <- rbind(presEnv, bgEnv)

# Calibrate model:
form <- presBg ~ elevation + distToForest +
I(distToForest^2) + elevation * distToForest +
madCover

model <- stats::glm(form, data = env, family = stats::binomial)
summary(model)

# Make predictions and map:
prediction <- predict(covariateRasters, model, type = "response")
prediction

plot(prediction)
plot(dypsis, pch = 1, add = TRUE)

# Not a great model!

}
