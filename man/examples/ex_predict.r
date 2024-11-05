if (grassStarted()) {

# Setup
library(sf)
library(terra)

### This example creates a simple model of Dypsis distribution using
# elevation, distance to forest, land cover class, and nearness to rivers.

# Elevation raster, forest cover in year 2000, land cover class, and
# points where Dypsis plants have been collected
madElev <- fastData("madElev")
madForest2000 <- fastData("madForest2000")
madCover <- fastData("madCover")
madRivers <- fastData("madRivers")
madDypsis <- fastData("madDypsis")

# Convert SpatRasters to GRasters and sf vectors to GVectors:
elev <- fast(madElev)
forest <- fast(madForest2000)
cover <- fast(madCover)
rivers <- fast(madRivers)
dypsis <- fast(madDypsis)

# Distance to forest
distToForest <- distance(forest, unit = "m")
distToForest <- log1p(distToForest) # log(x + 1) of distance
names(distToForest) <- "distToForest"

# "Stack" elevation and forest cover
continuous <- c(elev, distToForest)

# Scale continuous predictors to mean of 0 and sd of 1
continuousScaled <- scale(continuous)
names(continuousScaled) <- c("elevation", "distToForest")

# Project land cover raster
coverProj <- project(cover, continuousScaled)

# Near a river?
riverBuffer <- buffer(rivers, 5000)
nearRiver <- rasterize(riverBuffer, elev, background = 0)
names(nearRiver) <- "nearRiver"
levels(nearRiver) <- data.frame(value = 0:1, label = c("far", "near"))

# Combine continuous/categorical data
covariateRasters <- c(continuousScaled, coverProj, nearRiver)
plot(covariateRasters)

# Extract environmental values at Dypsis locations:
presEnv <- extract(covariateRasters, dypsis, cats = TRUE)
presEnv$presBg <- 1
head(presEnv)

# Extract elevation and forest cover at 2000 background sites:
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
  madCover + nearRiver

model <- stats::glm(form, data = env, family = stats::binomial)
summary(model)

# Make predictions and map:
prediction <- predict(covariateRasters, model, type = "response")
prediction

# Not a great model!
plot(prediction)
plot(dypsis, pch = 1, add = TRUE)

}
