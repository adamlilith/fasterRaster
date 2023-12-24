if (grassStarted()) {

# Setup
library(terra)

### This example creates a simple model of Dypsis distribution using
# elevation and distance to forest.

# Elevation raster, forest cover in year 2000, and points where Dypsis plants
# have been collected
madElev <- fastData("madElev")
madForest2000 <- fastData("madForest2000")
madDypsis <- fastData("madDypsis")

# Convert SpatRasters to GRasters and sf vector to GVector:
elev <- fast(madElev)
forest <- fast(madForest2000)
dypsis <- fast(madDypsis)

# Distance to forest
distToForest <- distance(forest, unit = "km")
distToForest <- log1p(distToForest) # log(x + 1) of distance
names(distToForest) <- "distToForest"

# "Stack" elevation and forest cover
land <- c(elev, distToForest)
plot(land)

# Scale predictors to mean of 0 and sd of 1
landScaled <- scale(land)
names(landScaled) <- c("elevation", "distToForest")

### Make model of dypsis locations:
# Extract elevation and forest cover at Dypsis locations
presEnv <- extract(landScaled, dypsis)
presEnv$presBg <- 1
head(presEnv)

# Extract elevation and forest cover at background 1000 sites:
bgEnv <- spatSample(landScaled, size = 1500, values = TRUE)
bgEnv <- bgEnv[stats::complete.cases(bgEnv), ]
bgEnv <- bgEnv[1:1000, ]
bgEnv$presBg <- 0
head(bgEnv)

# Combine presence and background data:
env <- rbind(presEnv, bgEnv)

# Calibrate model:
form <- presBg ~ elevation + distToForest +
I(distToForest^2) + elevation * distToForest

model <- stats::glm(form, data = env, family = stats::binomial)

summary(model)

# Make predictions and map:
prediction <- predict(landScaled, model, type = "response")
prediction

plot(prediction)
plot(dypsis, pch = 1, add = TRUE)

# Not a great model!

}
