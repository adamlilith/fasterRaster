# Make predictions from a linear or generalized linear model to a GRaster

This version of the `predict()` function make predictions to a set of
`GRaster`s from a model object.

The model must be either a linear model, which is of class `lm` and
typically created using the
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) function or a
generalized linear model (GLM), which is class `glm` and typically
created using [`stats::glm()`](https://rdrr.io/r/stats/glm.html). Other
packages can also create `lm` or `glm` objects, but they may not work in
this function. For example, generalized additive models, which can be
created using the `gam()` function in the **mgcv** package, inherit the
`glm` class, but cannot be used in this function. However, `glm` objects
created with the **speedglm** package should work with this function.

This `predict()` function can handle:

- Linear predictors and intercepts like `1 + x`;

- Quadratic terms like `x^2` (or, in **R** formula notation, `I(x^2)`);

- Two-way interaction terms between scalars like `x1:x2` and `x1 * x2`;

- Categorical predictors (i.e., categorical `GRaster`s; see
  [`vignette("GRasters", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/GRasters.md));

- Two-way interactions between a categorical predictor and a scalar
  predictor; and

- Two-way interactions between categorical predictors.

## Usage

``` r
# S4 method for class 'GRaster'
predict(object, model, type = "response")
```

## Arguments

- object:

  A `GRaster` with one or more layers.

- model:

  An `lm` or `glm` model object.

- type:

  Character: Type of prediction to make. This can be either `link`
  (default; predictions are made on the scale of the link function) or
  `response` (predictions are made on the scale of the response
  variable). This function can only make predictions on the scale of the
  response for the identity, logit, log, or cloglog (complementary
  log-log) link functions.

## Value

A `GRaster`.

## See also

[`terra::predict()`](https://rspatial.github.io/terra/reference/predict.html);
[`stats::predict()`](https://rdrr.io/r/stats/predict.html)

## Examples

``` r
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
plot(prediction, main = "Predicted")
plot(dypsis, pch = 1, add = TRUE)

}
```
