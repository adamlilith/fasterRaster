if (grassStarted()) {

# Setup
madForest2000 <- fastData("madForest2000")
madForest2014 <- fastData("madForest2014")

# Convert SpatRasters to GRasters:
forest2000 <- fast(madForest2000)
forest2014 <- fast(madForest2014)

# Combine:
forest <- c(forest2000, forest2014)
forest

nlyr(forest)

}
