if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madCoast4 <- fastData("madCoast4")

### aggregating a GRaster
#########################

# Convert:
elev <- fast(madElev)

### Aggregate GRaster by same factor in 2 dimensions
# fasterRaster
agg2 <- aggregate(elev, 2, "mean")
agg2

# Compare rasters aggregated by fasterRaster and terra.
# These should be the same.
agg2terra <- aggregate(madElev, 2)

agg2 <- rast(agg2)
agg2 <- extend(agg2, agg2terra)
agg2 - agg2terra # value is ~0

### Aggregate GRaster by a non-integer factor in 2 dimensions
# fasterRaster
agg2.9 <- aggregate(elev, 2.9, "mean")
agg2.9

# terra
agg2.9terra <- aggregate(madElev, 2.9, "mean")
agg2.9terra

# Compare rasters aggregated by fasterRaster and terra.
# These should be different.
res(agg2.9)
res(agg2.9terra) # terra rounds aggregation factor down
2 * res(madElev) # original resolution multiplied by 2

### Aggregate GRaster by different factor in 2 dimensions
agg2x3 <- aggregate(elev, c(2, 3), "mean")
agg2x3

### aggregating a GVector
#########################

madCoast4 <- fastData("madCoast4")

# Convert:
coast4 <- fast(madCoast4)

# Aggregate and disaggregate:
aggCoast <- aggregate(coast4)
disaggCoast <- disagg(coast4)

ngeom(coast4)
ngeom(aggCoast)
ngeom(disaggCoast)

# plot
oldpar <- par(mfrow = c(1, 3))
plot(coast4, main = "Original", col = 1:nrow(coast4))
plot(aggCoast, main = "Aggregated", col = 1:nrow(aggCoast))
plot(disaggCoast, main = "Disaggregated", col = 1:nrow(disaggCoast))
par(oldpar)

}
