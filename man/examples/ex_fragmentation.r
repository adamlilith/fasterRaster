if (grassStarted()) {

# Setup
library(terra)

# Example data:
madForest <- fastData("madForest2000") # raster

### Fragmentation classes from a SpatRaster
###########################################

fragTerra <- fragmentation(madForest)
plot(fragTerra)
levels(fragTerra)
freq(fragTerra)

### Fragmentation classes from a GRaster
########################################

# Convert to GRaster:
forest <- fast(madForest)

# Fragmentation class:
frag <- fragmentation(forest)
plot(frag)
levels(frag)
freq(frag)

}
