if (grassStarted()) {

# Setup
library(terra)

# Example data:
madForest <- fastData("madForest2000") # raster

### Fragmentation classes from a SpatRaster
###########################################

# Fragmentation class:
fragTerra <- fragmentation(madForest)
fragTerraMasked <- fragmentation(madForest, restrict = TRUE)

# Compare:
fragTerras <- c(fragTerra, fragTerraMasked)
names(fragTerras) <- c("unrestricted", "restricted")

plot(fragTerras)
levels(fragTerras)
freq(fragTerras)

### Fragmentation classes from a GRaster
########################################

# Convert to GRaster:
forest <- fast(madForest)

# Fragmentation class:
frag <- fragmentation(forest)
fragMasked <- fragmentation(forest, restrict = TRUE)

# Compare:
frags <- c(frag, fragMasked)
names(frags) <- c("unrestricted", "restricted")

plot(frags)
levels(frags)
freq(frags)

}
