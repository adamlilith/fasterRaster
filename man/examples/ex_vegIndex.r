if (grassStarted()) {

# Setup
library(terra)

# Elevation raster, rivers vector
madLANDSAT <- fastData("madLANDSAT")

# Convert a SpatRaster to a GRaster:
landsat <- fast(madLANDSAT)

# Normalized Difference Vegetation Index and Enhanced Vegetation Index:
indices <- c("ndvi", "evi")
vi <- vegIndex(landsat, index = indices, r = 1, b = 3, nir = 4, bits = 8)

plot(vi)

# All indices using R and NIR:
rnir <- vegIndex(landsat, index = c("rnir"), r = 1, nir = 4, bits = 8)

# Note: Some values are highly skewed, likely due to cloud cover and other
# anomalies that should be corrected.
plot(rnir)

}
