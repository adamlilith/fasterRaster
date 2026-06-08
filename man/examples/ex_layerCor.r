if (grassStarted()) {

# Setup
library(terra)

# Example data
madChelsa <- fastData("madChelsa")

# Convert a SpatRaster to a GRaster:
chelsa <- fast(madChelsa)

# For categorical tests, force two layers to be of type integer
chelsa[[1:2]] <- as.int(chelsa[[1:2]])

# Correlation
layerCor(chelsa, "cor") # Pearson correlation
layerCor(chelsa, "cor", cor = "spearman") # Spearman correlation

# Covariance
layerCor(chelsa, "cov")

# Chi-^2 and Cramer's V (integer vs integer)
layerCor(chelsa[[1:2]], "chisq")

# Kruskal-Wallis test (integer vs continuous)
layerCor(chelsa[[c(1, 3)]], "kw")

# automatic by data type
layerCor(chelsa, "auto", cor = "spear", verbose = TRUE)

}
