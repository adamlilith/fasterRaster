if (grassStarted()) {

# Setup
library(terra)

# Example data
madChelsa <- fastData("madChelsa")

# Convert a SpatRaster to a GRaster:
chelsa <- fast(madChelsa)

# Correlation
layerCor(chelsa, "cor", na.rm = FALSE) # Pearson correlation
layerCor(chelsa, "cor", cor = "spearman", na.rm = FALSE) # Spearman correlation

# Covariance
layerCor(chelsa, "cov", na.rm = FALSE)

# To illustrate categorical tests, force two layers to be of type integer
chelsa[[1:2]] <- as.int(chelsa[[1:2]])

# Chi-^2 and Cramer's V (integer vs integer)
layerCor(chelsa[[1:2]], "chisq", na.rm = FALSE)

# Kruskal-Wallis test (integer vs continuous)
integerCont <- c(chelsa[[1]], chelsa[[3]])
layerCor(integerCont, "kw", na.rm = FALSE)

# automatic by data type
layerCor(chelsa, "auto", cor = "spear", na.rm = FALSE,
   verbose = TRUE, integerAsNumeric = FALSE)

}
