if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)
elevs <- c(elev, elev, log10(elev) - 1, sqrt(elev))
names(elevs) <- c("elev1", "elev2", "log_elev", "sqrt_elev")

elev
elevs

# Comparisons
elev < 100
elev <= 100
elev == 100
elev != 100
elev > 100
elev >= 100

elev + 100 < 2 * elev

elevs > 10
10 > elevs

# logic
elev < 10 | elev > 200
elev < 10 | cos(elev) > 0.9

elev < 10 | TRUE
TRUE | elev > 200

elev < 10 | FALSE
FALSE | elev > 200

elev < 10 & cos(elev) > 0.9

elev < 10 & TRUE
TRUE & elev > 200

elev < 10 & FALSE
FALSE & elev > 200

}
