if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)

### Calculate input rasters
###########################

coeff_bh <- elev * 0 + 0.4 # just a guess
coeff_dh <- elev * 0 + 0.6 # just a guess

slope <- terrain(elev, "slope")
aspect <- terrain(elev, "aspect", northIs0 = FALSE)

horizon_step <- 90
directions <- seq(0, 359, horizon_step)
hh <- horizonHeight(elev, directions = directions, northIs0 = FALSE)

### calculate solar ir/radiance
###############################

solar <- sun(
	elevation = elev,
	coeff_bh = coeff_bh,
	coeff_dh = coeff_dh,
	slope = slope,
	aspect = aspect,
	hh = hh,
	horizon_step = horizon_step,
	albedo = 0.2,
	linke = 3,
	day = 1,
	step = 0.5,
	declination = NULL,
	solar_constant = 1367,
	
	distance_step = 1,
	npartitions = 1,

	beam_rad = TRUE,
	diff_rad = TRUE,
	refl_rad = TRUE,
	glob_rad = TRUE,
	insol_time = TRUE
)

solar

}
