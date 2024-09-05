if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)

### Calculate input rasters
###########################

# Values below are just a guess
coeff_bh <- coeff_dh <- elev
coeff_bh[] <- 0.4
coeff_dh[] <- 0.6

slope <- terrain(elev, "slope")
aspect <- terrain(elev, "aspect", northIs0 = FALSE)

horizon_step <- 90
hh <- horizonHeight(elev, step = horizon_step, northIs0 = FALSE)

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
	linke = 1.5,
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
	insol_time = TRUE,

	lowMemory = FALSE
)

solar

}
