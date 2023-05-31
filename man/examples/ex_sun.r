\dontrun{
# NB This example is in a "dontrun{}" block because it requires users to have
# GRASS GIS Version 8+ installed on their system.

# IMPORTANT #1: If you already have a GRASS session started, you will need to
# run the line below and the last line in this example to work with it again.
# If you have not started a GRASS session, you can skip this step and go to
# step #2.
opts. <- getFastOptions()

# IMPORTANT #2: Select the appropriate line below and change as necessary to
# where GRASS is installed on your system.

grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

# setup
library(terra)

# example data
madElev <- fastData('madElev')

# start GRASS session for examples only
faster(grassDir = grassDir, crs = madElev,
workDir = tempdir(), location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster
elev <- fast(madElev)

### calculate input rasters
coeff_bh <- elev * 0 + 0.4 # just a guess
coeff_dh <- elev * 0 + 0.6 # just a guess

slope <- terrain(elev, 'slope')
aspect <- terrain(elev, 'aspect', northIs0 = FALSE)

horizon_step <- 90
directions <- seq(0, 359, horizon_step)
hh <- horizonHeight(elev, directions = directions, northIs0 = FALSE)

### calculate solar ir/radiance
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

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
