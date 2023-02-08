\dontrun{

# change this to where GRASS 8 is installed on your system
# grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
# grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

# NB This examples uses "chaining" of a GRASS session between faster
# functions. That is, rasters are created in GRASS then kept in GRASS
# (not exported back to R). This way, the functions do not have to restart
# GRASS and do not need to copy rasters to/from R until the end.

library(terra)

# elevation raster
madElev <- fasterData('madElev')

# initialize a GRASS session with the elevation raster
startFaster(rast=madElev, inRastName='elevation',
grassDir=grassDir)

# beam and diffuse radiation fractions... assuming they are both 1
# (must be between 0 an 1)
coeff_bh <- coeff_dh <- madElev * 0 + 1
fasterRast(coeff_bh, grassName='coeff_bh')
fasterRast(coeff_dh, grassName='coeff_dh')

# calculate horizon height rasters... to reduce time just using 90 deg steps
fasterHorizon('elevation', units='radians', directions=c(0, 90, 180, 
270), northIs0=FALSE, grassDir=grassDir, grassToR = FALSE)

# terrain rasters
fasterTerrain('elevation', slope=TRUE, aspect=TRUE, northIs0=FALSE,
grassDir=grassDir, grassToR=FALSE)

# Assuming albedo and Linke turbidity are constants,
# but could be rasters

rads <- fasterSun(
	elevation='elevation',
	coeff_bh='coeff_bh',
	coeff_dh='coeff_dh',
	horizonHeight= 'horizonHeight',
	horizon_step = 90,
	aspect = 'aspectEastIs0',
	slope = 'slope',
	albedo = 0.2,
	linke = 3,
	day = 1,
	step = 0.5,
	declination = NULL,
	solar_constant = 1367,
	nprocs = 1,
	distance_step = 1,
	npartitions = 1,
	beam_rad = TRUE,
	diff_rad = TRUE,
	refl_rad = TRUE,
	glob_rad = TRUE,
	insol_time = TRUE,
	grassDir = grassDir,
	grassToR = TRUE
) 

# all values in Wh * m / day except for insolation time, which is in hours
plot(rads)

}
