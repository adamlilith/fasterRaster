#' Calculate solar radiance and irradiation
#'
#' This function calculates beam (direct), diffuse and ground reflected solar irradiation for a given day and set of topographic and atmospheric conditions. It utilizes the GRASS function \code{r.sun}. This function only works in "mode 2" of \code{r.run}.
#' @param elevation Either a raster or the name of a raster in an existing GRASS session with values representing elevation (typically in meters).
#' @param aspect Numeric, character, or raster layer. This is either a raster with values of terrain aspect, the name of such a raster in an existing GRASS session, or a numeric value. Units are in degrees such that a value of 0 is assumed to point east, 90 north, 180 west, and 270 south. This raster can be created using \code{\link{fasterTerrain}}. If aspect is give in degrees such that 0 is north, 90 east, and so on, then it can be converted to the "east = 0" orientation using \code{\link{fasterConvertDegree}}.
#' @param slope Numeric, character, or raster layer. This is either a raster with values of terrain slope, the name of such a raster in an existing GRASS session, or a numeric value. Units are in degrees. It can be created using \code{\link{fasterTerrain}}.
#' @param linke Numeric, character, or raster layer. This is either a raster with values of the Linke atmospheric turbidity coefficient, the name of such a raster in an existing GRASS session, or a numeric value. The Linke coefficient is unit-less. The default value is 3.
#' @param albedo Numeric, character, or raster layer. This is either a raster with values of ground albedo, the name of such a raster in an existing GRASS session, or a numeric value. Albedo is unit-less. The default value is 0.2.
#' @param coeff_bh Numeric, character, or raster layer. This is either a raster with values of the real-sky beam radiation coefficient, the name of such a raster in an existing GRASS session, or a numeric value. Valid values are in the range [0, 1].
#' @param coeff_dh Numeric, character, or raster layer. This is either a raster with values of the real-sky diffuse radiation coefficient, the name of such a raster in an existing GRASS session, or a numeric value. Valid values are in the range [0, 1].
#' @param horizonHeight Character or raster stack/brick. This is either a raster stack/brick, with values in each raster representing the height of the horizon in each direction, or the "basename" of such a raster stack in an existing GRASS session. If the latter, it is assumed that the raster stack is in an existing GRASS session and their names are as "horizonHeight_000", "horizonHeight_005", "horizonHeight_010", etc., where the "_xxx" string at the end represents the directional angle in which height is assessed (such that 0 degrees is east, then going counterclockwise so 90 is north, 180 west, and 270 south). It is assume that the first raster is for 0 degrees (east). Rasters with horizon height can be generated using \code{\link{fasterHorizon}}. Values of horizon height are in units of degrees. Note that regardless of the input format, \code{horizon_step} must be specified.
#' @param horizon_step Numeric >0. Difference between angular steps in which horizon height is measured in the \code{horizonHeight} rasters. For example, if the \code{horizon_basename} rasters were named "horizonHeight_000", "horizonHeight_005", "horizonHeight_010", etc. then the step size would be 5 degrees between each raster.
#' @param day Positive integer in the range [1, 365]. Day of year for which to calculate ir/radiation. Default is 1 (January 1st).
#' @param step Positive integer in the range (0, 24]. Time step in hours for all-day radiation sums. Decimal values are OK.
#' @param declination Numeric. Declination value. If \code{NULL} (default), this is calculated automatically. Otherwise, this overrides the internal calculation.
#' @param solar_constant Positive numeric. The solar constant (solar energy hitting the top of the atmosphere). Default is 1367. Units are W / m^2.
#' @param time Positive numeric. Local solar time. Used for "mode 1" in the \code{r.sun} GRASS module. Default is \code{NULL} (i.e., use "mode 2").
#' @param nprocs Positive numeric in the range [1, 1000]. Number of processor cores to use. Default is 1.
#' @param distance_step Positive numeric in the range [0.5, 1.5]. Sampling distance coefficient. Default is 1.
#' @param npartitions Positive numeric. Number of chunks in which to read input files. Default is 1.
#' @param beam_rad Logical. If \code{TRUE} (default), generate a raster with beam irradiation with units of Wh * m^2 / day ("mode 2" of the \code{r.sun} GRASS module).
#' @param refl_rad Logical. If \code{TRUE} (default), generate a raster with ground-reflected irradiation with units of Wh * m^2 / day ("mode 2" of the \code{r.sun} GRASS module).
#' @param glob_rad Logical. If \code{TRUE} (default), generate a raster with total irradiance/irradiation with units of Wh * m^2 / day ("mode 2" of the \code{r.sun} GRASS module).
#' @param insol_time Logical. If \code{TRUE} (default), generate a raster with total insolation time in hours ("mode 2" of the \code{r.sun} GRASS module).
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{longitude} and \code{latitude}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack stack with the same extent, resolution, and coordinate reference system as \code{elevation}. Otherwise, a raster or a set of rasters is written into an existing GRASS session. The names of these rasters are (assuming they are generated): \code{beam_rad}, \code{diff_rad}, \code{refl_rad}, \code{glob_rad}, and/or \code{insol_time}.
#' @details See the documentation for the GRASS module \code{r.sun} at \url{https://grass.osgeo.org/grass78/manuals/r.sun.html}.
#' @seealso \code{\link{fasterHorizon}}, \code{\link{fasterTerrain}}
#' @examples
#' \donttest{
#' # change this to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8' # example for a PC
#' grassDir <- "/Applications/GRASS-7.8.app/Contents/Resources" # for a Mac
#'
#' # NB This examples uses "chaining" of a GRASS session between faster
#' # functions. That is, rasters are created in GRASS then kept in GRASS
#' # (not exported back to R). This way, the functions do not have to restart
#' # GRASS and do not need to copy rasters to/from R until the end.
#' 
#' # elevation raster
#' data(madElev)
#' 
#' # initialize a GRASS session with the elevation raster
#' initGrass(alreadyInGrass=FALSE, rast=madElev, rastName='elevation',
#' grassDir=grassDir)
#' 
#' # beam and diffuse radiation fractions... assuming they are both 1
#' # (must be between 0 an 1)
#' coeff_bh <- coeff_dh <- madElev * 0 + 1
#' exportRastToGrass(coeff_bh, grassName='coeff_bh')
#' exportRastToGrass(coeff_dh, grassName='coeff_dh')
#' 
#' # calculate horizon height rasters... to reduce time just using 90 deg steps
#' fasterHorizon('elevation', units='radians', directions=c(0, 90, 180, 
#' 270), northIs0=FALSE, grassDir=grassDir, alreadyInGrass=TRUE,
#' grassToR = FALSE)
#' 
#' # terrain rasters
#' fasterTerrain('elevation', slope=TRUE, aspect=TRUE, northIs0=FALSE,
#' grassDir=grassDir, alreadyInGrass=TRUE, grassToR=FALSE)
#' 
#' # Assuming albedo and Linke turbidity are constants,
#' # but could be rasters
#' 
#' rads <- fasterSun(
#' 	elevation='elevation',
#' 	coeff_bh='coeff_bh',
#' 	coeff_dh='coeff_dh',
#' 	horizonHeight= 'horizonHeight',
#' 	horizon_step = 90,
#' 	aspect = 'aspectEastIs0',
#' 	slope = 'slope',
#' 	albedo = 0.2,
#' 	linke = 3,
#' 	day = 1,
#' 	step = 0.5,
#' 	declination = NULL,
#' 	solar_constant = 1367,
#' 	nprocs = 1,
#' 	distance_step = 1,
#' 	npartitions = 1,
#' 	beam_rad = TRUE,
#' 	diff_rad = TRUE,
#' 	refl_rad = TRUE,
#' 	glob_rad = TRUE,
#' 	insol_time = TRUE,
#' 	grassDir = grassDir,
#' 	alreadyInGrass = TRUE,
#' 	grassToR = TRUE
#' ) 
#' 
#' # all values in Wh * m / day except for insolation time, which is in hours
#' plot(rads)
#' }
#' @export

fasterSun <- function(
	elevation,
	coeff_bh,
	coeff_dh,

	horizonHeight = 'horizonHeight',
	horizon_step = 90,
	
	aspect = 180,
	slope = 0,
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
	
	
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# initialize GRASS
	input <- initGrass(alreadyInGrass, rast=elevation, vect=NULL, rastName='elevation', grassDir=grassDir)
	
	## export rasters/collect scalars
	
	# constants
	args <- list(
		cmd = 'r.sun',
		flags = flags,
		day = day,
		step = step,
		nprocs = nprocs,
		distance_step = distance_step,
		npartitions = npartitions,
		elevation = 'elevation',
		coeff_bh = 'coeff_bh',
		coeff_dh = 'coeff_dh'
	)

	if (!is.null(declination)) args <- c(args, declination)
	
	# aspect
	if (class(aspect) == 'RasterLayer') { # raster
		
		exportRasterToGrass(aspect, grassName='aspectFromEast')
		args <- c(args, aspect='aspectFromEast')
		
	} else if (class(aspect) == 'numeric') { # constant
		args <- c(args, aspect_value=aspect)
	} else if (class(aspect) == 'character') { # name in GRASS
		args <- c(args, aspect=aspect)
	}
	
	# slope
	if (class(slope) == 'RasterLayer') {
		exportRasterToGrass(slope, grassName='slope')
		args <- c(args, slope='slope')
	} else if (class(slope) == 'numeric') { # constant
		args <- c(args, slope_value=slope)
	} else if (class(slope) == 'character') { # name in GRASS
		args <- c(args, slope=slope)
	}
	
	# albedo
	if (class(albedo) == 'RasterLayer') {
		exportRasterToGrass(albedo, grassName='albedo')
		args <- c(args, albedo='albedo')
	} else if (class(albedo) == 'numeric') { # constant
		args <- c(args, albedo_value=albedo)
	} else if (class(albedo) == 'character') { # name in GRASS
		args <- c(args, albedo=albedo)
	}
	
	# linke
	if (class(linke) == 'RasterLayer') {
		exportRasterToGrass(linke, grassName='aspect')
		args <- c(args, aspect='aspect')
	} else if (class(linke) == 'numeric') { # constant
		args <- c(args, linke_value=linke)
	} else if (class(linke) == 'character') { # name in GRASS
		args <- c(args, linke=linke)
	}

	# coefficients of beam and diffuse radiation
	if (!alreadyInGrass) {
		exportRastToGrass(coeff_bh, grassName='coeff_bh')
		exportRastToGrass(coeff_dh, grassName='coeff_dh')
	}
	
	# horizon height
	if (class(horizonHeight) == 'character') {
		args <- c(args, 'horizon_basename' = horizonHeight)
	} else {
		for (i in 1:raster::nlayers(horizonHeight)) {
			direction <- (i - 1) * horizon_step
			direction <- paste0(
				ifelse(direction < 100, '0', ''),
				ifelse(direction < 10, '0', ''),
				direction
			)
			exportRastToGrass(horizonHeight[[i]], grassName=paste0('horizonHeight_', direction))
		}
		args <- c(args, 'horizon_basename' = 'horizonHeight')
	}
	
	args <- c(args, 'horizon_step' = horizon_step)
	
	# output names
	# if (incidout) args <- c(args, 'incidout' = 'incidout')
	if (beam_rad) args <- c(args, 'beam_rad' = 'beam_rad')
	if (diff_rad) args <- c(args, 'diff_rad' = 'diff_rad')
	if (refl_rad) args <- c(args, 'refl_rad' = 'refl_rad')
	if (glob_rad) args <- c(args, 'glob_rad' = 'glob_rad')
	if (insol_time) args <- c(args, 'insol_time' = 'insol_time')
	
	# execute!
	do.call(rgrass7::execGRASS, args=args)
	
	if (grassToR) {

		# for (outName in c('incidout', 'beam_rad', 'diff_rad', 'refl_rad', 'glob_rad', 'insol_time')) {
		for (outName in c('beam_rad', 'diff_rad', 'refl_rad', 'glob_rad', 'insol_time')) {
		
			outValue <- get(outName)
			
			if (outValue) {

				thisOut <- rgrass7::readRAST(outName)
				thisOut <- raster::raster(thisOut)
				names(thisOut) <- outName
				out <- if (exists('out', inherits=FALSE)) {
					raster::stack(out, thisOut)
				} else {
					thisOut
				}
			
			}
			
		}
		
		out
		
	}
	
}
