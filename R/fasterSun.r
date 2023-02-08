#' Solar radiance and irradiation rasters
#'
#' This function calculates beam (direct), diffuse and ground reflected solar irradiation for a given day and set of topographic and atmospheric conditions. It utilizes the \code{GRASS} function \code{r.sun}. This function only works in "mode 2" of \href{https://grass.osgeo.org/grass82/manuals/r.sun.html}{\code{r.sun}}.
#'
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param elevation Either a \code{SpatRaster} or the name of a raster in an existing \code{GRASS} session with values representing elevation (typically in meters).
#' @param aspect Numeric, character, or a \code{SpatRaster} layer. This is either a raster with values of terrain aspect, the name of such a raster in an existing \code{GRASS} session, or a numeric value. Units are in degrees such that a value of 0 is assumed to point east, 90 north, 180 west, and 270 south. This raster can be created using \code{\link{fasterTerrain}}. If aspect is give in degrees such that 0 is north, 90 east, and so on, then it can be converted to the "east = 0" orientation using \code{\link{fasterConvertDegree}}.
#' @param slope Numeric, character, or a \code{SpatRaster} layer. This is either a raster with values of terrain slope, the name of such a raster in an existing \code{GRASS} session, or a numeric value. Units are in degrees. It can be created using \code{\link{fasterTerrain}}.
#' @param linke Numeric, character, or a \code{SpatRaster} layer. This is either a raster with values of the Linke atmospheric turbidity coefficient, the name of such a raster in an existing \code{GRASS} session, or a numeric value. The Linke coefficient is unit-less. The default value is 3.
#' @param albedo Numeric, character, or a \code{SpatRaster} layer. This is either a raster with values of ground albedo, the name of such a raster in an existing \code{GRASS} session, or a numeric value. Albedo is unit-less. The default value is 0.2.
#' @param coeff_bh Numeric, character, or a \code{SpatRaster} layer. This is either a raster with values of the real-sky beam radiation coefficient, the name of such a raster in an existing \code{GRASS} session, or a numeric value. Valid values are in the range [0, 1].
#' @param coeff_dh Numeric, character, or \code{SpatRaster} layer. This is either a raster with values of the real-sky diffuse radiation coefficient, the name of such a raster in an existing \code{GRASS} session, or a numeric value. Valid values are in the range [0, 1].
#' @param horizonHeight Character or stack of \code{SpatRaster} layers. This is either a raster stack/brick, with values in each raster representing the height of the horizon in each direction, or the "basename" of such a raster stack in an existing \code{GRASS} session. If the latter, it is assumed that the raster stack is in an existing \code{GRASS} session and their names are as "horizonHeight_000", "horizonHeight_005", "horizonHeight_010", etc., where the "_xxx" string at the end represents the directional angle in which height is assessed (such that 0 degrees is east, then going counterclockwise so 90 is north, 180 west, and 270 south). It is assume that the first raster is for 0 degrees (east). Rasters with horizon height can be generated using \code{\link{fasterHorizon}}. Values of horizon height are in units of degrees. Note that regardless of the input format, \code{horizon_step} must be specified.
#' @param horizon_step Numeric >0. Difference between angular steps in which horizon height is measured in the \code{horizonHeight} rasters. For example, if the \code{horizon_basename} rasters were named "horizonHeight_000", "horizonHeight_005", "horizonHeight_010", etc. then the step size would be 5 degrees between each raster.
#' @param day Positive integer in the range [1, 365]. Day of year for which to calculate ir/radiation. Default is 1 (January 1st).
#' @param step Positive integer in the range (0, 24]. Time step in hours for all-day radiation sums. Decimal values are OK.
#' @param declination Numeric. Declination value. If \code{NULL} (default), this is calculated automatically. Otherwise, this overrides the internal calculation.
#' @param solar_constant Positive numeric. The solar constant (solar energy hitting the top of the atmosphere). Default is 1367. Units are W / m^2.
#' @param time Positive numeric. Local solar time. Used for "mode 1" in the \code{r.sun} \code{GRASS} module. Default is \code{NULL} (i.e., use "mode 2").
#' @param nprocs Positive numeric in the range [1, 1000]. Number of processor cores to use. Default is 1.
#' @param distance_step Positive numeric in the range [0.5, 1.5]. Sampling distance coefficient. Default is 1.
#' @param npartitions Positive numeric. Number of chunks in which to read input files. Default is 1.
#' @param beam_rad Logical. If \code{TRUE} (default), generate a raster with beam irradiation with units of Wh * m^2 / day ("mode 2" of the \code{r.sun} \code{GRASS} module).
#' @param refl_rad Logical. If \code{TRUE} (default), generate a raster with ground-reflected irradiation with units of Wh * m^2 / day ("mode 2" of the \code{r.sun} \code{GRASS} module).
#' @param glob_rad Logical. If \code{TRUE} (default), generate a raster with total irradiance/irradiation with units of Wh * m^2 / day ("mode 2" of the \code{r.sun} \code{GRASS} module).
#' @param insol_time Logical. If \code{TRUE} (default), generate a raster with total insolation time in hours ("mode 2" of the \code{r.sun} \code{GRASS} module).
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack stack with the same extent, resolution, and coordinate reference system as \code{elevation}. Regardless, a raster or a set of rasters is written into an existing \code{GRASS} session. The names of these rasters are (assuming they are generated): \code{beam_rad}, \code{diff_rad}, \code{refl_rad}, \code{glob_rad}, and/or \code{insol_time}.
#'
#' @seealso \code{\link[terra]{terrain}} in \pkg{terra}; \code{\link{fasterHorizon}} and \code{\link{fasterTerrain}} in \pkg{fasterRaster}; \href{https://grass.osgeo.org/grass82/manuals/r.sun.html}{\code{r.sun}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterSun.R
#'
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
	
	distance_step = 1,
	npartitions = 1,

	beam_rad = TRUE,
	diff_rad = TRUE,
	refl_rad = TRUE,
	glob_rad = TRUE,
	insol_time = TRUE,
	
	lowMemory = FALSE,
	
	cores = fasterGetOptions('cores', 1),
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	flags <- .getFlags(replace=replace)
	
	# initialize GRASS
	if (is.null(inits)) inits <- list()
	inits <- c(inits, list(rast=elevation, vect=NULL, inRastName='elevation', inVectName=NULL, replace=replace, grassDir=grassDir))
	input <- do.call('startFaster', inits)
	
	## export rasters/collect scalars
	
	# constants
	args <- list(
		cmd = 'r.sun',
		flags = flags,
		day = day,
		step = step,
		nprocs = cores,
		distance_step = distance_step,
		npartitions = npartitions,
		elevation = 'elevation',
		coeff_bh = 'coeff_bh',
		coeff_dh = 'coeff_dh'
	)

	if (!is.null(declination)) args <- c(args, declination)
	
	# aspect
	if (inherits(aspect, 'SpatRaster')) { # raster
		
		fasterRast(aspect, inRastName='aspectFromEast', replace=replace)
		args <- c(args, aspect='aspectFromEast')
		
	} else if (inherits(aspect, 'numeric')) { # constant
		args <- c(args, aspect_value=aspect)
	} else if (inherits(aspect, 'character')) { # name in \code{GRASS}
		args <- c(args, aspect=aspect)
	}
	
	# slope
	if (inherits(slope, 'SpatRaster')) {
		fasterRast(slope, inRastName='slope', replace=replace)
		args <- c(args, slope='slope')
	} else if (inherits(slope, 'numeric')) { # constant
		args <- c(args, slope_value=slope)
	} else if (inherits(slope, 'character')) { # name in \code{GRASS}
		args <- c(args, slope=slope)
	}
	
	# albedo
	if (inherits(albedo, 'SpatRaster')) {
		fasterRast(albedo, inRastName='albedo', replace=replace)
		args <- c(args, albedo='albedo')
	} else if (inherits(albedo, 'numeric')) { # constant
		args <- c(args, albedo_value=albedo)
	} else if (inherits(albedo, 'character')) { # name in \code{GRASS}
		args <- c(args, albedo=albedo)
	}
	
	# linke
	if (inherits(linke, 'SpatRaster')) {
		fasterRast(linke, inRastName='aspect', replace=replace)
		args <- c(args, aspect='aspect')
	} else if (inherits(linke, 'numeric')) { # constant
		args <- c(args, linke_value=linke)
	} else if (inherits(linke, 'character')) { # name in \code{GRASS}
		args <- c(args, linke=linke)
	}

	# coefficients of beam and diffuse radiation
	if (!inherits(coeff_bh, 'character')) fasterRast(coeff_bh, inRastName='coeff_bh', replace=replace)
	if (!inherits(coeff_dh, 'character')) fasterRast(coeff_dh, inRastName='coeff_dh', replace=replace)
	
	# horizon height
	if (inherits(horizonHeight, 'character')) {
		args <- c(args, 'horizon_basename' = horizonHeight)
	} else {
		for (i in 1:terra::nlyr(horizonHeight)) {
			direction <- (i - 1) * horizon_step
			direction <- paste0(
				ifelse(direction < 100, '0', ''),
				ifelse(direction < 10, '0', ''),
				direction
			)
			fasterRast(horizonHeight[[i]], inRastName=paste0('horizonHeight_', direction), replace=replace)
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
	do.call(rgrass::execGRASS, args=args)
	
	if (grassToR) {

		# for (outName in c('incidout', 'beam_rad', 'diff_rad', 'refl_rad', 'glob_rad', 'insol_time')) {
		for (outName in c('beam_rad', 'diff_rad', 'refl_rad', 'glob_rad', 'insol_time')) {
		
			outValue <- get(outName)
			
			if (outValue) {

				thisOut <- fasterWriteRaster(outName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
				out <- if (exists('out', inherits=FALSE)) {
					c(out, thisOut)
				} else {
					thisOut
				}
			}
		}
		out
	} else { invisible(TRUE) }
	
}
