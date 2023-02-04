#' Calculations across cells surrounding a focal cell
#'
#' This functions calculates statistics on a moving "neighborhood" of cells of a raster. The neighborhood can be a square, circle, or a user-defined set of cells (with or without weights). This function is based on \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.neighbors.html}{\code{r.neighbors}}. In that module some of the functions return integers for the mean, standard deviation, and variance.  However, unlike that module, this function will return rasters with floating-point values for most functions (unless otherwise noted--see argument \code{fun}).
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param w The size and nature of the neighborhood:
#' \itemize{
#' 	\item "Square" neighborhoods (when \code{circle = FALSE}: An odd integer >= 3, indicating indicates the size of a "square" neighborhood (number of cells wide and number or cells tall).
#' 	\item "Circular" neighborhoods (when \code{circle = TRUE}: An odd integer >=3, indicating the diameter of the circle.
#' 	\item A matrix of cell weights: The matrix must be square and have an odd number of rows and columns (example: \code{matrix(c(0.5, 1, 0.5, 1, 2, 1, 0.5, 1, 0.5), nrow=3)}). You cannot use a weights matrix when \code{circle = TRUE}. Cells with \code{NA} as a weight will be ignored.
#' }
#'
#' @param fun Name of the function to calculate on each neighborhood.
#' \itemize{
#' 	\item \code{'mean'} (default)
#' 	\item \code{'median'}
#' 	\item \code{'mode'}
#' 	\item \code{'min'} or \code{'max'}
#' 	\item \code{'range'}
#' 	\item \code{'sd'}: Sample standard deviation. See note about standard deviations and variances in \strong\code{{Details}}.
#' 	\item \code{'sdPop'}: Population standard deviation. See note about standard deviations and variances in \strong\code{{Details}}.
#' 	\item \code{'sum'}: Sum of non-\code{NA} cells.
#' 	\item \code{'count'}: Number of non-\code{NA} cells.
#' 	\item \code{'var'}: Sample variance. See note about standard deviations and variances in \strong\code{{Details}}.
#' 	\item \code{'varPop'}: Population variance. See note about standard deviations and variances in \strong\code{{Details}}.
#' 	\item \code{'diversity'}: Number of unique values.
#' 	\item \code{'interspersion'}: Percent of cells with values different from focal cell, plus 1 (e.g., if 6 of 8 cells have different values, then the interspersion is 100 * 6/8 + 1 = 76). Values will be integers (i.e., not fractions of a percent).
#' 	\item \code{'quantile'}: The value in argument \code{quantile} is used to specify the quantile.
#' }
#' The center cell value is always included in the calculations, and all calculations ignore \code{NA} cells (i.e., they do not count as cells in the focal neighborhood).
#'
#' @param circle If \code{FALSE} (default), use a "square" neighborhood. If \code{TRUE}, use a "circular" neighborhood. When this is \code{TRUE}, argument \code{w} cannot be a matrix.
#'
#' @param quantile Quantile to calculate when \code{fun = 'quantile'}. The default value is 0.5 (median), and valid values are in [0, 1].
#'
#' @param cores Number of cores to use (default is 1).
#'
#' @details The population variance is calculated as:
#' \deqn{\sum((x_i - x_bar)^2) / N}
#' whereas the sample variance is calculated as
#' \deqn{\sum((x_i - x_bar)^2) / (N - 1)}
#' The population and sample standard deviations are just the square root of these, respectively. The \code{\link{sd}} function in \code{R} (and functions in \pkg{terra} that can use \code{sd}-like functions) calculate the sample standard deviation. Which is preferable? It depends on whether you consider all of the cells in a neighborhood are the complete statistical "population" of cells in the neighborhood, or represent a random sample thereof.
#' 
#' @return if \code{returnToR} is \code{TRUE}, a \code{SpatRaster}. Either way, a raster with the name given by \code{outGrassName} is created in a \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{focal}} and \code{\link[terra]{focalCpp}} in package \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/r.neighbors.html}{\code{r.neighbors}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterFocal.R
#'
#' @export

fasterFocal <- function(
	rast,
	inRastName,
	w = 3,
	fun = 'mean',
	circle = FALSE,
	quantile = 0.5,

	cores = 1,
	outGrassName = 'focalRast',

	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	# for debugging
	if (FALSE) {
	
		rast <- madElev
		inRastName <- 'madElev'
		fun <- 'sd'
		w <- 3
		quantile <- 0.5
		cores <- 4
		circle <- FALSE
		quantile <- 0.5
		grassToR <- TRUE
		inRastName <- ifelse(is.null(names(rast)), 'rast', names(rast))
		outGrassName <- 'focalRast'
	
	}

	### commons v1
	##############

		### arguments
		if (exists('rast', where=environment(), inherits=FALSE)) {
			inRastName <- .getInRastName(inRastName, rast=rast)
			.checkRastExists(replace=replace, rast=rast, inRastName=inRastName, outGrassName=outGrassName)
		} else {
			rast <- inRastName <- NULL
		}
		
		### overwrites?

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterFocal')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### function-specific
	
	# errors?
	if (inherits(w, 'numeric') && (w < 3 | compareFloat(w %% 2, 0, '=='))) stop('Argument "w" must be an odd integer >= 3.')
	
	if (inherits(w, 'matrix') && (nrow(w) != ncol(w) | compareFloat(nrow(w) %% 2, 0, '==') | compareFloat(ncol(w) %% 2, 0, '=='))) stop('Matrix "w" must have the same number of rows and columns, and it must have an odd number of each.')
	
	if (is.matrix(w) & circle) ('Cannot use a circular neighborhood and a weights matrix at the same time.')
	
	if (!(tolower(fun) %in% c('mean', 'average', 'sd', 'sdpop', 'var', 'varpop', 'median', 'mode', 'max', 'min', 'maximum', 'minimum', 'count', 'range', 'diversity', 'interspersion', 'quantile'))) stop('Argument "fun" is invalid.')
	
	### initialize GRASS
	input <- do.call('initGrass', inits)

	### function-specific

	# size
	size <- if (inherits(w, 'matrix')) {
		nrow(w)
	} else {
		w
	}

	# use user-specified region
	if (!autoRegion) flags <- c(flags, 'a')
	
	# circle
	if (circle) flags <- c(flags, 'c')

	# arguments
	args <- list(
		cmd = 'r.neighbors',
		input = input,
		output = outGrassName,
		method = fun,
		size = size,
		nprocs = cores,
		flags = flags
	)
	args <- c(args, dots)
	
	# quantile
	if (fun == 'quantile') args$quantile = quantile

	# weights matrix
	if (inherits(w, 'matrix')) {
		
		if (anyNA(w)) w[is.na(w)] <- 0
		
		weight <- as.data.frame(w)
		colnames(weight) <- NULL
		
		tempFileDirBack <- tempFileDir <- tempdir()
		dir.create(tempFileDir, showWarnings=FALSE, recursive=TRUE)
		
		rand <- round(runif(1) * 1E9)
		weightFile <- paste0(tempFileDir, '/TEMPTEMPweights', rand, '.txt')
		weightFileBack <- paste0(tempFileDirBack, '\\TEMPTEMPweights', rand, '.txt')
		
		sink(weightFile)
		print(weight, row.names=FALSE)
		sink()
		
		args <- c(args, weight=weightFileBack, weighting_function='file')
		
	}

	### execute
	if (autoRegion) regionReshape(inRastName)
	
	# convert rasters to double precision for some functions
	mapCalcFlags <- .getFlags(replace, 'quiet')
	if (fun %in% c('mean', 'average')) {

		# convert to double to obviate issues with imprecision
		doubleRast <- .makeTempName('doubleRast')
		ex <- paste0(doubleRast, ' = double(', inRastName, ')')
		rgrass::execGRASS('r.mapcalc', expression=ex, flags=mapCalcFlags)

		# focal sums (numerator)
		sumRast <- .makeTempName('sumRast')
		thisArgs <- args
		thisArgs$method <- 'sum'
		thisArgs$input <- doubleRast
		thisArgs$output <- sumRast
		do.call(rgrass::execGRASS, args=thisArgs)

		# ones mask (for denomimator)
		onesRast <- .makeTempName('onesRast')
		ex <- paste0(onesRast, ' = if(isnull(', doubleRast, '), null(), double(1))')
		rgrass::execGRASS('r.mapcalc', expression=ex, flags=mapCalcFlags)

		# count raster (sum of ones--denominator)
		countRast <- .makeTempName('countRast')
		thisArgs <- args
		thisArgs$method <- 'sum'
		thisArgs$input <- onesRast
		thisArgs$output <- countRast
		do.call(rgrass::execGRASS, args=thisArgs)

		# calculate mean
		ex <- paste0(outGrassName, ' = ', sumRast, ' / ', countRast)
		rgrass::execGRASS('r.mapcalc', expression=ex, flags=mapCalcFlags)
	
	} else if (tolower(fun) %in% c('sdpop', 'sd', 'var', 'varpop')) {
	
		# convert to double to obviate issues with imprecision
		doubleRast <- .makeTempName('doubleRast')
		ex <- paste0(doubleRast, ' = double(', inRastName, ')')
		rgrass::execGRASS('r.mapcalc', expression=ex, flags=mapCalcFlags)
		
		### numerator LHS
			
			# square values
			squaredRast <- .makeTempName('squaredRast')
			ex <- paste0(squaredRast, ' = ', doubleRast, '^2')
			rgrass::execGRASS('r.mapcalc', expression=ex, flags=mapCalcFlags)
			
			# focal sums of squares
			sumOfSquaresRast <- .makeTempName('sumOfSquaresRast')
			thisArgs <- args
			thisArgs$method <- 'sum'
			thisArgs$input <- squaredRast
			thisArgs$output <- sumOfSquaresRast
			do.call(rgrass::execGRASS, args=thisArgs)

		### numerator RHS
				
			# focal sums
			sumsRast <- .makeTempName('sumsRast')
			thisArgs <- args
			thisArgs$method <- 'sum'
			thisArgs$input <- doubleRast
			thisArgs$output <- sumsRast
			do.call(rgrass::execGRASS, args=thisArgs)
			
			# squared sums raster
			squaredSumsRast <- .makeTempName('squaredSumsRast')
			ex <- paste0(squaredSumsRast, ' = ', sumsRast, '^2')
			rgrass::execGRASS('r.mapcalc', expression=ex, flags=mapCalcFlags)

		### counts
				
			# ones mask... used to obviate problems where NULL cells are counted as 0
			onesRast <- .makeTempName('onesRast')
			ex <- paste0(onesRast, ' = if(isnull(', doubleRast, '), null(), double(1))')
			rgrass::execGRASS('r.mapcalc', expression=ex, flags=mapCalcFlags)
			
			# count raster (sum of ones)
			countRast <- .makeTempName('countRast')
			thisArgs <- args
			thisArgs$method <- 'sum'
			thisArgs$input <- onesRast
			thisArgs$output <- countRast
			do.call(rgrass::execGRASS, args=thisArgs)

		### final
				
			# numerator
			numerRast <- .makeTempName('numerRast')
			ex <- paste0(numerRast, ' = ', sumOfSquaresRast, ' - (', squaredSumsRast, ' / ', countRast, ')')
			rgrass::execGRASS('r.mapcalc', expression=ex, flags=mapCalcFlags)
			
			# sd or variance
			ex <- if (tolower(fun) == 'sd') {
				paste0(outGrassName, ' = sqrt(', numerRast, ' / (', countRast, ' - 1))')
			} else if (tolower(fun) == 'sdpop') {
				paste0(outGrassName, ' = sqrt(', numerRast, ' / ', countRast, ')')
			} else if (tolower(fun) == 'var') {
				paste0(outGrassName, ' = ', numerRast, ' / (', countRast, ' - 1)')
			} else if (tolower(fun) == 'varpop') {
				paste0(outGrassName, ' = ', numerRast, ' / (', countRast, ')')
			}

			rgrass::execGRASS('r.mapcalc', expression=ex, flags=mapCalcFlags)

	
	} else {
	
		if (fun == 'max') {
			args$method <- 'maximum'
		} else if (fun == 'min') {
			args$method <- 'minimum'
		}
		
		do.call(rgrass::execGRASS, args=args)
	
	}
	
	### export
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE)
		out <- terra::setMinMax(out)
		out
		
	}

}
