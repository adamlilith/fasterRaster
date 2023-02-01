#' Calculations on neighboring cells of a raster
#'
#' These functions calculate statistics on a moving set of cells in a "neighborhood". There are several ways to define the neighborhood, depending on the function:\cr
#' \itemize{
#'	\item \code{fasterFocal}: Conduct calculations for a "square" neighborhood where all cells are equally weighted. This requires users to specify a single, odd integer value for argument \code{diameter}.
#'	\item \code{fasterFocal} with argument \code{circle} set to \code{TRUE}: Conduct calculations for a "square" neighborhood where all cells are equally weighted. This requires users to specify a single, odd integer value for argument \code{diameter}.
#'	\item \code{fasterFocal}: Conduct calculations for a "square" neighborhood where cells are weighted according to a user-specified matrix.
#'	\item \code{fasterFocalDecay}: Conduct calculations for a "square" neighborhood where the relative weoights of cells decay from the focal cell as per a Gaussian or exponential function..
#'	\item \code{fasterFocalDecay} with argument \code{circle} set to \code{TRUE}: Conduct calculations for a "circular" neighborhood where the relative weoights of cells decay from the focal cell as per a Gaussian or exponential function..
#' }
#' Depending on the function applied to cells in the nerighborhood, additional arguments may be required.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param fun Name of the function to calculate on each neighborhood.
#' \itemize{
#' 	\item \code{'mean'} (default): See note about rounding in \strong\code{{Details}}.
#' 	\item \code{'median'}
#' 	\item \code{'mode'}
#' 	\item \code{'min'} or \code{'max'}
#' 	\item \code{'range'}
#' 	\item \code{'sd'}: Sample standard deviation. See notes about standard deviations and rounding in \strong\code{{Details}}.
#' 	\item \code{'popSd'}: Population standard deviation. See notes about standard deviations and rounding in \strong\code{{Details}}.
#' 	\item \code{'sum'}: Sum of non-\code{NA} cells.
#' 	\item \code{'count'}: Number of non-\code{NA} cells.
#' 	\item \code{'var'}: This is the population variance, not sample variance. Also see about rounding in \strong\code{{Details}}.
#' 	\item \code{'diversity'}: Number of unique values.
#' 	\item \code{'interspersion'}: Percent of cells with values different from focal cell, plus 1 (e.g., if 6 of 8 cells have different values, then the interspersion is 100 * 6/8 + 1 = 76). Also see about rounding in \strong\code{{Details}}--unlike for other functions, rounding is \emph{NOT} corrected for.
#' 	\item \code{'quantile'}: The value in argument \code{quantile} is used to specify the quantile.
#' }
#' The center cell value is always included in the calculations.\cr
#'
#' @param circle If \code{FALSE} (default), use a "square" neighborhood. If \code{TRUE}, use a "circular" neighborhood. When this is \code{TRUE}, argument \code{w} cannot be a matrix.
#'
#' @param w This determines the size and nature of the neighborhood:
#' \itemize{
#' 	\item "Square" neighborhoods (\code{circle = FALSE}: An odd integer >= 3, indicating indicates the size of a "square" neighborhood (number of cells wide and number or cells tall).
#' 	\item "Circular" neighborhoods (\code{circle = TRUE}: An odd integer >=3, indicating the diameter of the circle.
#' 	\item A matrix of cell weights: The matrix must be square and have an odd number of rows and columns (example: \code{matrix(c(0.5, 1, 0.5, 1, 2, 1, 0.5, 1, 0.5), nrow=3)}). You cannot use a weights matrix when \code{circle = TRUE}.
#' }
#'
#' @param decayFx The name of the decay function. Valid values include: \code{'Gaussian'} (Gaussian weighting) or \code{'exponential'} (exponential weighting). See also argument \code{decayRate}.
#'
#' @param decayRate Value used to determine the decay rate (i.e., relative weighting of cells as one moves away from the focal cell). If the decay function is Gaussian, then this is the value of \eqn{\sigma} (the standard deviation), as in \deqn{exp(-(x*x+y*y)/(2*\sigma^2))/(2*π*σ^2)}. Smaller values increase the weight of cells near the center relative to those father away. If exponential, this is the value of the decay parameter \eqn{d}, as in \deqn{exp(d*\sqrt(x^2+y^2))}. Negative values cause weights of cells more distant from the center to have less influence. The default value of -0.1 may not make sense given your situation.
#'
#' @param mask Either \code{NULL} (default), a \code{SpatRaster}, or name of a raster already in \code{GRASS}. This is used to select cells to be used in calculations. If provided, then only cells that are not \code{NA} in the mask are filled in. All others are assigned the value in \code{rast}.
#'
#' @param inMaskName Either \code{NULL} (default) or the name of the \code{mask} raster in \code{GRASS}. If this is missing, the \code{\link[terra:names]{name}} of the raster or raster file will be used. If \code{NULL}, then "\code{maskRast}" will be used.
#'
#' @param quantile Quantile to calculate when \code{method = 'quantile'}. The default value is 0.5 (median), and valid values are in [0, 1].
#'
#' @param largeNum For functions that normally produce rounded output in \code{GRASS}, multiply the raster first by this value then execute the focal operation, then divide by this number. If you wish to skip this step (i.e., round the output), then set this equal to \code{NULL}. The default values is 1E6, so values should be accurate to within 1E-5. This \emph{really should} be an integer (positive or negative).
#'
#' @param cores Number of cores to use (default is 1).
#'
#' @details \strong{Rounding}: Depending on the data type of the input raster, Some of the functions noted above have outputs rounded to the nearest integer when executed in \code{GRASS}. To obviate this issue, for these functions the raster is first multipled by a large number (given in \code{largeNum}). The operation is performed, then the raster is divided by this large number again. If this is an issue, you can multiply the raster by a large number in \code{R} or by using \code{\link{fasterApp}}, do the focal operation, then divide by the same number. You can skip this step by setting \code{largeNum} to \code{NULL} (i.e., so output will be rounded for these functions)./cr
#'
#' \strong{Standard deviations}: The population standard deviation is calculated as:
#' \deqn{sqrt(\sum((x_i - x_bar)^2) / N}
#' whereas the sample standard deviation is calculated as
#' \deqn{sqrt(\sum((x_i - x_bar)^2) / (N - 1)}
#' By default, \code{GRASS} calculates the population standard deviation, whereas \code{R}'s \code{\link{sd}} (and functions in \pkg{terra} that can use the \code{sd} function) calculate the sample standard deviation. Which is better? It depends on whether you consider all of the cells in a neighborhood are the complete statistical "population" of cells in the neighborhood, or represent a random sample thereof.
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

	mask = NULL,
	inMaskName = NULL,

	quantile = 0.5,
	largeNum = 1E6,

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
	
		fun <- 'mean'
		w <- 3
		quantile <- 0.5
		largeNum <- 1E6
		cores <- 4
		grassToR <- TRUE
		inRastName <- ifelse(is.null(names(rast)), 'rast', names(rast))
		outGrassName <- 'focalRast'
	
	}

	### commons v1
	##############

		### arguments
		if (exists('rast', where=environment(), inherits=FALSE)) {
			inRastName <- .getInRastName(inRastName, rast=rast)
		} else {
			rast <- inRastName <- NULL
		}

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
	
	# translate to GRASS function
	if (fun == 'mean') fun <- 'average'
	if (fun == 'max') fun <- 'maximum'
	if (fun == 'min') fun <- 'minimum'
	if (fun == 'var') fun <- 'variance'
	
	if (fun == 'popSd') {
		fun <- 'stddev' # population standard deviation
	} else if (fun == 'sd') {
		fun <- 'stddev' # sample standard deviation
		sampleSd <- TRUE
	} else {
		sampleSd <- FALSE
	}
	
	### initialize GRASS
	input <- do.call('initGrass', inits)

	### execute
	
	# rescale
	if (fun %in% c('average', 'stddev', 'variance') & !is.null(largeNum)) {

		ex <- paste0('= ', sprintf('%.0f', largeNum), ' * ', inRastName)
		
		fasterApp(
			rast = input,
			expression = ex,
			grassToR = FALSE,
			outGrassName = input,
			replace = TRUE,
			restartGrass = FALSE,
			autoRegion = autoRegion,
			grassDir = grassDir,
			...
		)
		
	}

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

	# mask
	if (!is.null(mask)) {
	
		inMaskName <- .getInRastName(inMaskName, mask)
		if (is.null(inMaskName)) inMaskName <- 'maskRast'
		fasterRast(mask, inRastName=inMaskName, replace=replace, autoRegion=autoRegion)
		args <- c(args, selection=inMaskName)
	
	}

	# execute
	if (autoRegion) regionReshape(inRastName)

	# anything but sample standard deviation
	if (!sampleSd) {

		do.call(rgrass::execGRASS, args=args)

	# sample standard deviation
	} else {
	
		# mean
		meanRast <- paste0('TEMPTEMP_meanRast', round(1E6 * runif(1)))

		fasterFocal(
			rast = inRastName,
			inRastName = inRastName,
			w = w,
			fun = 'mean',
			circle = circle,

			mask = inMaskName,
			inMaskName = inMaskName,

			quantile = quantile,
			largeNum = largeNum,

			cores = cores,
			outGrassName = meanRast,

			replace = TRUE,
			grassToR = FALSE,
			autoRegion = FALSE,
			grassDir = grassDir,
			...
		)
	
		# count of non-NA cells
		countRast <- paste0('TEMPTEMP_countRast', round(1E6 * runif(1)))

		fasterFocal(
			rast = inRastName,
			inRastName = inRastName,
			w = w,
			fun = 'count',
			circle = circle,

			mask = inMaskName,
			inMaskName = inMaskName,

			largeNum = largeNum,

			cores = cores,
			outGrassName = countRast,

			replace = TRUE,
			grassToR = FALSE,
			autoRegion = FALSE,
			grassDir = grassDir,
			...
		)

		# calculate sample SD
		ex <<- paste0('= sqrt(sum((', inRastName, ' - ', meanRast, ')^2) / (', countRast, ' - 1))')
	
		fasterApp(
			rast = inRastName,
			inRastName = inRastName,
			expression = ex,
			grassToR = FALSE,
			outGrassName = outGrassName,
			replace = TRUE,
			restartGrass = FALSE,
			autoRegion = FALSE,
			grassDir = grassDir,
			...
		)
		
		# fasterRm(meanRast)
		# fasterRm(countRast)
	
	}
	
	# rescale
	if (fun %in% c('average', 'stddev', 'variance') & !is.null(largeNum)) {

		ex <- paste0('= ', outGrassName, ' / ', sprintf('%.0f', largeNum))
		
		fasterApp(
			rast = outGrassName,
			inRastName = outGrassName,
			expression = ex,
			grassToR = FALSE,
			outGrassName = outGrassName,
			replace = TRUE,
			restartGrass = FALSE,
			autoRegion = FALSE,
			grassDir = grassDir,
			...
		)

	}

	# # correct to sample SD
	# if (fun == 'stddev' && sampleSd) {
	
		# thisArgs <- args
		# thisArgs$method <- 'count'
		# output <- paste0('TEMPTEMP_focalCount', round(1E6 * runif(1)))
		# thisArgs$output <- output
		# thisArgs$flags <- .getFlags(TRUE, thisArgs$flags)

		# # calculate weighted sample size
		# do.call(rgrass::execGRASS, args=thisArgs)

		# # ex <- paste0('= ', outGrassName, ' / sqrt(', output, ')')
		# ex <- paste0('= sqrt(((', outGrassName, '^2) * ', output, ') / (', output, ' - 1))')

		# fasterApp(
			# rast = outGrassName,
			# inRastName = outGrassName,
			# expression = ex,
			# outGrassName = outGrassName,
			# replace = TRUE,
			# grassToR = FALSE,
			# restartGrass = FALSE,
			# autoRegion = FALSE,
			# grassDir = grassDir,
			# ...
		# )
		
		# # fasterRm(output)
	
	# }

	### export
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE)
		out <- terra::setMinMax(out)
		out
		
	}

}
