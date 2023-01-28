#' Calculations on "neighboring" cells of a raster
#'
#' Calculates statistics on a moving set of cells in a "neighborhood".
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
#' 	\item \code{'average'} or \code{'mean'} (default): \code{See note about rounding below!}
#' 	\item \code{'median'}
#' 	\item \code{'mode'}
#' 	\item \code{'minimum'} or \code{'maximum'}
#' 	\item \code{'range'}
#' 	\item \code{'stddev'} or \code{'sd'}: Standard deviation. \code{See note about rounding below!} \emph{Also note:} In \code{GRASS} the standard deviation is calculated as the population standard deviation: \deqn{sqrt(\sum((x_i - x_bar)^2) / N}, whereas in \code{base R} and in \pkg{terra}, it is calculated as the sample standard deviation: \deqn{sqrt(\sum((x_i - x_bar)^2) / (N - 1)}, (i.e., the same as in the function \code{sd}). Which is correct?  It depends on whether you considerall of the cells in a neighborhood are the complete statistical "population" of cells in the neighborhood, or represent a random sample thereof.
#' 	\item \code{'sum'}: Sum of non-\code{NA} cells.
#' 	\item \code{'count'}: Number of non-\code{NA} cells.
#' 	\item \code{'variance'} or \code{'var'}: \code{See note about rounding below!}. As with \code{stdev} (above), this is the population variance, not sample variance.
#' 	\item \code{'diversity'}: Number of unique values.
#' 	\item \code{'interspersion'}: Percent of cells with values different from focal cell, plus 1 (e.g., if 6 of 8 cells have different values, then the interspersion is 100 * 6/8 + 1 = 76). \code{See note about rounding below! Unlike other functions, rounding is NOT corrected for.}
#' 	\item \code{'quart1'}, \code{'quart3'}: First and third quarties (i.e., 25th and 75th quantiles).
#' 	\item \code{'perc90'}: The 90th quantile.
#' 	\item \code{'quantile'}
#' }
#' The center cell value is always included in the calculations.\cr
#' \emph{Rounding}: Some of the functions noted above normally have outputs rounded to the nearest integer when executed in \code{GRASS}. To obviate this issue, for these functions the raster is first multipled by a large number (given in \code{largeNum}). The operation is performed, then the raster is divided by this large number again. If this is an issue, you can multiply the raster by a large number, do the focal operation, then divide by the same number. You can skip this step by setting \code{largeNum} to \code{NULL} (i.e., so output will be rounded for these functions).
#' @param w Either the size of each neighborhood in number of cells across (a single, odd integer), \emph{or} a matrix of weights (example: \code{matrix(c(0.5, 1, 0.5, 1, 2, 1, 0.5, 1, 0.5), nrow=3)}). You cannot use a weights matric when \code{circle = TRUE} or \code{weightFx = 'gaussian' or 'exponential'}.
#' @param circle If \code{FALSE} (default), use a square neighborhood. If \code{TRUE}, then \code{size} will be the diameter of the circle (in the x- or y-direction). Cannot be used with \code{weightFx} or when \code{w} is a matrix.
#' @param weightFx Either \code{NULL} or the name of a weighting function. Valid values include: \code{'gaussian'} (Gaussian weighting) or \code{'exponential'} (exponential weighting). See also argument \code{weightFactor}. Cannot be used when \code{circle = TRUE} or \code{w} is a matrix.
#' @param weightFactor Either \code{NULL}, or factor used in the weighting function (if it is Gaussian or exponential). Ignored otherwise. If the weighting function is Gaussian, then this is the value of \eqn{\sigma} (the standard deviation), as in \deqn{exp(-(x*x+y*y)/(2*\sigma^2))/(2*π*σ^2)}. If exponential, this is the value of the decay parameter \eqn{d}, as in \deqn{exp(d*\sqrt(x^2+y^2))}. Negative values cause weights of cells more distant from the center to have less influence.
#' @param mask Either \code{NULL}, a \code{SpatRaster}, or name of a raster already in \code{GRASS}. This is used to select cells to be used in calculations. If provided, then only cells that are not \code{NA} in the mask are filled in. All others are assigned the value in \code{rast}.
#' @param quantile Quantile to calculate when \code{method = 'quantile'}. The default value is 0.5 (median), and valid values are in [0, 1].
#' @param largeNum For functions that normally produce rounded output in \code{GRASS}, multiply the raster first by this value then execute the focal operation, then divide by this number. Note that scientific notation cannot be used. If you wish to skip this step (i.e., round the output), then set this equal to \code{NULL}. The default values is 1000000, so values should be accurate to within 1E-5. This \emph{really should} be an integer (positive or negative).
#' @param cores Number of cores to use (default is 1).
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
	fun = 'mean',
	w = 3,
	circle = FALSE,
	weightFx = NULL,
	weightFactor = NULL,
	mask = NULL,
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
		circle <- FALSE
		weightFx <- NULL
		weightFactor <- NULL
		mask <- NULL
		quantile <- 0.5
		largeNum <- 1E6
		cores <- 2
		grassToR <- TRUE
		inRastName <- ifelse(is.null(names(rast)), 'rast', names(rast))
		outGrassName <- 'focal'
	
	}

	### begin common
	flags <- .getFlags(replace=replace)
	inRastName <- .getInRastName(inRastName, rast)
	# if (is.null(inVectName)) inVectName <- 'vect'
	
	# # region settings
	# success <- .rememberRegion()
	# on.exit(.restoreRegion(), add=TRUE)
	# on.exit(regionResize(), add=TRUE)
	
	if (is.null(inits)) inits <- list()
	### end common

	### catch errors
	if (inherits(w, 'matrix') & circle | inherits(w, 'matrix') & !is.null(weightFx) | circle & !is.null(weightFx)) stop('You can only use a scalar for "w", "circle = TRUE", or specify "weightFx".')

	if (!is.null(weightFx) & is.null(weightFactor)) stop('Please specify "weightFactor" when using "weightFx".')

	if (inherits(w, 'matrix')) {
		if (nrow(w) != ncol(w) | nrow(w) %% 2 == 0 | ncol(w) %% 2 == 0) stop('Matrix "w" must have the same number of rows and columns, and it must have an odd number of each.')
	}
	
	### size
	size <- if (inherits(w, 'matrix')) {
		nrow(w)
	} else {
		w
	}

	if (size %% 2 == 0) stop('Argument "size" must be an odd integer > 0.')

	### initialize GRASS
	inits <- c(inits, list(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, replace=replace, grassDir=grassDir))
	input <- do.call('initGrass', inits)

	if (fun == 'mean') fun <- 'average'
	if (fun == 'sd') fun <- 'stddev'
	if (fun == 'var') fun <- 'variance'
	
	if (fun %in% c('average', 'stddev', 'variance') & !is.null(largeNum)) {

		ex <- paste0('TEMPTEMPmult = ', sprintf('%.0f', largeNum), ' * ', inRastName)
		fasterApp(inRastName, expression=ex, grassToR=FALSE, outGrassName='TEMPTEMPmult', replace=replace, grassDir=grassDir)
		input[['raster']] <- 'TEMPTEMPmult'

	}

	theseFlags <- if (circle) {
		c(flags, 'c')
	} else {
		flags
	}
	
	if (inherits(w, 'matrix')) {
		
		weight <- as.data.frame(w)
		colnames(weight) <- NULL
		weightFx <- 'file'
		
		rand <- round(runif(1) * 1E6)
		tempFileDir <- if (exists('input', inherits = FALSE)) {
			paste0(attr(input, 'tempDir'), '/TEMP')
		} else {
			paste0(tempdir(), '/TEMP')
		}
		tempFileDirBack <- if (exists('input', inherits = FALSE)) {
			paste0(attr(input, 'tempDir'), '\\TEMP')
		} else {
			paste0(tempdir(), '\\TEMP')
		}
		
		dir.create(tempFileDir, showWarnings=FALSE, recursive=TRUE)
		
		weightFile <- paste0(tempFileDir, '/TEMPTEMPweights', rand, '.txt')
		weightFileBack <- paste0(tempFileDirBack, '\\TEMPTEMPweights', rand, '.txt')
		
		if (file.exists(weightFile)) unlink(weightFile, force=TRUE)
		if (file.exists(weightFile)) file.remove(weightFile, force=TRUE)

		sink(weightFile)
		print(weight, row.names=FALSE)
		sink()
		
	}

	# no mask
	if (is.null(mask)) {
	
		# no weight
		if (!inherits(w, 'matrix')) {
		
			# no weighting function
			if (is.null(weightFx)) {
				rgrass::execGRASS('r.neighbors', input=input, size=size, method=fun, output=outGrassName, flags=theseFlags, quantile=quantile, nprocs=cores)
			# weighting function
			} else {
				weightFx <- tolower(weightFx)
				rgrass::execGRASS('r.neighbors', input=input, size=size, method=fun, output=outGrassName, flags=theseFlags, weighting_function=weightFx, weighting_factor=weightFactor, quantile=quantile, nprocs=cores)
			}
		
		# weight (will always have weighting function = 'file')
		} else {
			rgrass::execGRASS('r.neighbors', input=input, size=size, method=fun, output=outGrassName, flags=theseFlags, weight=weightFileBack, weighting_function=weightFx, quantile=quantile, nprocs=cores)
		}
			
	# mask
	} else {
	
		maskName <- ifelse(is.null(names(mask)), 'TEMPTEMPmask', names(mask))
		fasterRast(mask, inRastName=maskName, replace=replace)
		
		# no weight
		if (!inherits(w, 'matrix')) {
		
			# no weighting function
			if (is.null(weightFx)) {
				rgrass::execGRASS('r.neighbors', input=input, size=size, method=fun, output=outGrassName, flags=theseFlags, quantile=quantile, nprocs=cores, selection=maskName)
			# weighting function
			} else {
				weightFx <- tolower(weightFx)
				rgrass::execGRASS('r.neighbors', input=input, size=size, method=fun, output=outGrassName, flags=theseFlags, weighting_function=weightFx, weighting_factor=weightFactor, quantile=quantile, nprocs=cores, selection=maskName)
			}
		
		# weight (will always have weighting function)
		} else {
			rgrass::execGRASS('r.neighbors', input=input, size=size, method=fun, output=outGrassName, flags=theseFlags, weight='TEMPTEMPweights.txt', weighting_function=weightFx, quantile=quantile, nprocs=cores, selection=maskName)
		}
	
	}
	
	if (fun %in% c('average', 'stddev', 'variance') & !is.null(largeNum)) {

		ex <- paste0(outGrassName, ' = ', outGrassName, ' / ', sprintf('%.0f', largeNum))
		fasterApp(inRastName, expression=ex, grassToR=FALSE, outGrassName=outGrassName, replace=replace, grassDir=grassDir)

	}

	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE)
		out
		
	}
	
}
