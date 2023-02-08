#' Report or define arguments shared across functions
#' 
#' These functions allows users to either 1) view or 2) define arguments or options shared by most \pkg{fasterRaster} functions. Using \code{fasterSetOptions}, you can set the default values for all functions at once. However you can override these defaults on a case-by-case basis by defining the relevant argument(s) when you use a particular function.  You can view the current default values of all shared \pkg{fasterRaster} arguments using:\cr
#'
#' \code{fasterGetOptions()}\cr
#'
#' or see the value of a particular argument using (for example):\cr
#'
#' \code{fasterGetOptions('grassDir')} or\cr
#' \code{fasterGetOptions('replace')}\cr
#'
#' These functions are a \pkg{fasterRaster}-specific wrappers for \code{\link{options}} and \code{\link{getOption}}.
#'
#' @param ... Any shared \code{fasterRaster} shared argument or option that can be defined using the \code{[argument/option] = [value]} pattern. These include:
#'
#' \itemize{
#'		\item	\code{grassDir} The folder in which \code{GRASS GIS} is installed on your computer. By default, this value is \code{NULL}, which causes the function to search for your installation of \code{GRASS} (and which usually fails). Depending on your operating system, your install directory will look something like this:\cr
#' \code{fasterSetOptions(grassDir = 'C:/Program Files/GRASS GIS 8.3') # example for Windows} \cr
#' \code{fasterSetOptions(grassDir = "/Applications/GRASS-8.3.app/Contents/Resources") # example for a Mac} \cr
#' \code{fasterSetOptions(grassDir = '/usr/local/grass') # example for Linux} \cr
#'
#'	\item \code{cores}: Number of processor cores to use on a task. The default is 1. Only some \code{GRASS} modules allow this option.
#' 
#'	\item \code{memory}: The amount of memory to allocate to a task, in MB. The default is 300 MB. Only some \code{GRASS} modules allow this option.
#' 
#'		\item	\code{replace}: The \code{replace} argument tells a function if it is allowed to overwrite any existing rasters or vectors in an active \code{GRASS} session. This includes rasters or vectors that are exported to the \code{GRASS} session or created within it. The default for this argument is \code{FALSE} (don't allow overwriting).
#'
#' 		\item \code{grassToR}: By default, this arguments is \code{TRUE}, which causes most \pkg{fasterRaster} functions to return rasters or vectors they create or modify back to \code{R}. This can take a lot of time. However, once a raster or vector is in a \code{GRASS} session, it can be manipulated by further functions without the time-consuming task of moving it back and forth between \code{GRASS} and \code{R}. Using multiple \pkg{fasterRaster} functions together like this is called \link{chaining}.
#'
#' 	\item \code{trimRast}: This options determines if rasters imported from \pkg{GRASS} to \code{R} will have rows and columns that are entirely \code{NA} removed from them first. The default is \code{TRUE}, which will trim these rows and columns. This is especially relevant when you are using rasters and/or vectors of very different spatial extents. This can cause smaller rasters to have "empty" rows and columns added to them.
#'
#' 	\item \code{outVectClass}: By default, this arguments is \code{'SpatRaster'}, which causes \code{fasterRaster} functions that output spatial vectors to produce objects of class \code{SpatVector} from the \pkg{terra} package. However, if this argument is \coed{'sf'}, these functions will output \code{sf} objects from the \pkg{sf} package.
#'
#' 	\item	\code{autoRegion}: In \code{GRASS}, a \link{region} is a data object that defines the extent and spatial resolution of operations on rasters, including raster creation and manipulation, and exporting and saving of rasters. By default, \code{autoRegion} is \code{TRUE}, which causes \pkg{fasterRaster} functions to automatically manage the extent and resolution of the active \code{GRASS} \link{region}. Most users of \pkg{fasterRaster} will likely desire to keep this behavior, so should not change this argument to \code{FALSE}. However, it can be turned off so users can manually manipulate the region. When this is the case, the functions that manipulate regions (they all start with "\code{region}") may be of use.
#'
#'	\item \code{grassVer}: The \code{\link{fasterHelp}} function supplies links to help pages for \code{GRASS} modules, but these links are version-dependent. You can change the version of \code{GRASS} you are using with this function, and thus avoid having to define it every time you use a link from \code{\link{fasterHelp}}. This should be the version number of your \code{GRASS} installation, as a character (not numeric). The decimal is ignored. Valid examples: \code{'8.0'} or \code{'80'}.
#' }
#'
#' @param restore If \code{TRUE}, the all shared arguments and settings will be reset to their default values.
#'
#' @param default Supplies the default value of the setting if it does not appear in \code{options}.
#'
#' @return The \code{fasterGetOptions} function reports the value of one or more \pkg{fasterRaster} settings. The \code{fasterRasterSet} function changes the values of these settings.
#'
#' @example man/examples/ex_fasterOptions.r
#'
#' @seealso \code{\link{options}} and \code{\link{getOption}}
#'
#' @export

fasterSetOptions <- function(
	...,
	restore = FALSE
) {

	dots <- list(...)
	
	### get current options
	olds <- list(
		cores = getOption('cores', .coresDefault()),
		memory = getOption('memory', .memoryDefault()),
		replace = getOption('replace', .replaceDefault()),
		grassToR = getOption('grassToR', .grassToRDefault()),
		trimRast = getOption('trimRast', .trimRastDefault()),
		outVectClass = getOption('outVectClass', .outVectClassDefault()),
		autoRegion = getOption('autoRegion', .autoRegionDefault()),
		grassDir = getOption('grassDir', .grassDirDefault()),
		grassVer = getOption('grassVer', .grassVerDefault())
	)
	
	# restore to defaults
	if (length(dots) == 0L & restore) {

		dots <- list(
			cores = .coresDefault(),
			memory = .memoryDefault(),
			replace = .replaceDefault(),
			grassToR = .grassToRDefault(),
			trimRast = .trimRastDefault(),
			outVectClass = .outVectClassDefault(),
			autoRegion = .autoRegionDefault(),
			grassDir = .grassDirDefault(),
			grassVer = .grassVerDefault()
		)
		
	} else if (length(dots) > 1L & restore) {
		stop('Cannot set an option and restore to defaults at the same time.')
	}

	### check for validity
	######################
	if (any(names(dots) %in% 'grassDir')) {
	
		if (!is.null(dots$grassDir) && !is.character(grassDir)) {
		
			if (!inherits(dots$grassDir, 'character')) stop('Setting "grassDir" must be NULL or a character string.')
			if (length(dots$grassDir) != 1L) stop('Setting "grassDir" must be a single character string.')

		}

	}

	if (any(names(dots) %in% 'cores')) {
		if (!is.numeric(dots$cores) | (dots$cores <= 0 & dots$cores %% 1 != 0)) stop('Setting "cores" must be a positive integer.')
	}

	if (any(names(dots) %in% 'memory')) {
		if (!is.numeric(dots$memory)) stop('Setting "memory" must be a positive numeric value.')
		if (dots$memory <= 0) stop('Setting "memory" must be a positive numeric value.')
	}

	if (any(names(dots) %in% 'replace')) {
		if (is.null(dots$replace) || is.na(dots$replace) || !is.logical(dots$replace)) stop('Setting "replace" must be TRUE or FALSE.')
	}

	if (any(names(dots) %in% 'grassToR')) {
		if (is.null(dots$grassToR) || is.na(dots$grassToR) || !is.logical(dots$grassToR)) stop('Setting "grassToR" must be TRUE or FALSE.')
	}

	if (any(names(dots) %in% 'autoRegion')) {
		if (is.null(dots$autoRegion) || is.na(dots$autoRegion) || !is.logical(dots$autoRegion)) stop('Setting "autoRegion" must be TRUE or FALSE.')
	}

	if (any(names(dots) %in% 'trimRast')) {
		if (
			is.null(dots$trimRast) ||
			is.na(dots$trimRast) ||
			!(is.logical(dots$trimRast))
		) stop('Setting "trimRast" must be TRUE or FALSE.')
	}

	if (any(names(dots) %in% 'outVectClass')) {
		if (
			is.null(dots$outVectClass) ||
			is.na(dots$outVectClass) ||
			!(dots$outVectClass %in% c('SpatVector', 'sf'))
		) stop('Setting "outVectClass" must be either "SpatVector" or "sf".')
	}

	if (any(names(dots) %in% 'grassVer')) {
	
		if (
			is.null(dots$grassVer) ||
			is.na(dots$grassVer) ||
			!inherits(dots$grassVer, c('numeric', 'character'))
		) stop('Setting "grassVer" must be the version number of GRASS installed on your system.')
		
		
		dots$grassVer <- as.character(dots$grassVer)
		dots$grassVer <- gsub(dots$grassVer, pattern='.', replacement='', fixed=TRUE)
		if (nchar(dots$grassVer) != 2L) stop('Please supply "grassVer" as a character string (including the trailing zero if "8.0").\n  Do not include the minor version (e.g., "8.2", not "8.2.1").')
		if (as.numeric(substr(dots$grassVer, 1, 1) < 8)) stop('fasterRaster and its supporting package rgrass only supports GRASS GIS version 8.0 or above.')
	
	}

	### set new values
	if (length(dots) > 0L) {
	
		for (i in seq_along(dots)) {
		
			arg <- names(dots)[i]
			val <- dots[i]
			
			# logical or numeric setting?
			ex <- if (arg %in% c('cores', 'memory', 'replace', 'grassToR', 'trimRast', 'autoRegion')) {
				paste0('options(', arg, ' = ', val, ')')
			# string/numeric setting?
			} else {
				paste0('options(', arg, ' = \"', val, '\")')
			}
			
			eval(str2expression(ex))
		
		}
	
	}
	
	invisible(olds)

}

#' @name `fasterGetOptions
#' @title Report arguments shared across functions
#' @rdname fasterSetOptions
#' @export
fasterGetOptions <- function(
	x,
	default = NULL
) {

	# report all values
	if (missing(x)) {
		out <- list(
			cores = getOption('cores', .coresDefault()),
			memory = getOption('memory', .memoryDefault()),
			replace = getOption('replace', .replaceDefault()),
			grassToR = getOption('grassToR', .grassToRDefault()),
			trimRast = getOption('trimRast', .trimRastDefault()),
			outVectClass = getOption('outVectClass', .outVectClassDefault()),
			autoRegion = getOption('autoRegion', .autoRegionDefault()),
			grassDir = getOption('grassDir', .grassDirDefault()),
			grassVer = getOption('grassVer', .grassVerDefault())
		)

	} else {

		opts <- c('grassDir', 'cores', 'memory', 'replace', 'grassToR', 'trimRast', 'outVectClass', 'autoRegion', 'grassVer')

		if (
			is.null(x) ||
			is.na(x) ||
			length(x) < 1L ||
			length(x) > 1L ||
			!(x %in% opts)
		) stop('Invalid value for argument "x". Please see ?fasterGetOptions.')

		if (is.null(default)) {
		
			default <- if (x == 'grassDir') {
				.grassDirDefault()
			} else if (x == 'cores') {
				.coresDefault()
			} else if (x == 'memory') {
				.memoryDefault()
			} else if (x == 'replace') {
				.replaceDefault()
			} else if (x == 'grassToR') {
				.grassToRDefault()
			} else if (x == 'trimRast') {
				.trimRastDefault()
			} else if (x == 'outVectClass') {
				.outVectClassDefault()
			} else if (x == 'autoRegion') {
				.autoRegionDefault()
			} else if (x == 'grassVer') {
				.grassVerDefault()
			}
		
		}

		out <- getOption(x, default=default)

	}
	
	out

}
