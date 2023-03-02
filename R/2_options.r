#' Set or get options shared across 'fasterRaster' functions
#'
#' These functions allows you to either 1) view or 2) define options shared by **fasterRaster** functions. You can view the current value for a particular option or for all options using:
#' ```
#' getFastOptions('grassDir')
#' getFastOptions('rastClass')
#' getFastOptions() # all options
#' ```
#' and all default values using
#' ```
#' getFastOptions('rastClass', default = TRUE)
#' getFastOptions(default = TRUE) # all options
#' ```
#' You can set options using forms like:
#' ```
#' setFastOptions(grassDir = 'C:/Program Files/GRASS GIS 8.3')
#' getFastOptions(rastClass = 'stars')
#' getFastOptions(vectClass = 'sf')
#' getFastOptions(memory = 600)
#' getFastOptions(cores = 4)
#' getFastOptions(grassVer = '83')
#' ```
#' You can view or set multiple options at once using either function simply by listing them with a comma between them.
#'
#' @param ... Any **fasterRaster** shared option that can be defined using an `option = value` pattern. These include:
#'
#'	* `grassDir` (character): The folder in which **GRASS** is installed on your computer. By default, this value is `NA`, which causes the function to search for your installation of **GRASS** (and which usually fails). Depending on your operating system, your install directory will look something like this:
#'     * Windows: `'C:/Program Files/GRASS GIS 8.3'`
#'     * Mac OS: `"/Applications/GRASS-8.3.app/Contents/Resources"`
#'     * Linux: `'/usr/local/grass'`
#'
#' * `rastClass` (character): Sets the type of rasters that **fasterRaster** functions generate when they are returned to **R**.  By default, this argument is `'SpatRaster'` (from the **terra** package), but it can also be set to `'stars'` (from the **stars** package).
#'
#' * `vectClass` (character): Sets the type of spatial vectors that **fasterRaster** functions generate when they are returned to **R**. By default, this argument is `'SpatVector'` (**terra** package), but can also be `'sf'` (**sf** package).
#'
#' * `cores` (integer/numeric): Number of processor cores to use on a task. The default is 1. Only some **GRASS** modules allow this option.
#'
#' * `memory` (integer/numeric): The amount of memory to allocate to a task, in MB. The default is 300 MB. Only some **GRASS** modules allow this option.
#'
#' * `details (logical): If `TRUE`, show details on run-time and otherwise hidden slots in classes. This is mainly used for debugging, so most users will want to keep this at its default, `FALSE`.
#'
#' * `grassVer` (character): The [fasterHelp()] function supplies links to help pages for **GRASS** modules, but these links are version-dependent. You can change the version of **GRASS** you are using with this function, and thus avoid having to define it every time you use a link from [fasterHelp()]. This should be the version number of your **GRASS** installation, as a character (not numeric). The decimal is ignored. Valid examples: `'8.0'` or `'80'`.
#'
#' @param restore If `TRUE`, the all options will be reset to their default values. The default is `FALSE`.
#'
#' @param default Supplies the default value of the option(s).
#'
#' @return The `getFastOptions` function returns a list of the value of one or more **fasterRaster** settings. The `fasterRasterSet` function changes the values of these settings and returns the pre-existing values as a list (invisible--i.e., so you can revert to them if you want).
#'
#' @example man/examples/ex_options.r
#'
#' @export

setFastOptions <- function(
	...,
	restore = FALSE
) {

	dots <- list(...)
	if (length(dots) == 1L && inherits(dots[[1L]], 'list')) dots <- dots[[1L]]

	# retrive in case we want to reset them
	out <- getFastOptions()

	### set options
	if (length(dots) > 0L & restore) {
		warning('Cannot set options and use restore = TRUE at the same time.\n  No options have been changed.')	
	} else {

		### check for validity
		error <- paste0('Option <grassDir> must be NA (which is likely to fail)\n  or a single character string. The default is ', .grassDirDefault(), '.')
		if (any(names(dots) %in% 'grassDir')) {
			if (!is.character(dots$grassDir)) stop(error)
			if (length(dots$grassDir) != 1L) stop(error)
		}

		error <- paste0('Option <workDir> must be a single character string. The default is\n  ', .workDirDefault(), '.')
		if (any(names(dots) %in% 'workDir')) {
			if (!is.character(dots$workDir)) stop(error)
			if (length(dots$workDir) != 1L) stop(error)
		}

		error <- paste0('Option <location> must be a single character string. The default is <', .locationDefault(), '>.')
		if (any(names(dots) %in% 'location')) {
			if (!is.character(dots$location)) stop(error)
			if (length(dots$location) != 1L) stop(error)
		}

		error <- paste0('Option <mapset> must be a single character string. The default is <', .mapsetDefault(), '>.')
		if (any(names(dots) %in% 'mapset')) {
			if (!is.character(dots$mapset)) stop(error)
			if (length(dots$mapset) != 1L) stop(error)
		}

		if (any(names(dots) %in% 'cores')) {
			if (!is.numeric(dots$cores) | (dots$cores <= 0 & dots$cores %% 1 != 0)) stop('Option <cores> must be an integer >= 1. The default is ', .coresDefault(), '.')
		}

		if (any(names(dots) %in% 'memory')) {
			if (!is.numeric(dots$memory)) stop('Option <memory> must be a positive number. The default is ', .memoryDefault(), ' (MB).')
			if (dots$memory <= 0) stop('Option <memory> must be a positive number. The default is ', .memoryDefault(), ' (MB).')
		}

		if (any(names(dots) %in% 'rastClass')) {
			if (is.null(dots$rastClass) || is.na(dots$rastClass) || !(dots$rastClass %in% c('SpatRaster', 'stars'))) {
				stop('Option <rastClass> must be either <SpatRaster> or <stars>. The default is <', .rastClassDefault(), '>.')
			}
		}

		if (any(names(dots) %in% 'vectClass')) {
			if (is.null(dots$vectClass) || is.na(dots$vectClass) || !(dots$vectClass %in% c('SpatVector', 'sf'))) {
				stop('Option <vectClass> must be either <SpatVector> or <sf>. The default is <', .vectClassDefault(), '>.')
			}
		}

		if (any(names(dots) %in% 'grassVer')) {
			if (is.null(dots$grassVer) || is.na(dots$grassVer) || !inherits(dots$grassVer, c('numeric', 'character'))) {
				stop('Option <grassVer> must be the version number of GRASS installed on your system. The default is ', .grassVerDefault(), '.')
			}
			dots$grassVer <- as.character(dots$grassVer)
			dots$grassVer <- gsub(dots$grassVer, pattern='.', replacement='', fixed=TRUE)
			if (nchar(dots$grassVer) != 2L) stop('Please supply <grassVer> as a character string (including the trailing zero if 8.0).\n  Do not include the minor version (e.g., 8.2, not 8.2.1).\n  The default is ', .grassVerDefault(), '.')
			if (as.numeric(substr(dots$grassVer, 1, 1) < 8)) stop('fasterRaster only supports GRASS GIS version 8.0 or above.\n  The default is ', .grassVerDefault(), '.')
		}

		### set the options
		charOpts <- .namesOfOptions('character')

		if (restore) {
			dots <- .namesOfOptions()
			names(dots) <- dots
		}

		for (opt in names(dots)) {

			if (restore) {
				val <- paste0('.', opt, 'Default()')
				val <- eval(str2expression(val))
			} else {
				val <- dots[[opt]]
			}

			ex <- if (opt %in% charOpts) {
				paste0('options(', opt, '= "', val, '")')
			} else {
				paste0('options(', opt, '= ', val, ')')
			}
			eval(str2expression(ex))
		
		}


	}

	invisible(out)

}

#' @name getFastOptions
#' @title Report arguments shared across functions
#' @rdname setFastOptions
#' @export
getFastOptions <- function(..., default = FALSE) {

	opts <- list(...)
	namesOfOpts <- .namesOfOptions()
	opts <- if (length(opts) == 0L) { namesOfOpts } else { unlist(opts) }
	if (any(!(opts %in% namesOfOpts))) stop('Invalid option(s): ', paste(opts[!(opts %in% namesOfOpts)], collapse=', '))

	### return default values
	if (default) {

		out <- list()
		for (opt in opts) {

			ex <- paste0('.', opt, 'Default()')
			out[[length(out) + 1L]] <- eval(str2expression(ex))

		}

	### return options as they currently are
	} else {

		out <- list()
		for (opt in opts) {

			default <- paste0('.', opt, 'Default()')
			default <- eval(str2expression(default))
			out[[length(out) + 1L]] <- getOption(opt, default=default)
		}
		
	}

	names(out) <- opts
	if (length(out) == 1L) out <- unlist(out)
	out

}
