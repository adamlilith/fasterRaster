#' Set or get options shared across 'fasterRaster' functions
#'
#' @description These functions allows you to either 1) view or 2) define options shared by **fasterRaster** functions. You can view the current option(s) using, for example:
#' ```
#' getFastOptions('cores')
#' getFastOptions() # all options
#' ```
#' and all default values using
#' ```
#' getFastOptions('memory', default = TRUE)
#' getFastOptions(default = TRUE) # all options
#' ```
#' You can set options using forms like:
#' ```
#' setFastOptions(grassDir = 'C:/Program Files/GRASS GIS 8.3')
#' ```
#' Multiple options can be set at once using
#' ```
#' setFastOptions(details = TRUE, memory = 600, cores = 4)
#' ```
#' @param ... Either a character (the name of an option), or **fasterRaster** option that can be defined using an `option = value` pattern. These include:
#'
#' * `cores` (integer/numeric): Number of processor cores to use on a task. The default is 1. Only some **GRASS** modules allow this option.
#'
#' * `memory` (integer/numeric): The amount of memory to allocate to a task, in MB. The default is 300 MB. Only some **GRASS** modules allow this option.
#'
#' * `details` (logical): If `TRUE`, show details on run-time and otherwise hidden slots in classes. This is mainly used for debugging, so most users will want to keep this at its default, `FALSE`.
#'
#' * `grassVer` (character): The [fasterHelp()] function supplies links to help pages for **GRASS** modules, but these links are version-dependent. You can change the version of **GRASS** you are using with this function, and thus avoid having to define it every time you use a link from [fasterHelp()]. This should be the version number of your **GRASS** installation, as a character (not numeric). The decimal is ignored. Valid examples: `'8.0'` or `'80'`.
#'
#'	* `grassDir` (character): The folder in which **GRASS** is installed on your computer. Typically, this option is set when you run [fastStart()], but you can define it before you run that function. All subsequent calls of `fastStart`() do not need `grassDir` set because it will be obtained from the options. By default, `grassDir` is `NULL`, which causes the function to search for your installation of **GRASS** (and which usually fails). Depending on your operating system, your install directory will look something like this:
#'     * Windows: `'C:/Program Files/GRASS GIS 8.3'`
#'     * Mac OS: `"/Applications/GRASS-8.3.app/Contents/Resources"`
#'     * Linux: `'/usr/local/grass'`
#'
#'  * `workDir` (character): The folder in which **GRASS** rasters, vectors, and other objects are created and manipulated. Typically, this is set when you first call [fastStart()]. All subsequent calls to `fastStart()` will not do not need `workDir` defined because it will be obtained from the options. By default, this is set to the temporary directory on your operating system (from [tempdir()]), appended with "`fr`". Ergo, the path will be `file.path(tempdir(), 'fr')`.
#'
#' @param restore If `TRUE`, the all options will be reset to their default values. The default is `FALSE`.
#'
#' @param default Supplies the default value of the option(s).
#'
#' @return If just one option is specified, `getFastOptions()` returns a vector with a single value. If more than one option is specified, `gastFasterOptions()` returns a named list with values. The `setFasterOptions()` function changes the values of these settings and returns the pre-existing values as a list (invisibly--i.e., so you can revert to them if you want).
#'
#' @example man/examples/ex_options.r
#'
#' @export

setFastOptions <- function(
	...,
	restore = FALSE
) {

	opts <- list(...)
	if (length(opts) == 1L && inherits(opts[[1L]], 'list')) opts <- opts[[1L]]

	namesOfOpts <- .namesOfOptions()
	if (!is.null(opts) && any(!(names(opts) %in% namesOfOpts))) stop('Invalid option(s): ', paste(names(opts[!(opts %in% namesOfOpts)]), collapse=', '))
	
	# retrieve in case we want to reset them
	out <- getFastOptions()

	### check for validity
	error <- paste0('Option <grassDir> must be NULL (which is likely to fail)\n  or a single character string. The default is ', .grassDirDefault(), '.')
	if (any(names(opts) %in% 'grassDir')) {
		if (!is.character(opts$grassDir)) stop(error)
		if (length(opts$grassDir) != 1L) stop(error)
	}

	error <- paste0('Option <workDir> must be a single character string. The default is\n  ', .workDirDefault(), '.')
	if (any(names(opts) %in% 'workDir')) {
		if (!is.character(opts$workDir)) stop(error)
		if (length(opts$workDir) != 1L) stop(error)
	}

	error <- paste0('Option <location> must be a single character string. The default is <', .locationDefault(), '>.')
	if (any(names(opts) %in% 'location')) {
		if (!is.character(opts$location)) stop(error)
		if (length(opts$location) != 1L) stop(error)
	}

	error <- paste0('Option <mapset> must be a single character string. The default is <', .mapsetDefault(), '>.')
	if (any(names(opts) %in% 'mapset')) {
		if (!is.character(opts$mapset)) stop(error)
		if (length(opts$mapset) != 1L) stop(error)
	}

	if (any(names(opts) %in% 'cores')) {
		if (!is.numeric(opts$cores) | (opts$cores <= 0 & opts$cores %% 1 != 0)) stop('Option <cores> must be an integer >= 1. The default is ', .coresDefault(), '.')
	}

	if (any(names(opts) %in% 'memory')) {
		if (!is.numeric(opts$memory)) stop('Option <memory> must be a positive number. The default is ', .memoryDefault(), ' (MB).')
		if (opts$memory <= 0) stop('Option <memory> must be a positive number. The default is ', .memoryDefault(), ' (MB).')
	}

	# if (any(names(opts) %in% 'rastClass')) {
		# if (is.null(opts$rastClass) || is.na(opts$rastClass) || !(opts$rastClass %in% c('SpatRaster', 'stars'))) {
			# stop('Option <rastClass> must be either <SpatRaster> or <stars>. The default is <', .rastClassDefault(), '>.')
		# }
	# }

	# if (any(names(opts) %in% 'vectClass')) {
		# if (is.null(opts$vectClass) || is.na(opts$vectClass) || !(opts$vectClass %in% c('SpatVector', 'sf'))) {
			# stop('Option <vectClass> must be either <SpatVector> or <sf>. The default is <', .vectClassDefault(), '>.')
		# }
	# }

	if (any(names(opts) %in% 'grassVer')) {
		if (is.null(opts$grassVer) || is.na(opts$grassVer) || !inherits(opts$grassVer, c('numeric', 'character'))) {
			stop('Option <grassVer> must be the version number of GRASS installed on your system. The default is ', .grassVerDefault(), '.')
		}
		opts$grassVer <- as.character(opts$grassVer)
		opts$grassVer <- gsub(opts$grassVer, pattern='.', replacement='', fixed=TRUE)
		if (nchar(opts$grassVer) != 2L) stop('Please supply <grassVer> as a character string (including the trailing zero if 8.0).\n  Do not include the minor version (e.g., 8.2, not 8.2.1).\n  The default is ', .grassVerDefault(), '.')
		if (as.numeric(substr(opts$grassVer, 1, 1) < 8)) stop('fasterRaster only supports GRASS GIS version 8.0 or above.\n  The default is ', .grassVerDefault(), '.')
	}

	### set the options
	if (length(opts) == 0L) {
		opts <- as.list(namesOfOpts)
		names(opts) <- namesOfOpts
	}
	
	for (opt in names(opts)) {
	
		# default
		if (restore | is.null(.fasterRaster$options[[opt]])) {
			val <- paste0('.', opt, 'Default()')
			val <- eval(str2expression(val))
		} else {
			val <- opts[[opt]]
		}
		if (is.null(val)) {
			.fasterRaster$options[[opt]] <- list(val)
		} else {
			.fasterRaster$options[[opt]] <- val
		}
	}

	invisible(out)

}

#' @name getFastOptions
#' @title Report arguments shared across functions
#' @rdname setFastOptions
#' @export
getFastOptions <- function(..., default = FALSE) {

	namesOfOpts <- .namesOfOptions()
	opts <- unlist(list(...))
	if (!is.null(opts) && any(!(opts %in% namesOfOpts))) stop('Invalid option(s): ', paste(opts[!(opts %in% namesOfOpts)], collapse=', '))

	### return default values
	if (default) {

		if (length(opts) == 0L) opts <- namesOfOpts

		out <- list()
		for (opt in opts) {

			ex <- paste0('.', opt, 'Default()')
			out[[length(out) + 1L]] <- eval(str2expression(ex))

		}
		names(out) <- opts

	### return options as they currently are
	} else {

		out <- list()
		# we have no options :(
		if (length(opts) == 0L) {
			out <- .fasterRaster$options
		# we have options, people!
		} else {
			out <- list()
			for (opt in opts) {
				out[[length(out) + 1L]] <- .fasterRaster$options[[opt]]
			}
			names(out) <- opts
		}

	}

	if (length(out) == 1L) out <- unlist(out)
	out

}
