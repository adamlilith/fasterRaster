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
#'	* `grassDir` (character): The folder in which **GRASS** is installed on your computer. By default, this value is `NULL`, which causes the function to search for your installation of **GRASS** (and which usually fails). Depending on your operating system, your install directory will look something like this:
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
#' * `showDetails (logical): If \code{TRUE}, show details on run-time and otherwise hidden slots in classes. This is mainly used for debugging, so most users will want to keep this at its default, `FALSE`.
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
	opts <- .namesOfOptions()

	out <- getFastOptions()

	### reset all options
	if (length(dots) == 0L & restore) {

		# return existing values
		if (exists('.fasterRaster', where=globalenv())) {

			# return existing values
			out <- getFastOptions()

			# reset
			for (opt in opts) {

				ex <- paste0('local({', opt, ' <- .', opt, 'Default()}, envir=.fasterRaster)')
				eval(str2expression(ex))

			}

		} else {
			out <- NULL
			warning('GRASS has not been initialized yet. Please use startFast().')
		}

	### setting one or more options
	} else if (length(dots) > 0L & !restore) {

		### check for validity
		if (any(names(dots) %in% 'grassDir')) {
			if (!is.null(dots$grassDir) && !is.character(grassDir)) {
				if (!inherits(dots$grassDir, 'character')) stop('Option <grassDir> must be NULL or a character string. The default is ', .grassDirDefault(), '.')
				if (length(dots$grassDir) != 1L) stop('Option <grassDir> must be a single character string. The default is ', .grassDirDefault(), '.')
			}
		}

		if (any(names(dots) %in% 'location')) {
			if (!is.null(dots$location) && !is.character(location)) {
				if (!inherits(dots$location, 'character')) stop('Option <location> must be a character string. The default is ', .locationDefault(), '.')
				if (length(dots$location) != 1L) stop('Option <location> must be a single character string. The default is ', .locationDefault(), '.')
			}
		}

		if (any(names(dots) %in% 'cores')) {
			if (!is.numeric(dots$cores) | (dots$cores <= 0 & dots$cores %% 1 != 0)) stop('Option <cores> must be an integer >= 1. The default is ', .coresDefault(), '.')
		}

		if (any(names(dots) %in% 'memory')) {
			if (!is.numeric(dots$memory)) stop('Option <memory> must be a positive integer. The default is ', .memoryDefault(), '.')
			if (dots$memory <= 0) stop('Option <memory> must be a positive numeric value. The default is ', .memoryDefault(), '.')
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
				stop('Option grassVer must be the version number of GRASS installed on your system. The default is ', .grassVerDefault(), '.')
			}
			dots$grassVer <- as.character(dots$grassVer)
			dots$grassVer <- gsub(dots$grassVer, pattern='.', replacement='', fixed=TRUE)
			if (nchar(dots$grassVer) != 2L) stop('Please supply grassVer as a character string (including the trailing zero if 8.0).\n  Do not include the minor version (e.g., 8.2, not 8.2.1).\n  The default is ', .grassVerDefault(), '.')
			if (as.numeric(substr(dots$grassVer, 1, 1) < 8)) stop('fasterRaster and its supporting package rgrass only supports GRASS GIS version 8.0 or above.\n  The default is ', .grassVerDefault(), '.')
		}

		### set the options
		if (length(dots) > 0L) {

			charOpts <- .namesOfOptions('character')

			for (opt in names(dots)) {

				ex <- if (opt %in% charOpts) {
					paste0('local({', opt, ' = "', dots[[opt]], '"}, envir=.fasterRaster)')
				} else {
					paste0('local({', opt, ' = ', dots[[opt]], '}, envir=.fasterRaster)')
				}
				eval(str2expression(ex))
			}

		}


	} else {
		warning('Cannot set an option and use restore = TRUE at the same time.\n  No options have been changed.')
	}

	invisible(out)

}

#' @name getFastOptions
#' @title Report arguments shared across functions
#' @rdname setFastOptions
#' @export
getFastOptions <- function(..., default=FALSE) {

	dots <- list(...)
	opts <- if (length(dots) > 0) { unlist(dots) } else { .namesOfOptions() }

	### return default values
	if (default) {

		out <- list()
		for (opt in opts) {

			ex <- paste0(opt, 'Default()')
			out[[length(out) + 1L]] <- eval(str2expression(ex))

		}

	### return options as they currently are
	} else {

		# environment exists?
		if (exists('.fasterRaster', where=globalenv())) {
			out <- list()
			for (opt in opts) {
				if (exists(opt, where=.fasterRaster)) {
					out[[length(out) + 1L]] <- get(opt, envir=.fasterRaster)
					names(out)[length(out)] <- opt
				}
			}
		# no? then warn
		} else {
			warning('GRASS has not been started. Use startFast().')
			out <- NULL
		}

	}

	if (length(out) == 1L) out <- unlist(out)
	out

}

#' Set hidden options
#'
#' These options are hidden to the typical user
#'
#' @param ... Name of option and its value. These include:
#'
#' * `workDir`: Folder in which **GRASS** rasters and vectors are to be saved. The default is to use [tempdir()], which works for most users who will export rasters/vectors they want from the **GRASS** session before quitting **R**.
#'
#' * `location`: A **GRASS** location is a set of rasters and/or vectors that have the same map projection. They can represent the same actual location on the Earth, or they can represent different places. Generally, mosyt users will not need to worry about this setting and should ignore it. The default value is `'default'`.
#'
#' @keywords internal
.setHiddenOptions <- function(
	...,
	restore = FALSE
) {

	dots <- list(...)
	opts <- .namesOfHiddenOptions()

	out <- .getHiddenOptions()

	### reset all options
	if (length(dots) == 0L & restore) {

		# return existing values
		if (exists('.fasterRaster', where=globalenv())) {

			# return existing values
			out <- .getHiddenOptions()

			# reset
			for (opt in opts) {

				ex <- paste0('local({', opt, ' <- .', opt, 'Default()}, envir=.fasterRaster)')
				eval(str2expression(ex))

			}

		} else {
			out <- NULL
			warning('GRASS has not been initialized yet. Please use startFast().')
		}

	### setting one or more options
	} else if (length(dots) > 0L & !restore) {

		### set the options
		if (length(dots) > 0L) {

			charOpts <- .namesOfHiddenOptions('character')

			for (opt in names(dots)) {

				ex <- if (opt %in% charOpts) {
					paste0('local({', opt, ' = "', dots[[opt]], '"}, envir=.fasterRaster)')
				} else {
					paste0('local({', opt, ' = ', dots[[opt]], '}, envir=.fasterRaster)')
				}
				eval(str2expression(ex))
			}

		}


	} else {
		warning('Cannot set an option and use restore = TRUE at the same time.\n  No options have been changed.')
	}

	invisible(out)

}

# Get HIDDEN options
# @param default Return default values
.getHiddenOptions <- function(..., default=FALSE) {

	dots <- list(...)
	opts <- if (length(dots) > 0) { unlist(dots) } else { .namesOfHiddenOptions() }

	### return options as they currently are
	if (exists('.fasterRaster', where=globalenv())) {

		out <- list()
		for (opt in opts) {
			if (exists(opt, where=.fasterRaster)) {
				out[[length(out) + 1L]] <- get(opt, envir=.fasterRaster)
				names(out)[length(out)] <- opt
			}
		}

	### return default values
	} else if (default) {

		out <- list()
		for (opt in opts) {

			ex <- paste0('.', opt, 'Default()')
			out[[length(out) + 1L]] <- eval(str2expression(ex))

		}

	} else {
		warning('No hidden options set and <default> != TRUE. Nothing to return.')
		out <- NULL
	}

	if (length(out) == 1L) out <- unlist(out)
	out

}
