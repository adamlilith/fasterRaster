#' Set or get options shared across 'fasterRaster' functions
#'
#' These functions allows you to either 1) view or 2) define options shared by **fasterRaster** functions.You can view the current default values of all shared **fasterRaster** arguments and options using:\cr
#' ```
#' fastGetOptions()
#' ```
#' or see the value of a particular argument using (for example):
#' ```
#' fastGetOptions('grassDir')
#' fastGetOptions('replace')
#' ```
#' These two functions are a **fasterRaster**-specific wrappers for [base::options()] and [base::getOption()].
#'
#' @param ... Any **fasterRaster** shared argument or option that can be defined using the `<argument/option> = <value>` pattern. These include:
#'
#'	* `grassDir` The folder in which **GRASS** is installed on your computer. By default, this value is \code{NULL}, which causes the function to search for your installation of **GRASS** (and which usually fails). Depending on your operating system, your install directory will look something like this:
#' ```{r eval=FALSE, indent='          '}
#' fastSetOptions(grassDir = 'C:/Program Files/GRASS GIS 8.3') # example for Windows
#' fastSetOptions(grassDir = "/Applications/GRASS-8.3.app/Contents/Resources") # example for a Mac
#' fastSetOptions(grassDir = '/usr/local/grass') # example for Linux
#' ```
#'
#' * `cores`: Number of processor cores to use on a task. The default is 1. Only some **GRASS** modules allow this option.
#'
#' * `memory`: The amount of memory to allocate to a task, in MB. The default is 300 MB. Only some **GRASS** modules allow this option.
#'
#' * `vectClass`: By default, this arguments is `'SpatRaster'`, which causes **fasterRaster** functions that output spatial vectors to produce objects of class `SpatVector` from the **`terra`** package. However, if this argument is `'sf'`, these functions will output `sf` objects from the **`sf`** package.
#'
#' * `grassVer`: The [fasterHelp()] function supplies links to help pages for **GRASS** modules, but these links are version-dependent. You can change the version of **GRASS** you are using with this function, and thus avoid having to define it every time you use a link from [fasterHelp()]. This should be the version number of your **GRASS** installation, as a character (not numeric). The decimal is ignored. Valid examples: `'8.0'` or `'80'`.
#'
#' @param restore If `TRUE`, the all shared arguments and settings will be reset to their default values.
#'
#' @param default Supplies the default value of an option if it does not appear in \code{options}.
#'
#' @return The \code{fastGetOptions} function reports the value of one or more **fasterRaster** settings. The \code{fasterRasterSet} function changes the values of these settings.
#'
#' @example man/examples/ex_fastOptions.r
#'
#' @seealso [base::options()] and [base::getOption()]
#'
#' @export

fastSetOptions <- function(
	...,
	restore = FALSE
) {

	dots <- list(...)

	### get current options
	olds <- list(
		grassDir = getOption('grassDir', .grassDirDefault()),
		rastClass = getOption('rastClass', .rastClassDefault()),
		vectClass = getOption('vectClass', .vectClassDefault()),
		cores = getOption('cores', .coresDefault()),
		memory = getOption('memory', .memoryDefault()),
		grassVer = getOption('grassVer', .grassVerDefault())
	)

	# restore to defaults
	if (length(dots) == 0L & restore) {

		dots <- list(
			grassDir = .grassDirDefault(),
			rastClass = .rastClassDefault(),
			vectClass = .vectClassDefault(),
			cores = .coresDefault(),
			memory = .memoryDefault(),
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

	if (any(names(dots) %in% 'rastClass')) {
		if (
			is.null(dots$rastClass) ||
			is.na(dots$rastClass) ||
			!(dots$rastClass %in% c('SpatRaster', 'stars'))
		) stop('Setting "rastClass" must be either "SpatRaster" or "stars".')
	}

	if (any(names(dots) %in% 'vectClass')) {
		if (
			is.null(dots$vectClass) ||
			is.na(dots$vectClass) ||
			!(dots$vectClass %in% c('SpatVector', 'sf'))
		) stop('Setting "vectClass" must be either "SpatVector" or "sf".')
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

		if (is.null(getOption('faster'))) {
			options(faster = list())
			options(faster$memory = .memoryDefault())
		}

		for (i in seq_along(dots)) {

			arg <- names(dots)[i]
			val <- dots[i]

			# logical or numeric setting?
			ex <- if (arg %in% c('cores', 'memory')) {
				paste0('options(faster$', arg, ' = ', val, ')')
			# string/numeric setting?
			} else {
				paste0('options(faster$', arg, ' = \"', val, '\")')
			}

			print(ex)
			eval(str2expression(ex))

		}

	}

	invisible(olds)

}

#' @name fastGetOptions
#' @title Report arguments shared across functions
#' @rdname fastSetOptions
#' @export
fastGetOptions <- function(
	x,
	default = NULL
) {

	# report all values
	if (missing(x)) {
		out <- list(
			grassDir = getOption('grassDir', .grassDirDefault()),
			rastClass = getOption('rastClass', .rastClassDefault()),
			vectClass = getOption('vectClass', .vectClassDefault()),
			cores = getOption('cores', .coresDefault()),
			memory = getOption('memory', .memoryDefault()),
			grassVer = getOption('grassVer', .grassVerDefault())
		)

	} else {

		opts <- c('grassDir', 'cores', 'memory', 'rastClass', 'vectClass', 'grassVer')

		if (
			is.null(x) ||
			is.na(x) ||
			length(x) < 1L ||
			length(x) > 1L ||
			!(x %in% opts)
		) stop('Invalid value for argument "x". Please see ?fastGetOptions.')

		if (is.null(default)) {

			default <- if (x == 'grassDir') {
				.grassDirDefault()
			} else if (x == 'cores') {
				.coresDefault()
			} else if (x == 'memory') {
				.memoryDefault()
			} else if (x == 'rastClass') {
				.rastClassDefault()
			} else if (x == 'vectClass') {
				.vectClassDefault()
			} else if (x == 'grassVer') {
				.grassVerDefault()
			}

		}

		out <- getOption(x, default=default)

	}

	out

}
