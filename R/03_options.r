#' Set or get options shared across "fasterRaster" functions
#'
#' @description `faster()` either sets or gets options used across **fasterRaster** functions. If the `default` argument is `TRUE`, then the default value(s) of options is returned (but value(s) are kept unchanged). If the `restore` argument is `TRUE`, then all options will be set to their default values. You cannot simultaneously set options and restore their default values.
#'
#' @param ... Either a character (the name of an option), or an option and the value of the option using an `option = value` pattern. These include:
#'
#' * `grassDir` (character): The folder in which **GRASS** is installed on your computer. Typically, this option is set when you run [faster()]. Depending on your operating system, your install directory will look something like this:
#'     * Windows: `"C:/Program Files/GRASS GIS 8.3"`
#'     * Mac OS: `"/Applications/GRASS-8.3.app/Contents/Resources"`
#'     * Linux: `"/usr/local/grass"`
#'
#' *  `addonsDir` (character): Folder in which **GRASS** addons are stored. If `NA` and `grassDir` is not `NA`, this will be assumed to be `file.path(grassDir, "addons")`. The default values is `NA`.
#'
#' * `cores` (integer/numeric integer): Number of processor cores to use on a task. The default is 2. Some **GRASS** modules are parallelized.
#'
#' * `correct` (logical): Should the topology of `GVectors` be corrected automatically when they are created using [fast()]? By default, this is `TRUE`, which will not necessarily produce vectors commensurate with [terra::vect()]. Topology correction can take a long time, but skipping it can yield errors in some geographic operations. You can set this globally for every use of [fast()] using `faster(correct = <TRUE/FALSE>)`, or override the global setting on a case-by-case basis when using `fast()` by using the `correct` argument for that function. See [vector topology][tutorial_vector_topology] for more information.
#'
#' * `memory` (integer/numeric): The amount of memory to allocate to a task, in GB, for **GRASS**. The default is 1024 MB (i.e., 1 GB). Some **GRASS** modules can take advantage of more memory.
#'
#' * `rasterPrecision` (character): The [precision][tutorial_raster_data_types] of values when applying mathematical operations to a `GRaster`. By default, this is `"double"`, which allows for precision to about the 16th decimal place. However, it can be set to `"float"`, which allows for precision to about the 7th decimal place. `float` rasters are smaller in memory and on disk. The default is `"double"`.`
#'
#' * `useDataTable` (logical): If `FALSE` (default), use `data.frame`s when going back and forth between data tables of `GVector`s and **R**. This can be slow for very large data tables. If `TRUE`, use `data.table`s from the **data.table** package. This can be much faster, but it might require you to know how to use `data.table`s if you want to manipulate them in **R**. You can always convert them to `data.frame`s using [base::as.data.frame()].
#' 
#' * `verbose` (logical): If `TRUE`, show **GRASS** messages and otherwise hidden slots in classes. This is mainly used for debugging, so most users will want to keep this at its default, `FALSE`.
#'
#'  * `workDir` (character): The folder in which **GRASS** rasters, vectors, and other objects are created and manipulated. By default, this is given by [tempdir()].
#'
#' @param restore Logical: If `TRUE`, the all options will be reset to their default values. The default is `FALSE`.
#'
#' @param default Logical: Return the default value(s) of the option(s). The default value of `default` is `FALSE`.
#'
#' @return If values of options changed, then a named list of option values *before* they were changed is returned invisibly.
#'
#' If option values are requested, a named list with option values is returned (not invisibly).
#'
#' @example man/examples/ex_faster.r
#'
#' @aliases faster
#' @rdname faster
#' @export
faster <- function(
	...,
	default = FALSE,
	restore = FALSE
) {

	opts <- list(...)
	if (length(opts) == 1L && inherits(opts[[1L]], "list")) opts <- opts[[1L]]
	# # if (length(opts) == 1L && is.null(names(opts))) names(opts) <- "grassDir"
	# # if (length(opts) > 1L && any(names(opts) == "")) names(opts)[names(opts) == ""] <- "grassDir"
	nd <- length(opts)
	
	if (default & restore) {
		
		stop("Cannot request default values and restore to default values at the same time.")

	} else if (nd > 0L & restore) {
	
		stop("Cannot simultaneously set options and restore options to default values.")

	}

	# getting options if ... has no names
	gettingOpts <- is.null(names(opts)) & !restore

	# retrieve options
	if (gettingOpts) {
		out <- .getFastOptions(..., default = default)
		out
	} else {
		out <- .setFastOptions(..., restore = restore)
		invisible(out)
	}

}

#' Set arguments shared across functions
#' @noRd
.setFastOptions <- function(
	...,
	restore = FALSE
) {

	opts <- list(...)

	namesOfOpts <- .namesOfOptions()
	if (!is.null(opts) && any(!(names(opts) %in% namesOfOpts))) stop("Invalid option(s): ", paste(names(opts[!(opts %in% namesOfOpts)]), collapse=", "))
	
	# retrieve in case we want to reset them
	out <- faster()

	### check for validity
	error <- paste0("Option ", sQuote("grassDir"), " must be ", dQuote("NULL"), " (which is likely to fail)\n  or a single character string. The default is ", dQuote(.grassDirDefault()), ".")
	if (any(names(opts) %in% "grassDir")) {
		if (!is.na(opts$grassDir)) {
   			if (!is.character(opts$grassDir) || length(opts$grassDir) != 1L) stop(error)
		}
		if (!file.exists(opts$grassDir)) {
			opts$grassDir <- NA_character_
			warning(sQuote("grassDir"), " invalid. This directory does not exist. Value has been set to NA.")
		}
	}

	error <- paste0("Option ", sQuote("addonsDir"), " must be ", sQuote("NULL"), " or a single character string. The default is ", dQuote(.addonsDirDefault()), ".")
	if (any(names(opts) %in% "addonsDir")) {
		if (!is.na(opts$addonsDir)) {
   			if (!is.character(opts$addonsDir) || length(opts$addonsDir) != 1L) stop(error)
		}

	} else if ((any(names(opts) %in% "grassDir") && !is.na(opts$grassDir)) & (is.null(.fasterRaster$options$addonsDir) || is.na(.fasterRaster$options$addonsDir))) {
	
		opts$addonsDir <- file.path(opts$grassDir, "addons")
	
	}

	error <- paste0("Option ", sQuote("workDir"), " must be a single character string. The default is\n  ", dQuote(.workDirDefault()), ".")
	if (any(names(opts) %in% "workDir")) {

  		if (!is.character(opts$workDir) || length(opts$workDir) != 1L) stop(error)

		workDir <- forwardSlash(workDir)

	}

	# error <- paste0("Option ", sQuote("location"), " must be a single character string. The default is ", dQuote(.locationDefault()), ".")
	# if (any(names(opts) %in% "location")) {
  	# 	if (!is.character(opts$location) || length(opts$location) != 1L) stop(error)
	# }

	# error <- paste0("Option ", sQuote("mapset"), " must be a single character string. The default is ", dQuote(.mapsetDefault()), ".")
	# if (any(names(opts) %in% "mapset")) {
  	# 	if (!is.character(opts$mapset) || length(opts$mapset) != 1L) stop(error)
	# }

	if (any(names(opts) %in% "cores")) {
		if (!is.numeric(opts$cores) | (opts$cores <= 0 & opts$cores %% 1 != 0)) stop("Option ", sQuote("cores"), " must be an integer >= 1. The default is ", .coresDefault(), ".")
	}

	if (any(names(opts) %in% "correct")) {
  		if (is.na(opts$correct) || !is.logical(opts$correct)) stop("Option ", sQuote("correct"), " must be a logical. The default is ", .correctDefault(), ".")
	}

	if (any(names(opts) %in% "verbose")) {
  		if (is.na(opts$verbose) || !is.logical(opts$verbose)) stop("Option ", sQuote("verbose"), " must be a logical. The default is ", .verboseDefault(), ".")
	}

	if (any(names(opts) %in% "memory")) {
		if (!is.numeric(opts$memory) || opts$memory <= 0) stop("Option ", sQuote("memory"), " must be a positive number. The default is ", .memoryDefault(), " (GB).")
	}

	# if (any(names(opts) %in% "autoRegion")) {
	# 	if (!is.logical(opts$autoRegion)) stop("Option ", sQuote("autoRegion"), " must be a logical. The default is ", .autoRegionDefault(), ".")
	# 	if (is.na(opts$autoRegion)) stop("Option ", sQuote("autoRegion"), " must be TRUE or FALSE (not NA). The default is ", .autoRegionDefault(), ".")
	# }

	if (any(names(opts) %in% "rasterPrecision")) {
	
		if (is.na(opts$rasterPrecision) || !is.character(opts$rasterPrecision)) stop("Option ", sQuote("rasterPrecision"), " must be ", sQuote("float"), ") or ", sQuote("double"), ".\n  The default is ", .rasterPrecisionDefault(), ".")

     	opts$rasterPrecision <- omnibus::pmatchSafe(opts$rasterPrecision, c("FCELL", "float", "DCELL", "double"))
		opts$rasterPrecision <- if (opts$rasterPrecision == "FCELL") {
   			"float"
		} else if (opts$rasterPrecision == "DCELL") {
			"double"
		}
	}

	if (any(names(opts) %in% "useDataTable")) {

		if (is.na(opts$useDataTable) || !is.logical(opts$useDataTable)) stop("Option ", sQuote("useDataTable"), " must be a logical. The default is ", .useDataTableDefault(), ".")
	
	}

	### set the options
	if (length(opts) == 0L) {
		opts <- as.list(namesOfOpts)
		names(opts) <- namesOfOpts
	}
	
	for (opt in names(opts)) {

		# default
		if (restore) {
			val <- paste0(".", opt, "Default()")
			val <- eval(str2expression(val))
		} else {
			val <- opts[[opt]]
		}

		.fasterRaster$options[[opt]] <- val

	}

	if (any(names(opt) %in% "verbose")) {
		rgrass::set.echoCmdOption(opt$verbose)
	}

	invisible(out)

}

#' Report arguments shared across functions
#' @noRd
.getFastOptions <- function(..., default = FALSE) {

	namesOfOpts <- .namesOfOptions()
	
	opts <- unlist(list(...))
	
	if (!is.null(opts) && any(!(opts %in% namesOfOpts))) stop("Invalid option(s): ", paste(opts[!(opts %in% namesOfOpts)], collapse=", "))

	### return default values
	if (default) {

		if (length(opts) == 0L) opts <- namesOfOpts

		out <- list()
		for (opt in opts) {

			ex <- paste0(".", opt, "Default()")
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
