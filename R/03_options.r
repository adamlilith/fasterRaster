#' Set or get options shared across "fasterRaster" functions
#'
#' @description `faster()` either sets or gets options used across **fasterRaster** functions. Its use can vary:
#' * Get current values of a particular option: Use `faster("option_name")`. Values will remain unchanged.
#' * Get current values of all options: Use `faster()` (no arguments). Values will remain unchanged.
#' * Get default values of a particular option: Use `faster("option_name", default = TRUE)`. Values will remain unchanged.
#' * Get default values of all options: Use `faster(default = TRUE)`. Values will remain unchanged.
#' * Set values of particular options: Use the form `faster(option 1 = value1, option2 = value2)`.
#' * Set all options to their defaults: Use `faster(restore = TRUE)`.
#'
#' You cannot simultaneously get and set options.
#'
#' @param ... Either:
#' * A character vector: Name(s) of option(s) to get values of;
#' * An option and the value of the option using an `option = value` pattern; or
#' * A names `list` with names that match the options you wish to change, and with values to assign to each option.
#'
#' Options include:
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
#' * `memory` (integer/numeric): The amount of memory to allocate to a task, in GB, for **GRASS**. The default is 2048 MB (i.e., 2 GB). Some **GRASS** modules can take advantage of more memory.
#'
#' * `clean` (logical): If `TRUE` (default), remove temporary files created internally by functions. If not deleted, they can eventually fill up hard drive space, but deleting them takes a little bit of time (usually <1 second for each function).
#'
#' * `useDataTable` (logical): If `FALSE` (default), functions that return tabular output produce `data.frame`s. If `TRUE`, output will be `data.table`s from the **data.table** package. This can be much faster, but it might require you to know how to use `data.table`s if you want to manipulate them in **R**. You can always convert them to `data.frame`s using [base::as.data.frame()].
#' 
#' * `verbose` (logical): If `TRUE`, show **GRASS** messages and otherwise hidden slots in classes. This is mainly used for debugging, so most users will want to keep this at its default, `FALSE`.
#'
#'  * `workDir` (character): The folder in which **GRASS** rasters, vectors, and other objects are created and manipulated. By default, this is given by [tempdir()].
#'
#' @param restore Logical: If `TRUE`, the all options will be reset to their default values. The default is `FALSE`.
#'
#' @param default Logical: Return the default value(s) of the option(s). The default value of `default` is `FALSE`.
#'
#' @return If options are changed, then a named list of option values *before* they were changed is returned invisibly.
#'
#' If option values are requested, a named list with option values is returned (not invisibly).
#'
#' @example man/examples/ex_options.r
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
	if (length(opts) == 1L && is.list(opts[[1L]])) opts <- opts[[1L]]
	# if (length(opts) == 1L && inherits(opts[[1L]], "list")) opts <- opts[[1L]]
	# # if (length(opts) == 1L && is.null(names(opts))) names(opts) <- "grassDir"
	# # if (length(opts) > 1L && any(names(opts) == "")) names(opts)[names(opts) == ""] <- "grassDir"
	nOpts <- length(opts)
	
	if (default & restore) {
		stop("Cannot request default values and restore to default values at the same time.")
	} else if (nOpts > 0L & restore) {
		stop("Cannot simultaneously set options and restore options to default values.")
	}

	# getting options if ... has no names
	gettingOpts <- is.null(names(opts)) & !restore

	# retrieve options
	if (gettingOpts) {
		out <- .getFastOptions(..., default = default)
		out
	} else {
		# out <- .setFastOptions(..., restore = restore)
		out <- .setFastOptions(opts, restore = restore)
		invisible(out)
	}

}

#' Set arguments shared across functions
#' @noRd
.setFastOptions <- function(
	opts,
	restore = FALSE
) {

	# opts <- list(...)

	namesOfOpts <- .namesOfOptions()
	if (!is.null(opts) && any(!(names(opts) %in% namesOfOpts))) stop("Invalid option(s): ", paste(names(opts[!(opts %in% namesOfOpts)]), collapse=", "))
	
	# retrieve in case we want to reset them
	out <- faster()

	### check for validity
	error <- paste0("Option `grassDir` must be NULL (which is likely to fail)\n  or a single character string. The default is `", .grassDirDefault(), "`.")
	if (any(names(opts) %in% "grassDir")) {
		if (!is.na(opts$grassDir)) {
   			if (!is.character(opts$grassDir) || length(opts$grassDir) != 1L) stop(error)
		}
		if (!file.exists(opts$grassDir)) {
			opts$grassDir <- NA_character_
			warning("`grassDir` is invalid. This directory does not exist. Value has been set to NA.")
		}
	}

	error <- paste0("Option `addonsDir` must be NULL or a single character string. The default is `", .addonsDirDefault(), "`.")
	if (any(names(opts) %in% "addonsDir")) {
		if (!is.na(opts$addonsDir)) {
   			if (!is.character(opts$addonsDir) || length(opts$addonsDir) != 1L) stop(error)
		}

	} else if ((any(names(opts) %in% "grassDir") && !is.na(opts$grassDir)) & (is.null(.fasterRaster$options$addonsDir) || is.na(.fasterRaster$options$addonsDir))) {
	
		opts$addonsDir <- file.path(opts$grassDir, "addons")
	
	}

	error <- paste0("Option `workDir` must be a single character string. The default is\n  ", .workDirDefault(), ".")
	if (any(names(opts) %in% "workDir")) {

  		if (!is.character(opts$workDir) || length(opts$workDir) != 1L) stop(error)

		workDir <- omnibus::forwardSlash(opts$workDir)

	}

	# if (any(names(opts) %in% "nAtATime")) {
	# 	if (!is.numeric(opts$nAtATime) | (opts$nAtATime <= 0 & !omnibus::is.wholeNumber(opts$nAtATime))) stop("Option `nAtATime` must be an integer >= 1. The default is ", .nAtATimeDefault(), ".")
	# }

	if (any(names(opts) %in% "cores")) {
		if (!inherits(opts$cores, c("integer", "numeric")) || opts$cores <= 0 || !omnibus::is.wholeNumber(opts$cores)) stop("Option `cores` must be an integer >= 1. The default is ", .coresDefault(), ".")
	}

	if (any(names(opts) %in% "clean")) {
  		if (is.na(opts$clean) || !is.logical(opts$clean)) stop("Option `clean` must be a logical. The default is ", .cleanDefault(), ".")
	}

	if (any(names(opts) %in% "verbose")) {
  		if (is.na(opts$verbose) || !is.logical(opts$verbose)) stop("Option `verbose` must be a logical. The default is ", .verboseDefault(), ".")
	}

	if (any(names(opts) %in% "memory")) {
		if (!is.numeric(opts$memory) || opts$memory <= 0) stop("Option `memory` must be a positive number. The default is ", .memoryDefault(), " (GB).")
	}

	if (any(names(opts) %in% "useDataTable")) {

		if (is.na(opts$useDataTable) || !is.logical(opts$useDataTable)) stop("Option `useDataTable` must be a logical. The default is ", .useDataTableDefault(), ".")
	
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

	if (any(names(opts) %in% "verbose")) {
		info <- rgrass::set.echoCmdOption(.fasterRaster$options$verbose)
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
