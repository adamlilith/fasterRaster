#' Summary statistics for GRasters
#'
#' @description `global()` calculates a summary statistic across all the cells of a `GRaster`. It returns a single value for each layer of the raster.
#'
#' @param x A `GRaster` or missing.  If missing, then a vector of all of the accepted function names is returned.
#'
#' @param fun Character vector: The name of the function(s):
#' * `"*"`: All of the functions below.
#' * `"cv"`: Sample coefficient of variation (expressed as a proportion of the mean).
#' * `"cvpop"`: Population coefficient of variation (expressed as a proportion of the mean).
#' * `"max"` and `"min"`: Highest and lowest values across non-`NA` cells. NB: [minmax()] is faster.
#' * `"mean"` (default): Average.
#' * `"meanAbs"`: Mean of absolute values.
#' * `"median"`: Median.
#' * `"quantile"`: Quantile (see also argument `probs`).
#' * `"range"`: Range. Note that following [terra::global()], the minimum and maximum are reported, not the actual range.
#' * `"sd"`: Sample standard deviation (same as [stats::sd()]).
#' * `"sdpop"`: Population standard deviation.
#' * `"sum"`: Sum.
#' * `"var"`: Sample variance (same as [stats::var()]).
#' * `"varpop"`: Population variance.
#'
#' @param probs Numeric within the range from 0 to 1: Quantile(s) at which to calculate `quantile`.
#'
#' @param ... Other arguments (unused).
#'
#' @returns If `x` is missing, the function returns a character vector of all accepted function names. If `x` is a `GRaster`, a data frame with the specified statistics is returned.
#'
#' @seealso [terra::global()] and **GRASS** module `r.univar`
#'
#' @example man/examples/ex_global.r
#'
#' @aliases global
#' @rdname global
#' @exportMethod global
methods::setMethod(
	f = "global",
	signature = c(x = "GRaster"),
	definition = function(
		x,
		fun = "mean",
		probs = seq(0, 1, 0.25),
		...
	) {

	.global(x = x, fun = fun, probs = probs)

	} # EOF
)

# x 		A `GRaster` or the [sources()] name of one
# fun 		character
# probs		numeric in [0, 1]
#' @noRd
.global <- function(x, fun, probs = seq(0, 1, 0.25)) {

	if (inherits(x, "GRaster")) {
		.locationRestore(x)
		.region(x)
		src <- sources(x)
	} else {
		src <- x
	}

	if (any(fun == "*")) fun <- global()
	funs <- global()
	fun <- omnibus::pmatchSafe(fun, funs, useFirst = TRUE, error = TRUE)

	if (any(fun == "range")) {
		if (length(fun) == 1L) {
			fun <- c("min", "max")
		} else {
			where <- which(fun == "range")
			lf <- length(fun)
			if (where == 1L) {
				fun <- c("min", "max", fun[2L:lf])
			} else if (where == lf) {
				fun <- c(fun[1L:(lf - 1L)], "min", "max")
			} else {
				fun <- c(fun[1L:(where - 1L)], "min", "max", fun[(where + 1L):lf])
			}
		}
	}

	fun <- unique(fun)
	if (any(fun == "quantile") && any(probs < 0) | any(probs > 1)) stop("The value for `probs` must be in the range [0, 1].")
	
	versionNumber <- grassInfo("versionNumber")

	out <- data.frame()
	nLayers <- length(src)

	### for each layer
	for (i in seq_len(nLayers)) {

		if (any(fun == "quantile")) {

			# NB using r.quantile() can be **much** faster than using "r.univar" to get quantiles!!! (minutes vs weeks)

			percents <- 100 * probs
			
			quantInfo <- rgrass::execGRASS(
				cmd = 'r.quantile',
				input = src[i],
				percentiles = percents,
				flags = c(.quiet(), "overwrite"),
				Sys_show.output.on.console = FALSE,
				echoCmd = FALSE,
				intern = TRUE
			)

		}

		if (any(fun != "quantile")) {

			args <- list(
				cmd = "r.univar",
				map = src[i],
				flags = c(.quiet(), "r"),
				Sys_show.output.on.console = FALSE,
				echoCmd = FALSE,
				intern = TRUE
			)

			if (versionNumber >= 8.3) args$nprocs <- faster("cores")
			if (any(fun == "median")) args$flags <- c(args$flags, "e")

			info <- do.call(rgrass::execGRASS, args)

		}
			
		# values for this layer
		nc <- length(fun)
		if (any(fun == "quantile")) nc <- nc + length(probs) - 1
		thisOut <- data.frame(
			matrix(NA_real_,
				ncol = nc,
				nrow = 1L
			),
			row.names = src[i]
		)
		
		if (any(fun == "quantile")) {
			
			colNames <- fun
			isQuant <- which(colNames == "quantile")
			quantNames <- paste0("quantile_", probs)
			if (length(fun) == 1L & fun[1L] == "quantile") {
				colNames <- quantNames
			} else if (colNames[1L] == "quantile") {
				colNames <- c(quantNames, colNames[2L:length(colNames)])
			} else if (colNames[length(colNames)] == "quantile") {
				colNames <- c(colNames[1L:(length(colNames) - 1L)], quantNames)
			}
			names(thisOut) <- colNames
			
		} else {
			names(thisOut) <- fun
		}
			
		### for each function
		for (countFun in seq_along(fun)) {

			thisFun <- fun[countFun]

			pattern <- if (thisFun == "meanAbs") {
				"mean of absolute values: "
			} else if (thisFun == "mean") {
				"mean: "
			} else if (thisFun == "min") {
				"minimum: "
			# } else if (thisFun == "countNA") {
				# "total null cells: "
			} else if (thisFun == "range") {
				"range: "
			} else if (thisFun == "sdpop") {
				"standard deviation: "
			} else if (thisFun == "varpop") {
				"variance: "
			} else if (thisFun == "sum") {
				"sum: "
			} else { 
				NA
			}
			
			if (!is.na(pattern)) {
			
				this <- info[grepl(info, pattern=pattern)]
				this <- sub(this, pattern = pattern, replacement = "")
				this <- as.numeric(this)

			} else {
			
				# r.stats returns "nan" if all vales on the raster are the same... but the minimum is the given value, so use that
				if (thisFun == "max") {
				
					pattern <- "maximum: "
					this <- info[grepl(info, pattern = pattern)]
					this <- strsplit(this, split=":")[[1L]][2L]
					this <- as.numeric(this)

					if (is.nan(this)) {
						pattern <- "minimum: "
						this <- info[grepl(info, pattern = pattern)]
						this <- strsplit(this, split=":")[[1L]][2L]
						this <- as.numeric(this)
					}
					
				} else if (thisFun == "quantile") {
				
					# pattern <- "percentile: "
					# this <- info[grepl(info, pattern=pattern)]
					# this <- strsplit(this, split=":")[[1L]][2L]
					# this <- as.numeric(this)

					this <- strsplit(quantInfo, split = ":")
					this <- lapply(this, as.numeric)
					this <- do.call(rbind, this)
					this <- this[ , 3L, drop = TRUE]
					
				} else if (thisFun == "median") {
				
					pattern <- "median \\(even number of cells\\): "

					this <- info[grepl(info, pattern = pattern)]

					if (length(this) == 0L) {

						pattern <- "median \\(odd number of cells\\): "

						this <- info[grepl(info, pattern = pattern)]

					}

					this <- sub(this, pattern = pattern, replacement = "")
					this <- as.numeric(this)

				# } else if (thisFun == "countNonNA") {
				
				# 	pattern <- "total null and non-null cells: "
				# 	this1 <- info[grepl(info, pattern=pattern)]
				# 	this1 <- sub(this1, pattern=pattern, replacement="")
				# 	this1 <- as.numeric(this1)
					
				# 	pattern <- "total null cells: "
				# 	this2 <- info[grepl(info, pattern=pattern)]
				# 	this2 <- sub(this2, pattern=pattern, replacement="")
				# 	this2 <- as.numeric(this2)
					
				# 	this <- this1 - this2
					
				} else if (thisFun == "cvpop") {
				
					pattern <- "variation coefficient: "
					this <- info[grepl(info, pattern=pattern)]
					this <- sub(this, pattern=pattern, replacement="")
					this <- sub(this, pattern="%", replacement="")
					this <- as.numeric(this)
					this <- this / 100
					
				} else if (thisFun == "cv") {
				
					pattern <- "mean: "
					mean. <- info[grepl(info, pattern = pattern)]
					mean. <- sub(mean., pattern = pattern, replacement="")
					mean. <- as.numeric(mean.)

					srcSS <- .makeSourceName("r_mapcalc", "rast")
					ex <- paste0(srcSS, " = (", sources(x)[i], " - ", mean., ")^2")
					
					rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
				
					args <- list(
						cmd = "r.univar",
						flags = c("r", .quiet()),
						map = srcSS,
						nprocs = faster("cores"),
						Sys_show.output.on.console = FALSE,
						echoCmd = FALSE,
						intern = TRUE
					)

					if (versionNumber >= 8.3) args$nprocs <- faster("cores")

					thisInfo <- do.call(rgrass::execGRASS, args = args)
					if (faster("clean")) on.exit(.rm(srcSS, type = "raster", warn = FALSE), add = TRUE)

					pattern <- "sum: "
					ss <- thisInfo[grepl(info, pattern=pattern)]
					ss <- sub(ss, pattern=pattern, replacement="")
					ss <- as.numeric(ss)

					pattern <- "total null and non-null cells: "
					n1 <- thisInfo[grepl(thisInfo, pattern=pattern)]
					n1 <- sub(n1, pattern=pattern, replacement="")
					n1 <- as.numeric(n1)
					
					pattern <- "total null cells: "
					n2 <- thisInfo[grepl(thisInfo, pattern=pattern)]
					n2 <- sub(n2, pattern=pattern, replacement="")
					n2 <- as.numeric(n2)
					
					n <- n1 - n2
					stdev <- sqrt(ss / (n - 1))
					this <- stdev / mean.

				} else if (thisFun %in% c("var", "sd")) {
	
					pattern <- "mean: "
					mean. <- info[grepl(info, pattern = pattern)]
					mean. <- sub(mean., pattern = pattern, replacement="")
					mean. <- as.numeric(mean.)

					srcSS <- .makeSourceName("r_mapcalc", "raster")
					
					ex <- paste0(srcSS, " = (", src[i], " - ", mean., ")^2")

					rgrass::execGRASS(
						cmd = "r.mapcalc",
						expression = ex,
						flags = c(.quiet(), "overwrite")
					)

					args <- list(
						cmd = "r.univar",
						flags = c("r", .quiet()),
						map = srcSS,
						nprocs = faster("cores"),
						Sys_show.output.on.console = FALSE,
						echoCmd = FALSE,
						intern = TRUE
					)

					if (grassInfo("versionNumber") >= 8.3) args$nprocs <- faster("cores")

					thisInfo <- do.call(rgrass::execGRASS, args = args)
					if (faster("clean")) on.exit(.rm(srcSS, type = "raster", warn = FALSE), add = TRUE)

					pattern <- "sum: "
					ss <- thisInfo[grepl(info, pattern=pattern)]
					ss <- sub(ss, pattern=pattern, replacement="")
					ss <- as.numeric(ss)

					# NB need to convert to numeric to obviate overflow
					n <- .nonnacell(x[[i]], warn = FALSE, nLayers = 1)

					this <- ss / (n - 1)
					if (thisFun == "sd") this <- sqrt(this)

				} # custom function
				
			} # match function
			
			if (fun[countFun] != "quantile") {
				thisOut[1L, fun[countFun]] <- this
			} else {
				thisOut[1L, match(paste0("quantile_", probs), colnames(thisOut))] <- this
			}
				
		} # next function

		out <- rbind(out, thisOut)

	} # next layer

	rownames(out) <- names(x)
	out

}

#' @aliases global
#' @rdname global
#' @exportMethod global
methods::setMethod(
	f = "global",
	signature = c(x = "missing"),
	definition = function(x, ...) {
	
	c(
		# "countNA",
		# "countNonNA",
		"cv",
		"cvpop",
		"mean",
		"meanAbs",
		"median",
		"min",
		"max",
		"range",
		"sd",
		"sdpop",
		"sum",
		"var",
		"varpop",
		"quantile"
	)

	} # EOF
)
