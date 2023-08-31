#' Summary statistics for GRasters
#'
#' @description `global()` calculates a summary statistic across all the cells of a `GRaster`. It returns a single value for each layer of the raster.
#'
#' @param x A `GRaster` or missing.  If missing, then a vector of all of the accepted function names is returned.
#'
#' @param fun Character vector: The name of the function(s):
#' * `"countNA"`: Total number of `NA` cells.
#' * `"countNonNA"`: Total number of non-`NA` cells.
#' * `"cv"`: Sample coefficient of variation (expressed as a proportion of the mean).
#' * `"cvpop"`: Population coefficient of variation (expressed as a proportion of the mean).
#' * `"max"` and `"min"`: Highest and lowest values across non-`NA` cells.
#' * `"mean"` (default): Average.
#' * `"meanAbs"`: Mean of absolute values.
#' * `"median"`: Median.
#' * `"quantile"`: Quantile (see also argument `prob`).
#' * `"range"`: Range.
#' * `"sd"`: Sample standard deviation.
#' * `"sdpop"`: Population standard deviation.
#' * `"sum"`: Sum.
#' * `"var"`: Sample variance.
#' * `"varpop"`: Population variance.
#'
#' @param prob Numeric: Quantile at which to calculate `quantile`. Only a single value between 0 and 1 is allowed.
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
		prob = NULL,
		...
	) {
	
	funs <- c("sum", "mean", "median", "min", "max", "cv", "cvpop", "meanAbs", "countNA", "countNonNA", "range", "sd", "var", "sdpop", "varpop", "quantile")
	funNames <- fun
	funs <- tolower(funs)
	fun <- tolower(fun)
	
	if (any(fun == "quantile") & is.null(prob)) stop("You must specify a value for ", sQuote("prob"), " when calculating quantile statistics.")
	
	nLayers <- nlyr(x)
	out <- data.frame()

	### for each layer
	for (i in seq_len(nLayers)) {

		args <- list(
			cmd = "r.univar",
			flags = c("quiet", "r"),
			map = sources(x)[i],
			Sys_show.output.on.console = FALSE,
			echoCmd = FALSE,
			intern = TRUE
		)

		if (getFastOptions("grassVer") >= "8.3") args$nprocs <- getFastOptions("cores")
	
		if (any(fun %in% c("quantile", "median"))) args$flags <- c(args$flags, "e")
		if (any(fun == "quantile")) {
			if (prob < 0 | prob > 1) stop("Values of ", sQuote("prob"), " must be in the range [0, 1].")
			perc <- 100 * prob
			args <- c(args, percentile=perc)
		}

		info <- do.call(rgrass::execGRASS, args)
		
		# values for this layer
		thisOut <- data.frame(matrix(NA_real_, ncol=length(fun), nrow=1L), row.names=names(x)[i])
		names(thisOut) <- funNames
			
		### for each function
		for (countFun in seq_along(fun)) {
		
			thisFun <- fun[countFun]
			funName <- funNames[countFun]
			matchFun <- pmatchSafe(thisFun, funs)

			pattern <- if (matchFun == "meanabs") {
				"mean of absolute values: "
			} else if (matchFun == "mean") {
				"mean: "
			} else if (matchFun == "median") {
				"median \\(even number of cells\\): "
			} else if (matchFun == "min") {
				"minimum: "
			} else if (matchFun == "max") {
				"maximum: "
			} else if (matchFun == "countna") {
				"total null cells: "
			} else if (matchFun == "range") {
				"range: "
			} else if (matchFun == "sd") {
				"standard deviation: "
			} else if (matchFun == "var") {
				"variance: "
			} else if (matchFun == "sum") {
				"sum: "
			} else { 
				NA
			}
			
			if (!is.na(pattern)) {
			
				this <- info[grepl(info, pattern=pattern)]
				this <- sub(this, pattern=pattern, replacement="")
				this <- as.numeric(this)
			
			} else {
			
				if (matchFun == "quantile") {
				
					pattern <- "percentile: "
					this <- info[grepl(info, pattern=pattern)]
					this <- strsplit(this, split=":")[[1L]][2L]
					this <- as.numeric(this)
					
				} else if (matchFun == "countnonna") {
				
					pattern <- "total null and non-null cells: "
					this1 <- info[grepl(info, pattern=pattern)]
					this1 <- sub(this1, pattern=pattern, replacement="")
					this1 <- as.numeric(this1)
					
					pattern <- "total null cells: "
					this2 <- info[grepl(info, pattern=pattern)]
					this2 <- sub(this2, pattern=pattern, replacement="")
					this2 <- as.numeric(this2)
					
					this <- this1 - this2
					
				} else if (matchFun == "cv") {
				
					pattern <- "variation coefficient: "
					this <- info[grepl(info, pattern=pattern)]
					this <- sub(this, pattern=pattern, replacement="")
					this <- sub(this, pattern="%", replacement="")
					this <- as.numeric(this)
					this <- this / 100
					
				} else if (thisFun == "cvpop") {
				
					pattern <- "mean: "
					mean. <- info[grepl(info, pattern=pattern)]
					mean. <- sub(mean., pattern=pattern, replacement="")
					mean. <- as.numeric(mean.)

					gnSS <- .makeSourceName("delta", "rast")
					ex <- paste0(gnSS, " = (", sources(x)[i], " - ", mean., ")^2")
					rgrass::execGRASS("r.mapcalc", expression=ex, flags=c("quiet", "overwrite"), intern=TRUE)
				
					thisInfo <- rgrass::execGRASS(
						cmd = "r.univar",
						flags = c("r", "quiet"),
						map = gnSS,
						nprocs = getFastOptions("cores"),
						Sys_show.output.on.console = FALSE,
						echoCmd = FALSE,
						intern = TRUE
					)

					if (getFastOptions("grassVer") >= "8.3") args$nprocs <- getFastOptions("cores")

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
					stdev <- sqrt(ss / n)
					this <- stdev / mean.

				} else if (thisFun %in% c("varpop", "sdpop")) {
				
					pattern <- "mean: "
					mean. <- info[grepl(info, pattern=pattern)]
					mean. <- sub(mean., pattern=pattern, replacement="")
					mean. <- as.numeric(mean.)

					gnSS <- .makeSourceName("delta", "rast")
					ex <- paste0(gnSS, " = (", sources(x)[i], " - ", mean., ")^2")
					rgrass::execGRASS("r.mapcalc", expression=ex, flags=c("quiet", "overwrite"), intern=TRUE)
				
					thisInfo <- rgrass::execGRASS(
						cmd = "r.univar",
						flags = c("r", "quiet"),
						map = gnSS,
						nprocs = getFastOptions("cores"),
						Sys_show.output.on.console = FALSE,
						echoCmd = FALSE,
						intern = TRUE
					)

					if (getFastOptions("grassVer") >= "8.3") args$nprocs <- getFastOptions("cores")

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
					this <- ss / n
					if (thisFun == "sdpop") this <- sqrt(this)

				} # custom function
				
			} # match function
			
			thisOut[1L, funName] <- this
				
		} # next function

		out <- rbind(out, thisOut)
		
	} # next layer

	rownames(out) <- names(x)
	out
	
	} # EOF
)


#' @aliases global
#' @rdname global
#' @exportMethod global
methods::setMethod(
	f = "global",
	signature = c(x = "missing"),
	definition = function(x, ...) {
	
	c(
		"countNA",
		"countNonNA",
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
