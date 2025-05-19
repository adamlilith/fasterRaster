#' Calculate cell values based on values of nearby cells
#' 
#' @description This function calculates statistics on a moving "neighborhood" of cells of a raster. The neighborhood can be a square, circle, or a user-defined set of cells (with or without weights).
#' 
#' @param x A `GRaster`.
#' 
#' @param w Numeric integer or a square matrix with an odd number of rows and columns: The size and nature of the neighborhood:
#' * "Square" neighborhoods (when `circle = FALSE`): An odd integer >= 3, indicating indicates the size of a "square" neighborhood (number of cells wide and number or cells tall).
#' * "Circular" neighborhoods (when `circle = TRUE`): An odd integer >=3, indicating the diameter of the circle.
#' * A matrix of cell weights: The matrix must be square and have an odd number of rows and columns (example: `matrix(c(0.5, 1, 0.5, 1, 2, 1, 0.5, 1, 0.5), nrow=3)`). You cannot use a weights matrix when `circle = TRUE`. Cells with `NA` as a weight will be ignored. Note that weighted matrices should not be used for function `min`, `max`, `count`, `nunique`, or `interspersion`.
#' 
#' @param fun Character: Name of the function to apply to the neighborhood:
#' * "`mean`" (default)
#' * "`median`"
#' * "`mode`"
#' * "`min`" or "`max`": Minimum or maximum. Should not use a weights matrix.
#' * "`range`": Difference between the maximum and minimum.  Should not use a weights matrix.
#' * "`sd`": Sample standard deviation.  NB: This is the same as the [stats::sd()] function.
#' * "`sdpop`": Population standard deviation. NB: This is the same as the function "stddev" in the **GRASS** tool `r.neighbors`.
#' * "`sum`": Sum of non-`NA`` cells.
#' * "`count`": Number of non-`NA cells. Should not use a weights matrix.
#' * "`var`": Sample variance.  NB: This is the same as the [stats::var()] function.
#' * "`varpop`": Population variance. NB: This is the same as the function "variance" in the **GRASS** tool `r.neighbors`.
#' * "`nunique`": Number of unique values. Should not use a weights matrix.
#' * "`interspersion`": Proportion of cells with values different from focal cell (e.g., if 6 of 8 cells have different values, then the interspersion is 6/8 = 0.75). NB: This is slightly different from how it is defined in the **GRASS** tool `r.neighbors`. Should not use a weights matrix.
#' * "`quantile`": Quantile of values. The value in argument `quantile` is used to specify the quantile.
#' 
#' The center cell value is always included in the calculations, and all calculations ignore `NA` cells (i.e., they do not count as cells in the focal neighborhood).
#'
#' @param circle Logical: If `FALSE` (default), use a square neighborhood. If `TRUE`, use a circular neighborhood. When this is `TRUE`, argument `w` cannot be a matrix.
#'
#' @param quantile Numeric between 0 and 1, inclusive: Quantile to calculate when `fun = "quantile"`. The default value is 0.5 (median), and valid values must be in the range between 0 and 1, inclusive.
#'
#' @returns A `GRaster`.
#' 
#' @seealso [terra::focal()], **GRASS** manual page for tool `r.neighbors` (see `grassHelp("r.neighbors")`)
#' 
#' @example man/examples/ex_focal.r
#' 
#' @aliases focal
#' @rdname focal
#' @exportMethod focal
methods::setMethod(
    f = "focal",
    signature = c(x = "GRaster"),
    function(x, w = 3, fun = "sum", circle = FALSE, quantile = 0.5) {

	if (inherits(w, "numeric") && (w < 3 | omnibus::compareFloat(w %% 2, 0, "=="))) stop("Argument `w` must be an odd integer >= 3.")
	
	if (inherits(w, "matrix") && (nrow(w) != ncol(w) | omnibus::compareFloat(nrow(w) %% 2, 0, "==") | omnibus::compareFloat(ncol(w) %% 2, 0, "=="))) stop("Matrix `w` must have the same number of rows and columns, and it must have an odd number of each.")
	
	if (is.matrix(w) & circle) ("Cannot use a circular neighborhood and a weights matrix at the same time.")
	
    funs <- c("mean", "sum", "sd", "sdpop", "var", "varpop", "median", "mode", "max", "min", "count", "range", "nunique", "interspersion", "quantile")

    if (!is.character(fun)) stop("Argument `fun` must be a character.")

    fun <- omnibus::pmatchSafe(fun, funs, useFirst = TRUE)
    if (fun == "mean") fun <- "average"
    if (fun == "min") fun <- "minimum"
    if (fun == "max") fun <- "maximum"
    if (fun == "nunique") fun <- "diversity"
	
    if (fun == "quantile") {
        if (quantile < 0 | quantile > 1) stop("Argument `quantile` must be in the range [0, 1].")
    }

    .locationRestore(x)
    .region(x)

	# size of neighborhood
	size <- if (inherits(w, "matrix")) {
		nrow(w)
	} else {
		w
	}

    args <- list(
        cmd = "r.neighbors",
        input = NA_character_,
        output = NA_character_,
        method = fun,
        size = size,
        nprocs = faster("cores"),
        memory = faster("memory"),
        flags = c(.quiet(), "overwrite")
    )

    if (circle) args$flags <- c(args$flags, "c")
    if (fun == "quantile") args$quantile = quantile

    ### weights matrix
    if (inherits(w, "matrix")) {
        
		if (anyNA(w)) w[is.na(w)] <- 0
		
		weight <- as.data.frame(w)
		colnames(weight) <- NULL
		
        # GRASS wants backslashes
		rand <- omnibus::rstring(1L)
		weightFile <- paste0(tempdir(), "/TEMPTEMPweights", rand, ".txt")
		weightFileBack <- paste0(tempdir(), "\\TEMPTEMPweights", rand, ".txt")
		
		sink(weightFile)
		print(weight, row.names = FALSE)
		sink()
		
		args$weight <- weightFileBack
        args$weighting_function <- "file"
        
    }

    ### process each layer of raster
    nLayers <- nlyr(x)
    for (i in seq_len(nLayers)) {

        # must convert to double in some cases
        if (fun == "average") {

			# convert to double to obviate issues with imprecision
			gnDouble <- .makeSourceName("double", "raster")
			ex <- paste0(gnDouble, " = double(", sources(x)[i], ")")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)

			# focal sums (numerator)
			gnSum <- .makeSourceName("sum", "raster")
			thisArgs <- args
			thisArgs$method <- "sum"
			thisArgs$input <- gnDouble
			thisArgs$output <- gnSum
			do.call(rgrass::execGRASS, args = thisArgs)

			# ones mask (for denominator)
			gnOnes <- .makeSourceName("ones", "raster")
			ex <- paste0(gnOnes, " = if(isnull(", gnDouble, "), null(), double(1))")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)

			# count raster (sum of ones--denominator)
			gnCount <- .makeSourceName("count", "raster")
			thisArgs <- args
			thisArgs$method <- "sum"
			thisArgs$input <- gnOnes
			thisArgs$output <- gnCount
			do.call(rgrass::execGRASS, args = thisArgs)

			# calculate mean
			src <- .makeSourceName(fun, "raster")
			ex <- paste0(src, " = ", gnSum, " / ", gnCount)
			rgrass::execGRASS("r.mapcalc", expression = ex, flags=c(.quiet(), "overwrite"), intern = TRUE)

        } else if (fun %in% c("sdpop", "sd", "var", "varpop")) {
	
			# convert to double to obviate issues with imprecision
			gnDouble <- .makeSourceName("double", "raster")
			ex <- paste0(gnDouble, " = double(", sources(x)[i], ")")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)
		
			### numerator LHS
			
				# square values
				gnSquared <- .makeSourceName("squared", "raster")
				ex <- paste0(gnSquared, " = ", gnDouble, "^2")
				rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)
				
				# focal sums of squares
				gnSoS <- .makeSourceName("sumOfSquares", "raster")
				thisArgs <- args
				thisArgs$method <- "sum"
				thisArgs$input <- gnSquared
				thisArgs$output <- gnSoS
				do.call(rgrass::execGRASS, args = thisArgs)

			### numerator RHS
				
				# focal sums
				gnSums <- .makeSourceName("sum", "raster")
				thisArgs <- args
				thisArgs$method <- "sum"
				thisArgs$input <- gnDouble
				thisArgs$output <- gnSums
				do.call(rgrass::execGRASS, args = thisArgs)
				
				# squared sums raster
				gnSquaredSums <- .makeSourceName("squaredSums", "raster")
				ex <- paste0(gnSquaredSums, " = ", gnSums, "^2")
				rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)

			### counts
				
				# ones mask... used to obviate problems where NULL cells are counted as 0
				gnOnes <- .makeSourceName("ones", "raster")
				ex <- paste0(gnOnes, " = if(isnull(", gnDouble, "), null(), double(1))")
				rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)
				
				# count raster (sum of ones)
				gnCount <- .makeSourceName("count", "raster")
				thisArgs <- args
				thisArgs$method <- "sum"
				thisArgs$input <- gnOnes
				thisArgs$output <- gnCount
				do.call(rgrass::execGRASS, args = thisArgs)

			### final
				
				# numerator
				gnNumer <- .makeSourceName("numerator", "raster")
				ex <- paste0(gnNumer, " = ", gnSoS, " - (", gnSquaredSums, " / ", gnCount, ")")
				rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)
				
				# sd or variance
				src <- .makeSourceName(fun, "raster")
				ex <- if (fun == "sd") {
					paste0(src, " = sqrt(", gnNumer, " / (", gnCount, " - 1))")
				} else if (fun == "sdpop") {
					paste0(src, " = sqrt(", gnNumer, " / ", gnCount, ")")
				} else if (tolower(fun) == "var") {
					paste0(src, " = ", gnNumer, " / (", gnCount, " - 1)")
				} else if (tolower(fun) == "varpop") {
					paste0(src, " = ", gnNumer, " / (", gnCount, ")")
				}

				rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)

        } else if (fun == "interspersion") {
		
			gnInter <- .makeSourceName("interspersion", "raster")
			args$output <- gnInter
			args$input <- sources(x)[i]
			do.call(rgrass::execGRASS, args = args)

			src <- .makeSourceName("focal", "raster")
			ex <- paste0(src, " = (double(", gnInter, ") - 1) / 100")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)
			
        } else {

			src <- .makeSourceName("focal", "raster")
			args$output <- src
			args$input <- sources(x)[i]
			do.call(rgrass::execGRASS, args = args)

        }
	
		if (is.factor(x)[i] & fun %in% c("mode", "min", "max")) {
			levels <- cats(x)[[i]]
		} else {
			levels <- NULL
		}
	
        this <- .makeGRaster(src, names(x)[i], levels = levels)
        if (i == 1L) {
            out <- this
        } else {
            out <- c(out, this)
        }

    } # next raster
    out

    } # EOF
) 

