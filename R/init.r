#' GRaster with values equal to row, column, coordinate, regular, or "chess"
#'
#' @description This function can be used to make a `GRaster` with cell values equal to the cell center's longitude, latitude, row, or column, or in a "chess"-like or "regular" pattern.
#'
#' @param x A `GRaster` to be used as a template.
#'
#' @param fun Character: Any of:
#' * `"x"` or `"y"`: Cell longitude or latitude
#' * `"row"` or `"col"`: Cell row or column
#' * `"chess"`: Alternating values.
#' * `"regular"`: Evenly-spaced cells with the same value.
#'
#' @param odd Logical: If `TRUE` (default), and `fun` is `"chess"`, then the top left cell in the raster will be a "negative" cell. If `FALSE`, then the top left cell with be "positive".
#'
#' @param every Numeric or integer: If `fun` is `"regular"`, then make every `every` cell a "positive" cell, and interstitial cells "negative." The default is 2 (every other cell).
#'
#' @param vals Vector of two numeric values: If `fun` is `"chess"` or `"regular"`, then assign the first value to "positive" cells and the second value to "negative" cells. The default is `c(1, 0)`
#'
#' @returns A `GRaster` with as many layers as `x`.
#'
#' @seealso [terra::init()], [longlat()]
#'
#' @example man/examples/ex_init.r
#'
#' @aliases init
#' @rdname init
#' @exportMethod init
methods::setMethod(
	f = "init",
	signature = c(x = "GRaster"),
	function(x, fun, odd = TRUE, vals = c(0, 1)) {

	funs <- c("x", "y", "row", "col", "chess", "regular")
	fun <- omnibus::pmatchSafe(fun, funs, n = 1)

	# if (fun %in% c("chess", "regular") && (!omnibus::is.wholeNumber(every) | every < 1)) stop("The value of `every` must be a whole number > 0.")

	.locationRestore(x)
	.region(x)

	vals <- as.character(vals)
	vals[vals == "NA"] <- "null()"

	nLayers<- nlyr(x)
	srcs <- .makeSourceName(paste0("init_", fun), "raster", nLayers)
	for (i in seq_len(nLayers)) {
	
		if (fun == "chess") {
		
			if (odd) {
				ex <- paste0(srcs[i], " = if((row() % 2 == 1 & col() % 2 == 1) | (row() % 2 == 0 & col() % 2 == 0), ", vals[1L], ", ", vals[2L], ")")
			} else {
				ex <- paste0(srcs[i], " = if((row() % 2 == 0 & col() % 2 == 1) | (row() % 2 == 1 & col() % 2 == 0), ", vals[1L], ", ", vals[2L], ")")
			}
		
		} else if (fun == "regular") {
		
			if (odd) {
				ex <- paste0(srcs[i], " = if(row() % 2 == 1 | col() % 2 == 1, ", vals[1L], ", ", vals[2L], ")")
			} else {
				ex <- paste0(srcs[i], " = if(row() % 2 == 0 | col() % 2 == 0, ", vals[1L], ", ", vals[2L], ")")
			}
		
		} else if (fun != "chess") {
		
			ex <- paste0(srcs[i], " = ", fun, "()")

		}
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

	}
	.makeGRaster(srcs, fun)

	} # EOF
)
