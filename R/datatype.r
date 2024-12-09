#' Get the datatype of a GRaster or of GVector columns
#'
#' @description For `GRaster`s, `datatype()` returns the data type (see `vignette("GRasters", package = "fasterRaster")`). For `GVector`s, `datatype()` returns the class of each column of the attribute table.
#'
#' @param x A `GRaster` or `GVector`.
#' @param type (`GRaster`s only) `NULL` or character: Type of datatype to report (`GRaster` only):
#' * `"fasterRaster"` (default): Reports the **fasterRaster** type (factor, integer, float, or double)
#' * `"terra"`: Report the (inferred) **terra** data type (e.g., INT2U, FLT4S). Please see the table in the documentation for [writeRaster()` for an explanation of these codes.
#' * `"GRASS"`: Will return "CELL" (integer), "FCELL" (floating-point value), or "DCELL" (double-floating point value)
#' * `"GDAL"`: See [GDAL: Raster Band](https://gdal.org/user/raster_data_model.html). Please also see the table in the [writeRaster()] help page.
#'
#' @param forceDouble Logical (`GRaster`s and `SpatRaster`s only): If `TRUE` (default), and the raster appears to represent non-integer values, then the raster will be assumed to represent double-floating point values (**GRASS**: type "DCELL", **terra**: type "FLT8S", **fasterRaster**: type "double", and **GDAL**: type "Float64"). `forceDouble` reports the actual datatype if `type = "fasterRaster"` (i.e., the type is not forced to "double").
#'
#' @returns `datatype()` for a `GRaster` returns a character. `datatype()` for a `GVector` returns a data frame, with one row per field. If the `GVector` has no attribute table, the function returns `NULL`.
#'
#' @seealso [terra::datatype()], `vignette("GRasters", package = "fasterRaster")`
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases datatype
#' @rdname datatype
#' @exportMethod datatype
methods::setMethod(
	f = "datatype",
	signature = c(x = "GRaster"),
	definition = function(x, type = "fasterRaster", forceDouble = TRUE) {

	type <- omnibus::pmatchSafe(type, c("fasterRaster", "GRASS", "terra", "GDAL"))

	out <- x@datatypeGRASS
	
	if (type == "fasterRaster") {

		isFact <- is.factor(x)
		out[out == "CELL" & isFact] <- "factor"
		out[out == "CELL" & !isFact] <- "integer"
		out[out == "FCELL"] <- "float"
		out[out == "DCELL"] <- "double"
	
	} else if (type %in% c("terra", "GDAL")) {

		stats <- .global(x, fun = c("min", "max", "sum"))

		min <- stats[ , "min"]
		max <- stats[ , "max"]
		sum <- stats[ , "sum"]

		integer <- all(omnibus::is.wholeNumber(stats))

	    # remainder <- stats %% 1L
		# integer <- rowSums(abs(remainder)) == 0L

		for (i in seq_along(out)) {

			if (out[i] == "CELL") {
			
				if (min[i] >= 0L & max[i] <= 255L) {
					out[i] <- if (type == "GDAL") { "Byte" } else { "INT1U" }
				} else if (min[i] >= 0L & max[i] <= 65534L) {
					out[i] <- if (type == "GDAL") { "UInt16" } else { "INT2U" }
				} else if (min[i] >= -32767L & max[i] <= -32767L) {
					out[i] <- if (type == "GDAL") { "Int16" } else { "INT2S" }
				} else if (min[i] >= -2147483647L & max[i] <= 2147483647L) {
					out[i] <- if (type == "GDAL") { "Int32" } else { "INT4S" }
				}
				
			} else if (forceDouble) {
				out[i] <- if (type == "GDAL") { "Float64" } else { "FLT8S" }
			} else if (out[i] == "FCELL") {
				out[i] <- if (type == "GDAL") { "Float32" } else { "FLT4S" }
			} else if (out[i] == "DCELL") {
				out[i] <- if (type == "GDAL") { "Float64" } else { "FLT8S" }
			} else {
				warning("Values are too small/large to represent. Assigning `double` type.")
				out[i] <- if (type == "GDAL") { "Float64" } else { "FLT8S" }
			}

		} # next raster

	} # want non-fasterRaster type
	out
	
	} # EOF
)

### NB This overwrites terra::datatype() for signature 'SpatRaster'!
# # # #' @aliases datatype
# # # #' @rdname datatype
# # # #' @exportMethod datatype
# # # methods::setMethod(
# # # 	f = "datatype",
# # # 	signature = c(x = "SpatRaster"),
# # # 	definition = function(x, type = "fasterRaster", forceDouble = TRUE) {
	
# # # 	type <- pmatch(type, c("fasterRaster", "GRASS", "terra", "GDAL"))

# # # 	stats <- terra::global(x, fun = c("min", "max", "sum"), na.rm = TRUE)

# # # 	min <- stats[ , "min"]
# # # 	max <- stats[ , "max"]
# # # 	sum <- stats[ , "sum"]

# # # 	integer <- all(omnibus::is.wholeNumber(stats))

# # # 	# remainder <- stats %% 1
# # # 	# integer <- rowSums(abs(remainder)) == 0

# # # 	nl <- terra::nlyr(x)
	
# # # 	out <- rep(NA_character_, nl)
# # # 	for (i in seq_len(nl)) {
	
# # # 		# integer
# # # 		if (integer[i]) {

# # # 			if (type == "GRASS") {
# # # 				out[i] <- "CELL"
# # # 			} else if (type == "fasterRaster") {
# # # 				out[i] <- "integer"
# # # 			} else {

# # # 				if (min[i] >= 0 & max[i] <= 255) {
# # # 					out[i] <- if (type == "GDAL") { "Byte" } else if (type == "terra") { "INT1U" }
# # # 				} else if (min[i] >= 0 & max[i] <= 65534) {
# # # 					out[i] <- if (type == "GDAL") { "UInt16" } else if (type == "terra") { "INT2U" }
# # # 				} else if (min[i] >= -32767 & max[i] <= -32767) {
# # # 					out[i] <- if (type == "GDAL") { "Int16" } else if (type == "terra") { "INT2S" }
# # # 				} else if (min[i] >= -2147483647 & max[i] <= 2147483647) {
# # # 					out[i] <- if (type == "GDAL") { "Int32" } else if (type == "terra") { "INT4S" }
# # # 				}
				
# # # 			}
			
# # # 		# not integer
# # # 		} else {
			
# # # 			if (forceDouble) {
# # # 				out[i] <- if (type == "GDAL") { "Float64" } else if (type == "terra") { "FLT8S" } else if (type == "GRASS") { "DCELL" } else if (type == "fasterRaster") { "double" }
# # # 			} else if (min[i] > -3.4e+38 & max[i] < 3.4e+38) {
# # # 				out[i] <- if (type == "GDAL") { "Float32" } else if (type == "terra") { "FLT4S" } else if (type == "GRASS") { "FCELL" } else if (type == "fasterRaster") { "float" }
# # # 			} else if (min[i] > -1.79e+308 & max[i] < 1.79e+308) {
# # # 				out[i] <- if (type == "GDAL") { "Float64" } else if (type == "terra") { "FLT8S" } else if (type == "GRASS") { "DCELL" } else if (type == "fasterRaster") { "double" }
# # # 			} else {
# # # 				warning("Values are too small/large to represent. Assigning `double` type.")
# # # 				out[i] <- if (type == "GDAL") { "Float64" } else if (type == "terra") { "FLT8S" } else if (type == "GRASS") { "DCELL"} else if (type == "fasterRaster") { "double" }
# # # 			}
# # # 		}

# # # 	} # next raster

# # # 	if (type == "fasterRaster") {

# # # 		isFact <- is.factor(x)
# # # 		out[out == "CELL" & isFact] <- "factor"
# # # 		out[out == "CELL" & !isFact] <- "integer"
# # # 		out[out == "FCELL"] <- "float"
# # # 		out[out == "DCELL"] <- "double"
	
# # # 	}
# # # 	out
	
# # # 	} # EOF
# # # )

#' @aliases datatype
#' @rdname datatype
#' @exportMethod datatype
setMethod(f = "datatype",
	signature = c(x = "GVector"),
	definition = function(x) {

	if (nrow(x@table) > 0L) {
		sapply(x@table, "class")
	} else {
		NULL
	}

	} # EOF
)
