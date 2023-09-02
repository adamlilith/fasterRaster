#' Get the datatype of a GRaster or of GVector columns
#'
#' @description `datatype()` returns the [data type[tutorial_raster_data_types] of a `GRaster` or of each column of a `GVector`.
#'
#' @param x A `GRaster` or `GVector`.
#' @param type `NULL` or character: Type of datatype to report (`GRaster` only):
#' * `"fasterRaster"` (default): Reports the "*fasterRaster**" type (factor, float, or double)
#' * `"terra"`: Report the (inferred) **terra** data type (e.g., INT2U, FLT4S). Please see the table in the [tutorial on raster data types] for more information.
#' * `"GRASS"`: Will return "CELL" (integer), "FCELL" (floating-point value), or "DCELL" (double=floating point value)
#' * `"GDAL"`: See [GDAL: Raster Band](https://gdal.org/user/raster_data_model.html#raster-band).
#'
#' @return `datatype()` for a `GRaster` returns a character. `datatype()` for a `GVector` returns a data frame, with one row per field.
#'
#' @seealso [terra::datatype()] 
#'
#' @example man/examples/ex_GRaster.r
#'
#' @aliases datatype
#' @rdname datatype
#' @exportMethod datatype
methods::setMethod(
	f = "datatype",
	signature = c(x = "GRaster"),
	definition = function(x, type = "fasterRaster") {
	
	type <- pmatchSafe(type, c("fasterRaster", "GRASS", "terra", "GDAL"))
	out <- x@datatypeGRASS
	
	if (type == "fasterRaster") {

		numLevels <- nlevels(x)
		out[out == "CELL" & numLevels > 0L] <- "factor"
		out[out == "CELL" & numLevels == 0L] <- "integer"
		out[out == "FCELL"] <- "float"
		out[out == "DCELL"] <- "double"
	
	} else if (type %in% c("terra", "GDAL")) {

		stats <- global(x, fun = c("min", "max", "range", "sum"))

		min <- stats[ , "min"]
		max <- stats[ , "max"]
		range <- stats[ , "range"]
		sum <- stats[ , "sum"]

	    remainder <- stats %% 1
		integer <- rowSums(abs(remainder)) == 0

		for (i in seq_along(x)) {

			if (integer[i] & out[i] == "CELL") {
			
				if (min >= 0 & max <= 255) {
					out[i] <- if (type == "GDAL") { "Byte" } else { "INT1U" }
				} else if (min >= 0 & max <= 65534) {
					out[i] <- if (type == "GDAL") { "UInt16" } else { "INT2U" }
				} else if (min >= -32767 & max <= -32767) {
					out[i] <- if (type == "GDAL") { "Int16" } else { "INT2S" }
				} else if (min >= -2147483647 & max <= 2147483647) {
					out[i] <- if (type == "GDAL") { "Int32" } else { "INT4S" }
				}
			} else if (out[i] == "FCELL" | (min >= -3.4E38 & max <= 3.4E38)) {
				out[i] <- if (type == "GDAL") { "Float32" } else { "FLT4S" }
			} else if (out[i] == "DCELL" | (min >= -1.7E308 & max <= 1.7E308)) {
				out[i] <- if (type == "GDAL") { "Float64" } else { "FLT8S" }
			} else {
				warning("Values are too small/large to represent.")
				out[i] <- if (type == "GDAL") { "Float64" } else { "FLT8S" }
			}

		} # next raster

	} # want non-fasterRaster type
	out
	
	} # EOF
)

#' @aliases datatype
#' @rdname datatype
#' @exportMethod datatype
setMethod(f = "datatype",
	signature = c(x = "GVector"),
	definition = function(x) {

		if (inherits(x@db, "GFullMetaTable")) {
			data.frame(
				field = x@db@fields,
				datatype = x@db@classes
			)
		} else {
			NULL
		}
	
	} # EOF
)
