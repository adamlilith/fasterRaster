#' Get the datatype of a GRaster or of GVector columns
#'
#' Returns the data type of a `GRaster` or of each column of a `GVector`.
#'
#' @param x A `GRaster` or `GVector`.
#' @param type Character: What type of datatype to report (`GRaster` only): `"GRASS"` (default), `"terra"` (**terra** package data types; see [terra::writeRaster()]), or `"GDAL"` (see [GDAL: Raster Band](https://gdal.org/user/raster_data_model.html#raster-band)).
#'
#' @return `datatype()` for a `GRaster` returns a character. `datatype()` for a `GVector` returns a data frame, with one row per field.
#'
#' @seealso [terra::datatype()] 
#'
#' @example man/examples/ex_GRaster.r
#'
#' @aliases datatype
#' @rdname datatype
#' @export datatype
#' @exportMethod datatype
setMethod(f = "datatype",
	signature = c(x = "GRaster"),
	definition = function(x, type = "GRASS") {
	
	dt <- x@datatypeGRASS
	
	type <- pmatchSafe(type, c("GRASS", "terra", "GDAL"))
	
	if (type == "terra") {
		dt[dt == "CELL"] <- "INT4S"
		dt[dt == "FCELL"] <- "FLT4S"
		dt[dt == "DCELL"] <- "FLT8S"
	} else if (type == "GDAL") {
		dt[dt == "CELL"] <- "Int32"
		dt[dt == "FCELL"] <- "Float32"
		dt[dt == "DCELL"] <- "Float64"
	}
	
	dt
	
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

#' Intuit GDAL/terra data type of a raster
#'
#' Inuit GDAL/terra data type of a raster
#'
#' @param x A `GRaster` or a vector of `gnames`.
#' @param type Character: Type of datatype to return: `"GDAL"` or `"terra"`.
#' @param force `NULL` (default), or `CELL`, `DCELL`, or `FCELL` to force the output to correspond to this **GRASS** data type.
#'
#' @returns Character.
#'
#' @noRd
.datatype <- function(x, type = "terra", force = NULL) {

	type <- pmatchSafe(type, c("terra", "GDAL"))

	out <- rep(NA_character_, nlyr(x))
 	if (inherits(x, "GRaster")) x <- .gnames(x)

	if (is.null(force)) force <- "NO_FORCE"

	for (i in seq_along(x)) {

		args <- list(
			cmd = "r.univar",
			flags = c("e", "r", "quiet"),
			map = gns,
			intern = TRUE
		)
		info <- do.call(rgrass::execGRASS, args = args)

		min <- info[grepl(info, pattern = "minimum: ")]
		max <- info[grepl(info, pattern = "maximum: ")]
		range <- info[grepl(info, pattern = "range: ")]
		sum <- info[grepl(info, pattern = "sum: ")]

		min <- gsub(min, pattern = "minimum: ", replacement = "")
		max <- gsub(max, pattern = "maximum: ", replacement = "")
		range <- gsub(range, pattern = "range: ", replacement = "")
		sum <- gsub(sum, pattern = "sum: ", replacement = "")

		min <- as.numeric(min)
		max <- as.numeric(max)
		range <- as.numeric(range)
		sum <- as.numeric(sum)

		integer <- (min %% 1 == 0 & max %% 1 == 0 & range %% 1 == 0 & sum %% 1 == 0)
		
		if ((integer | force == "CELL") && min >= 0 & max <= 255) {
			out[i] <- if (type == "GDAL") { "Btye" } else { "INT1U" }
		} else if ((integer | force == "CELL") && min >= 0 & max <= 65534) {
			out[i] <- if (type == "GDAL") { "UInt16" } else { "INT2U" }
		} else if ((integer | force == "CELL") && min >= -32767 & max <= -32767) {
			out[i] <- if (type == "GDAL") { "Int16" } else { "INT2S" }
		} else if ((integer | force == "CELL") && min >= -2147483647 & max <= 2147483647) {
			out[i] <- if (type == "GDAL") { "Int32" } else { "INT4S" }
		} else if (force == "FCELL" | (min >= -3.4E38 & max <= 3.4E38)) {
			out[i] <- if (type == "GDAL") { "Float32" } else { "FLT4S" }
		} else if (force == "DCELL" | (min >= -1.7E308 & max <= 1.7E308)) {
			out[i] <- if (type == "GDAL") { "Float64" } else { "FLT8S" }
		} else {
			warning("Values are too small/large to represent.")
			out[i] <- if (type == "GDAL") { "Float64" } else { "FLT8S" }
		}

	} # next raster
	out

}
