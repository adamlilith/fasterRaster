#' Increase/decrease the size of a vector or around non-NA cells of a raster
#'
#' Buffers can be constructed for `GRaster`s or `GVector`s. For rasters, the `buffer()` function creates a buffer around non-`NA` cells. The output will be a raster. For vectors, the `buffer()` and `st_buffer()` functions create a vector polygon larger or smaller than the focal vector.
#'
#' @param x A `GRaster` or `GVector`.
#' 
#' @param width Numeric: For rasters -- Maximum distance cells must be from focal cells to be within the buffer. For rasters, if the buffering unit is `"cells`", then to get `n` cell widths, use `n + epsilon`, where `epsilon` is a small number (e.g., 0.001). The larger the buffer, this smaller this must be to ensure just `n` cells are included.
#' 
#' For vectors, distance from the object to place the buffer. Negative values create "inside" buffers. Units are in the same units as the current coordinate reference system (e.g., degrees for WGS84 or NAD83, often meters for projected systems).
#' 
#' @param dist Vectors -- Same as `width`.
#' 
#' @param background Numeric: Rasters -- Value to assign to cells that are not `NA` and not part of the buffer (default is 0).
#' 
#' @param unit Character: Rasters -- Indicates the units of \code{width}. Can be one of:
#' 	* `"cells"`: Units are numbers of cells.
#'	* `"meters"` (default), `"kilometers"` or `"km"`, `"feet"`, `"miles"`, or `"nautmiles"` (nautical miles).
#' Partial matching is used and case is ignored.
#'
#' @param method Character: Rasters -- Only used if `units` is `"cells"`. Indicates the manner in which distances are calculated for adding of cells:
#' * `"Euclidean"`: Euclidean distance
#' * `"Manhattan"`: "taxi-cab" distance)
#' * `"maximum"`: Maximum of the north-south and east-west distances between points. 
#' Partial matching is used and case is ignored.
#'
#' @param lowMemory Logical: Rasters -- Only used if buffering a raster and `units` is not `"meters"`. If `FALSE` (default) use faster, memory-intensive procedure. If `TRUE` then use the slower, low-memory version. To help decide which to use, consider using the low-memory version on a system with 1 GB of RAM for a raster larger than about 32000x32000 cells, or for a system with  with 8 GB of RAM a raster larger than about 90000x90000 cells.
#'
#' @param capstyle,endCapStyle Character: Vectors -- Style for ending the "cap" of buffers around lines. Valid options include `"rounded"`, `"square"`, and "`flat`".
#'
#' @param union LogicalVectors --: If `FALSE` (default), construct a buffer for each geometry. If `TRUE`, union all buffers after creation.
#'
#' @seealso [terra::buffer()], [sf::st_buffer()], and modules `r.buffer`, `r.grow`, and `v.buffer` in **GRASS**
#'
#' @example man/examples/ex_buffer.R
#'
#' @aliases buffer
#' @rdname buffer
#' @exportMethod buffer
methods::setMethod(
	"buffer",
	signature(x = "GRaster"),
	function(x, width, unit = "meters", method = "Euclidean", background = 0, lowMemory = FALSE) {

	.restore(x)
	region(x)

	units <- c("cells", "meters", "kilometers", "km", "feet", "miles", "nautmiles")
	unit <- pmatchSafe(unit, units)
	if (unit == "km") unit <- "kilometers"

	if (nlyr(x) > 1L) out <- list()

	# for each layer
	for (i in 1L:nlyr(x)) {

		gnBuffer <- .makeSourceName("buffer", "raster")

		### buffering by cells
		if (unit == "cells") {
		
			methods <- c("euclidean", "manhattan", "maximum")
			method <- tolower(method)
			method <- pmatchSafe(method, method)

			args <- list(
				cmd = "r.grow",
				input = sources(x[[i]]),
				output = gnBuffer,
				radius = width,
				metric = method,
				old = 1,
				new = 1,
				flags = c("quiet", "overwrite")#,
				# intern = TRUE
			)
		
		### buffering by distance
		} else {
		
			if (lowMemory) {
				fx <- "r.buffer.lowmem"
			} else {
				fx <- "r.buffer"
			}

			args <- list(
				cmd = ifelse (lowMemory, "r.buffer.lowmem", "r.buffer"),
				input = sources(x),
				output = gnBuffer,
				distances = width,
				units = unit,
				flags = c("quiet", "overwrite"),
				intern = TRUE
			)
			
		}

		### buffer
		do.call(rgrass::execGRASS, args)

		### reclass
		src <- .makeSourceName("buffer", "raster")
		ex <- if (!is.na(background)) {
			paste0(src, " = if(isnull(", gnBuffer, "), ", background, ", if(", gnBuffer, " == 2, 1, 1))")
		} else {
			paste0(src, " = if(", gnBuffer, " == 2, 1, 1)")
		}
		rgrass::execGRASS("r.mapcalc", expression=ex, flags=c("quiet", "overwrite"))
		
		if (nlyr(x) > 1L) {
			group[[i]] <- .makeGRaster(src, names(x[[i]]))
		} else {
			out <- .makeGRaster(src, names(x[[i]]))
		}
		
	} # next layer
	
	if (nlyr(x) > 1L) out <- c(out)
	out

	} # EOF
)

#' @aliases buffer
#' @rdname buffer
#' @exportMethod buffer
methods::setMethod(
	"buffer",
	signature(x = "GVector"),
	function(x, width, capstyle = "round", union = FALSE) {

	.restore(x)

	### buffer
	src <- .makeSourceName("v_buffer", "vector")
	args <- list(
		cmd = "v.buffer",
		input = sources(x),
		output = src,
		distance = width,
		flags = c("quiet", "overwrite"),
		intern = TRUE
	)

	if (!union) args$flags <- c(args$flags, "t")

	capstyle <- tolower(capstyle)
	capstyle <- pmatchSafe(capstyle, c("round", "square", "flat"))
	if (capstyle == "square") {
		args$flags <- c(args$flags, "s")
	} else if (capstyle == "flat") {
		args$flags <- c(args$flags, "c")
	}

	do.call(rgrass::execGRASS, args)

	# dissolve by category
	if (!union) {

		# dissolve
		srcBuff <- src
		src <- .makeSourceName("v_dissolve", "vector")
		args <- list(
			cmd = "v.dissolve",
			input = srcBuff,
			output = src,
			flags = c("quiet", "overwrite")
		)
		do.call(rgrass::execGRASS, args = args)

		# add categories
		nGeoms <- max(.vGeometries(src))
		catTable <- data.frame(cat = 1L:nGeoms)
		.vRecat(catTable, src, removeTable = FALSE)

	}

	.makeGVector(src)
	
	} # EOF
)

#' @aliases st_buffer
#' @rdname buffer
#' @exportMethod st_buffer
methods::setMethod(
	"st_buffer",
	signature(x = "GVector"),
	function(x, dist, endCapStyle = "round", union = FALSE) {
		buffer(x, width = dist, capstyle = endCapStyle, union = union)
	} # EOF
)

st_buffer <- function(x) UseMethod("st_buffer", x)
