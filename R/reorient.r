#' Convert degrees between 'north-orientation' and 'east orientation'
#'
#' @description This function converts facing between "north orientation" and "east orientation".
#'
#' In "north orientation" systems, a 0-degree facing is north, and the angle of facing proceeds clockwise. For example, a 90 degree facing faces east, 180 south, and 270 west. In "east orientation", a 0-degree facing is east, and the facing angle proceeds counter-clockwise. For example, 90 is north, 180 is west, and 270 south.
#'
#' @param x A numeric vector or a `GRaster` with cell values equal to facing (in degrees).
#'
#' @returns A `GRaster` or numeric vector. Values will be in the range between 0 and 360 and represents facing in the system "opposing" the input's system. For example, if the input is north orientation, the output will be east orientation. If the input is in east orientation, the output will be in north orientation.
#'
#' @example man/examples/ex_reorient.r
#'
#' @aliases reorient
#' @rdname reorient
#' @exportMethod reorient
methods::setMethod(
	f = "reorient",
	signature = c(x = "GRaster"),
	definition = function(x) {
	
	.locationRestore(x)
	.region(x)

	nLayers <- nlyr(x)
	srcs <- .makeSourceName("reorient", "raster", n = nLayers)

	for (i in seq_len(nLayers)) {

		ex <- paste0(srcs[i], " = ((360 - ", sources(x)[i], "% 360) % 360 + 90) % 360")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

	}
	.makeGRaster(srcs, names(x))

	} # EOF
)

#' @aliases reorient
#' @rdname reorient
#' @exportMethod reorient
methods::setMethod(
	f = "reorient",
	signature = c(x = "numeric"),
	definition = function(x) ((360 - (x %% 360)) + 90) %% 360
)
