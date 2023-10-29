#' Coerce as multipart GVector to a singlepart GVector
#'
#' @description `GVectors` can contain a mix of "singlepart" and "multipart" features. A singlepart feature is a single point, set of connected line segments, or a polygon. A multipart feature is a set of lines, sets of connected line segments, or set of polygons that are treated as a single feature. This function converts all multipart features to singlepart features. If the `GVector has an attribute table, it will be removed from the output.
#' 
#' @param x A `GVector`.
#' 
#' @returns A `GVector`.
#' 
#' @seealso [aggregate()]
#' 
#' @example man/examples/ex_aggregate_disagg.r
#' 
#' @aliases disagg
#' @rdname disagg
#' @exportMethod disagg
methods::setMethod(
	f = "disagg",
	signature = c(x = "GVector"),
	function(x) {

	.restore(x)

	srcDel <- .makeSourceName("v_category_del", "vector")
	rgrass::execGRASS(
		cmd = "v.category",
		input = sources(x),
		output = srcDel,
		option = "del",
		cat = -1,
		flags = c("quiet", "overwrite")
	)

	src <- .makeSourceName("v_category_add", "vector")
	rgrass::execGRASS(
		cmd = "v.category",
		input = srcDel,
		output = src,
		option = "add",
		flags = c("quiet", "overwrite")
	)

	# replicate data table
	
	if (nrow(x) == 0L) {
		table <- NULL
	} else {
		
		cats <- .vCats(x, table = FALSE, integer = TRUE)
		if (anyNA(cats)) {
			warning("At least one geometry has a combined category. Data table cannot be copied.")
			table <- NULL
		} else {
			table <- x@table
			table <- table[cats]
		}

	}

	.makeGVector(src, table = table)

	} # EOF
)
