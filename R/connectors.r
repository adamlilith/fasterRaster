#' Create lines connecting nearest features of two GVectors
#'
#' @description `connectors()` creates a lines `GVector` which represent the shortest (Great Circle) paths between each feature of one `GVector` and the nearest feature of another `GVector`.
#'
#' @param x,y `GVector`s.
#' @param minDist,maxDist Either `NULL` (default) or numeric values: Ignore features separated by less than or greater than these distances.
#'
#' @return A `GVector`.
#' 
#' @seealso Module `v.distance` in **GRASS**
#'
#' @example man/examples/ex_connectors.r
#'
#' @aliases connectors
#' @rdname connectors
#' @exportMethod connectors
methods::setMethod(
	"connectors",
	signature(x = "GVector", y = "GVector"),
	function(x, y, minDist = NULL, maxDist = NULL) {

	if (!is.null(minDist) && minDist < 0) stop("Argument ", sQuote("minDist"), " must be positive or NULL.")
	if (!is.null(maxDist) && maxDist < 0) stop("Argument ", sQuote("maxDist"), " must be positive or NULL.")
	if ((!is.null(maxDist) & !is.null(maxDist)) && (minDist > maxDist)) stop("Argument ", sQuote("minDist"), " is greater than ", sQuote("maxDist"), ".")

	compareGeom(x, y)
	.restore(x)

	if (is.null(minDist)) minDist <- -1
	if (is.null(maxDist)) maxDist <- -1

	# # make copy of x because we add a new column to it
	# gnCopy <- .makeSourceName(x, "vector")
	# fromTo <- paste0(sources(x), ",", gnCopy)
	# rgrass::execGRASS("g.copy", vector=fromTo, flags=c("quiet", "overwrite"))
	# rgrass::execGRASS("v.db.addcolumn", map=gnCopy, columns="TEMP_minDist_meters", flags="quiet")
	
	rgrass::execGRASS("v.db.addcolumn", map=sources(x), columns="TEMPTEMP_minDist_meters", flags="quiet")

	# create connectors
	src <- .makeSourceName("connectors", "vector")

	args <- list(
		cmd = "v.distance",
		from = sources(x),
		to = sources(y),
		output = src,
		upload = "dist",
		flags = c("quiet", "overwrite"),
		dmin = minDist,
		dmax = maxDist,
		column = "TEMPTEMP_minDist_meters",
		intern = TRUE
	)
	
	input <- do.call(rgrass::execGRASS, args)

	# remove column that was added to x
	rgrass::execGRASS("v.db.dropcolumn", map=sources(x), columns="TEMPTEMP_minDist_meters", flags="quiet")

	.makeGVector(src)

	} # EOF
)
