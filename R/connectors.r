#' Create lines connecting nearest features of two GVectors
#'
#' @description `connectors()` creates a lines `GVector` which represent the shortest (Great Circle) paths between each feature of one `GVector` and the nearest feature of another `GVector`.
#'
#' @param x,y `GVector`s.
#' @param minDist,maxDist Either `NULL` (default) or numeric values: Ignore features separated by less than or greater than these distances.
#'
#' @returns A `GVector` with a data table that has the length of each connecting line in meters.
#' 
#' @seealso **GRASS** manual for module `v.distance` (see `grassHelp("v.distance")`).
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

	if (!is.null(minDist) && minDist < 0) stop("Argument `minDist` must be positive or NULL.")
	if (!is.null(maxDist) && maxDist < 0) stop("Argument `maxDist` must be positive or NULL.")
	if ((!is.null(maxDist) & !is.null(maxDist)) && (minDist > maxDist)) stop("Argument `minDist` is greater than `maxDist`.")

	compareGeom(x, y)
	.locationRestore(x)

	if (is.null(minDist)) minDist <- -1
	if (is.null(maxDist)) maxDist <- -1

	if (!.vHasDatabase(x)) {

		cats <- .vCats(x)
		# db <- data.frame(TEMPTEMP_minDist_meters = -1)
		db <- data.frame(cat = cats, TEMPTEMP_minDist_meters = -1)
		.vAttachDatabase(x, table = db, replace = TRUE, cats = cats)
		
	}

	# create connectors
	src <- .makeSourceName("connectors", "vector")

	rgrass::execGRASS(
		cmd = "v.distance",
		from = sources(x),
		to = sources(y),
		output = src,
		upload = "dist",
		dmin = minDist,
		dmax = maxDist,
		column = "TEMPTEMP_minDist_meters",
		flags = c(.quiet(), "overwrite")
	)
	
	# # remove column that was added to x
	# rgrass::execGRASS("v.db.dropcolumn", map = sources(x), columns = "TEMPTEMP_minDist_meters", flags = .quiet())

	table <- .vAsDataTable(x)
	table <- table[ , "TEMPTEMP_minDist_meters"]
	names(table) <- "length_meters"
	.makeGVector(src, table = table)

	} # EOF
)
