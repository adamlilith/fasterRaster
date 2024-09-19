#' Aggregates or disaggregates a vector in GRASS
#'
#' @param src [sources()] name.
#' @param resolve "aggregate" or "disaggregate"
#' @param verbose T/F
#'
#' @returns [sources()] name.
#' @noRd
.aggDisaggVect <- function(src, resolve, verbose) {

	if (resolve == "disaggregate") {

		if (verbose) omnibus::say("Fixing invalid vector by disaggregating polygons. This will remove any data table.")

		# delete categories
		srcIn <- src
		src <- .makeSourceName("fast_v_category_del")
		rgrass::execGRASS(
			cmd = "v.category",
			input = srcIn,
			output = src,
			option = "del",
			cat = -1,
			flags = c(.quiet(), "overwrite")
		)

		# assign each subgeometry its own category
		srcIn <- src
		src <- .makeSourceName("fast_v_category_add")
		rgrass::execGRASS(
			cmd = "v.category",
			input = srcIn,
			output = src,
			option = "add",
			flags = c(.quiet(), "overwrite")
		)

	} else if (resolve == "aggregate") {

		if (verbose) omnibus::say("Fixing invalid vector by aggregating polygons. This will remove any data table.")

		srcIn <- src
		src <- .makeSourceName("fast_v_extract")
		rgrass::execGRASS(
			cmd = "v.extract",
			input = srcIn,
			output = src,
			new = 1L,
			flags = c(.quiet(), "overwrite", "d")
		)
	
	}

	src

}
