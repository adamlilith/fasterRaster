#' Are the category values of a vector valid?
#'
#' @description Category values of a **GRASS** vector can be invalid if **GRASS** assigns more than one value to a geometry (e.g., "7/12"). This can occur when the vector was created by software that does not use a topological system (e.g., a shapefile).
#'
#' @param x A `GVector` or the [sources()] name of one.
#'
#' @returns Logical.
#'
#' @keywords internal
.vValidCats <- function(x) {

	if (inherits(x, "GVector")) x <- sources(x)
	meows <- .vCats(x, db = FALSE, integer = TRUE)
	length(meows) > 0L && !anyNA(meows)
	
}
