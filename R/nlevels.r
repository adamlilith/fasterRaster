#' Number of levels in a categorical raster
#'
#' @description This function reports the number of categories (levels) in a [categorical raster][tutorial_categorical_rasters].
#'
#' @param x A `GRaster`.
#'
#' @param droplevels Logical: If `TRUE` (default), drop levels that do not appear in the raster.
#'
#' @return A numeric vector of integers. These represent the number of levels that appear in the raster (always for `GRaster`s, and for `SpatRaster`s when `dropLevels = TRUE`), or the number of levels that appear or could appear in the raster (for `SpatRaster`s only, when `dropLevels = FALSE`).
#'
#' @seealso [terra::nlevels()], [levels()], [terra::levels()], [categorical rasters][tutorial_raster_data_types]
#'
#' @example man/examples/ex_GRaster_categorical.r
#'
#' @aliases nlevels
#' @rdname nlevels
#' @exportMethod nlevels
setMethod(
    f = "nlevels",
    signature = "GRaster",
    definition = function(x) {
        out <- sapply(x@levels, nrow)
		names(out) <- names(x)
        out
    } # EOF
)

#' Count number of levels
#'
#' @description Counts number of levels in a character string (specifically, the empty string `""`), a `data.frame`, `data.table`, or list of `data.frame`s or `data.table`s or empty strings.
#'
#' @param x A `data.frame`, `data.table`, an empty string, or a list thereof.
#'
#' @noRd
.nlevels <- function(x) {

	if (inherits(x, "SpatRaster")) x <- levels(x)
	
	if (!is.list(x)) x <- list(x)
	n <- rep(NA_integer_, length(list))

	for (i in seq_along(x)) {
	
		if (inherits(x[[i]], "character")) {
			if (x[[i]] == "") {
				n[i] <- 0L
			} else {
				stop("Argument x must be a data frame, data table, an empty string or a list of any of these.")
			}
		} else if (inherits(x[[i]], c("data.frame", "data.table"))) {
			n[i] <- nrow(x[[i]])
		}
	}
	n

}
