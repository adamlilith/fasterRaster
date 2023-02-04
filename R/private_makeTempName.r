#' Make the name of a temporary raster or vector
#'
#' Private. Makes a name of a temporary raster or vector.
#'
#' @param x String to include in the name. This will be as: "TEMPTEMP_<string><random_number>".
#'
#' @keywords internal

.makeTempName <- function(x) {
	paste0('TEMPTEMP_', x, round(1E9*runif(1)))
}
