#' GRASS citation, version, and copyright information
#'
#' Report the **GRASS** citation, version/release year, version number, or copyright information.
#'
#' @param x Character: What to return. Any of:
#' * `"citation"` (default)
#' * `"copyright"`: Copyright information
#' * `"version"`: Version number and release year
#' * `"versionNumber"`: Version number as numeric, major and minor only (e.g., 8.3)
#'
#' Partial matching is used and case is ignored.
#' 
#' @return Character.
#'
#' @example man/examples/ex_grassInfo.r
#' 
#' @aliases grassInfo
#' @rdname grassInfo
#' @export grassInfo
grassInfo <- function(x = "citation") {
	
	match <- omnibus::pmatchSafe(x, table = c("citation", "version", "versionNumber", "copyright"), useFirst = TRUE, nmax = 1L)

	if (match == "citation") {
		out <- rgrass::execGRASS("g.version", flags = "x", intern = TRUE)
	} else if (match == "copyright") {
		out <- rgrass::execGRASS("g.version", flags = "c", intern = TRUE)
	} else if (match == "version") {
		out <- rgrass::execGRASS("g.version", intern = TRUE)
	} else if (match == "versionNumber") {

		out <- .fasterRaster$grassVersion
		if (is.na(out)) {

			out <- rgrass::execGRASS("g.version", intern = TRUE)
			out <- strsplit(out, split = " ")[[1L]]
			out <- out[2L]
			out <- substr(out, 1L, 3L)
			out <- as.numeric(out)

		}
	}
	out
}
