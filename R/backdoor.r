#' Setup fasterRaster for ABS
#'
#' This is a secret function to be used for faster development of **fasterRaster**. It assume development is on a Windows machine.
#'
#' @param ver Character: **GRASS**: e.g., "83" or "84".
#'
#' @returns `TRUE` (invisibly).
#'
#' @keywords internal
.backdoor <- function(ver = "84") {
	
	verNice <- paste0(substr(ver, 1L, 1L), ".", substr(ver, 2L, 2L))
	
	faster(
		grassDir = paste0("C:/Program Files/GRASS GIS ", verNice),
		memory = 1024 * 8,
		cores = 2,
		verbose = TRUE
	)
	invisible(TRUE)
}
