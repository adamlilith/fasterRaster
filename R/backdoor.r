#' Setup fasterRaster for ABS
#'
#' This is a secret function to be used for faster development of **fasterRaster**. It calls [faster()] to set the install directory for **GRASS**, increases default memory, and number of cores. The function assumes development is on a Windows machine.
#'
#' @param start Logical: If `TRUE`, start the **GRASS** session by creating the `madElev` `GRaster`.
#' @param ver Character: **GRASS**: e.g., "83" or "84".
#'
#' @returns `TRUE` (invisibly).
#'
#' @keywords internal
.backdoor <- function(start = FALSE, ver = "84") {
	
	verNice <- paste0(substr(ver, 1L, 1L), ".", substr(ver, 2L, 2L))
	
	faster(
		grassDir = paste0("C:/Program Files/GRASS GIS ", verNice),
		addonsDir = "C:/Users/adame/AppData/Roaming/GRASS8/addons",
		memory = 1024 * 8,
		cores = 2,
		useDataTable = TRUE,
		verbose = TRUE
	)

	if (start) {
		madElev <- fastData("madElev")
		elev <- fast(madElev)
		out <- elev
	} else {
		out <- TRUE
	}

	invisible(out)
}
