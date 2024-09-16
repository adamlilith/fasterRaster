#' Get levels from a SpatRaster or a file
#'
#' @param x A `SpatRaster`, `data.frame`, `data.table`, empty string, `NULL`, or a file name. CSV, TAB, RDS, and RDA are allowed file types.
#'
#' @returns A list.
#'
#' @noRd
.getLevels <- function(x) {

	if (is.null(x)) {
		levels <- NULL
	} else if (inherits(x, "SpatRaster")) {
		levels <- terra::cats(x)
	} else if (is.character(x)) {
		if (x == "") {
			levels <- list(x)
		} else {
		
			# load from file
		    nc <- nchar(x)
			suffix <- substr(x, nc - 3L, nc)
			
			suffix <- tolower(suffix)
			
			if (suffix %in% c(".csv", ".tab")) {
				levels <- data.table::fread(x, nThread = faster("cores"), na.strings = "<not defined>", showProgress = FALSE)
			} else if (suffix == ".rds") {
				levels <- readRDS(x)
			} else if (suffix == ".rda") {
				levelsObj <- load(x)
				levels <- get(levelsObj)
			} else {

				suffix <- substr(x, nc - 5L, nc)

				suffix <- tolower(suffix)

				if (suffix == ".rdata") {
					levelsObj <- load(levels)
					levels <- get(levelsObj)
				} else {
					stop("Cannot open a file with level data of this type.\n  Supported types include .csv, .tab, .rds, .rda/.rdata (case is ignored).")
				}

			}
		}
	
	} else if (!inherits(x, c("list", "data.frame", "data.table"))) {
		stop("Argument `levels` must be a data.frame, data.table, an empty string, a list of these, OR NULL or a file name.")
	}
	levels

}
