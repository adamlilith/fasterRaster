#' Return a character list of flags
#'
#' This private function adds "overwrite" to a "flags" character list if the argument \code{remove} is \code{TRUE}.
#'
#' @param replace Logical. Determines if rasters or vectors imported into or created in \code{GRASS} can overwrite existing rasters or vectors.
#' @param flags Either \code{NULL} (default) or a character vector of flags. The flag "overwrite" will be added to this if \code{replace} is \code{TRUE}.
#'
#' @return a Character vector.
#'
#' @keywords internal

.getFlags <- function(replace, flags = 'quiet') {

	if (!is.na(replace) && replace) flags <- c(flags, 'overwrite')
	flags <- unique(flags)
	flags

}
