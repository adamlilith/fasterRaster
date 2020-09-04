#' Calculate density (pf, or proportion of cells occupied) from a binary matrix
#' 
#' Calculates proportion of cells occupied (values = 1). Riitters, K., J. Wickham, R. O'Neill, B. Jones, and E. Smith. 2000. Global-scale patterns of forest fragmentation. Conservation Ecology 4:3. URL: https://www.jstor.org/stable/26271763. (Also note the erratum to the paper on their classification scheme at https://www.ecologyandsociety.org/vol4/iss2/art3/errata/january26.2001.html).
#' @param x Binary matrix, with values equal to 0, 1, or \code{NA}.
#' @param na.rm Logical, if \code{FALSE} (default) count \code{NA} cells towards cells that could be occupied.
#' @return Numeric in the range [0, 1] or \code{NA} if all cells are \code{NA}.
#' @examples
#' x <- runif(100)
#' x <- x >= 0.7
#' x <- matrix(x, nrow=10)
#' fasterRaster:::.fragDensity(x)
#' diag(x) <- NA
#' fasterRaster:::.fragDensity(x)
#' @seealso \code{\link[fasterRaster]{fragmentation}}, \code{\link[fasterRaster]{fasterFragmentation}}
#' @keywords internal
.fragDensity <- compiler::cmpfun(function(x, na.rm=FALSE) {
	
	if (all(is.na(x))) return(NA)

	x <- matrix(x, nrow=round(sqrt(length(x))), byrow=FALSE)
	
	# number of valid cells
	n <- if (na.rm) {
		sum(!is.na(x))
	} else {
		nrow(x) * ncol(x)
	}
	
	# density
	occ <- if (n == 0) {
		0
	} else {
		sum(x, na.rm=TRUE)
	}
	
	occ / n

})

