#' Calculate pf (proportion of cells occupied) from a binary matrix
#' 
#' Calculates proportion of cells occupied (values = 1). Riitters, K., J. Wickham, R. O'Neill, B. Jones, and E. Smith. 2000. Global-scale patterns of forest fragmentation. Conservation Ecology 4:3. URL: https://www.jstor.org/stable/26271763. (Also note the erratum to the paper on their classification scheme at https://www.ecologyandsociety.org/vol4/iss2/art3/errata/january26.2001.html).
#' @param x Binary matrix, with values equal to 0, 1, or \code{NA}.
#' @param na.rm Logical, if \code{FALSE} (default) count \code{NA} cells towards cells that could be occupied.
#' @return Numeric in the range [0, 1] or \code{NA} if all cells are \code{NA}.
#' @example
#' x <- runif(100)
#' x <- x >= 0.7
#' x <- matrix(x, nrow=10)
#' fasterRaster:::.pfFunct(x)
#' diag(x) <- NA
#' fasterRaster:::.pfFunct(x)
#' @seealso \code{\link[fasterRaster]{.pf}},  \code{\link[fasterRaster]{.codeClassify}}, \code{\link[fasterRaster]{frag}}, \code{\link[fasterRaster]{fasterFrag}}
#' @export
.pf <- compiler::cmpfun(function(x, na.rm=FALSE) {
	
	# x 	binary matrix possibly with NAs
	# na.rm logical
	
	if (all(is.na(x))) return(NA)
	x <- matrix(x, nrow=round(sqrt(length(x))), byrow=TRUE)
	
	n <- if (na.rm) {
		sum(!is.na(x))
	} else {
		prod(dim(x))
	}
	
	if (n == 0) {
		NA
	} else {
		sum(x, na.rm=TRUE) / n
	}

})

