#' Calculate pff (connectivity between cells) of a binary matrix
#' 
#' Calculates connectivity between cells from a binary raster. Two cells are connected if they both have a value of 1 and are neighbors in a  cardinal direction of one another.  From Riitters, K., J. Wickham, R. O'Neill, B. Jones, and E. Smith. 2000. Global-scale patterns of forest fragmentation. Conservation Ecology 4:3. URL: https://www.jstor.org/stable/26271763. (Also note the erratum to the paper on their classification scheme at https://www.ecologyandsociety.org/vol4/iss2/art3/errata/january26.2001.html).
#' @param x Binary matrix, with values equal to 0, 1, or \code{NA}.
#' @param na.rm Logical, if \code{FALSE} (default) count \code{NA} cells towards cells that could be occupied/connected.
#' @return Numeric in the range [0, 1] or \code{NA} if all cells are \code{NA}.
#' @examples
#' x <- runif(100)
#' x <- x >= 0.7
#' x <- matrix(x, nrow=10)
#' fasterRaster:::.fragConnect(x)
#' diag(x) <- NA
#' fasterRaster:::.fragConnect(x)
#' @seealso \code{\link[fasterRaster]{fragmentation}}, \code{\link[fasterRaster]{fasterFragmentation}}
#' @keywords internal
.fragConnect <- compiler::cmpfun(function(x, na.rm=FALSE) {

	if (all(is.na(x))) return(NA)
	
	x <- matrix(x, nrow=round(sqrt(length(x))), byrow=FALSE)

	# get subsections of matrix
	xUpper <- x[1:(nrow(x) - 1), ]
	xLower <- x[2:nrow(x), ]

	xLeft <- x[ , 1:(ncol(x) - 1)]
	xRight <- x[ , 2:ncol(x)]

	# enumerate adjacencies
	rowsAdj <- xUpper == xLower & xUpper == 1 & xLower == 1
	colsAdj <- xLeft == xRight & xLeft == 1 & xRight == 1
	adj <- sum(rowsAdj, colsAdj, na.rm=TRUE)
	
	# enumerate possible adjacencies
	rowsPoss <- (xUpper == 1 | xLower == 1) & !is.na(xUpper) & !is.na(xLower)
	colsPoss <- (xLeft == 1 | xRight == 1) & !is.na(xLeft) & !is.na(xRight)
	if (na.rm) {
		rowsPoss <- rowsPoss & !is.na(xUpper) & !is.na(xLower)
		colsPoss <- colsPoss & !is.na(xLeft) & !is.na(xRight)
	}
	possible <- sum(rowsPoss, colsPoss)
	
	connect <- if (possible == 0) {
		0
	} else {
		adj / possible
	}
	
	connect

})
