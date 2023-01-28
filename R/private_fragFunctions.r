#' Classify fragmentation from density (pf) and connectivity (pff)
#' 
#' This function classifies fragmentation based on pf (density) and pff (conditional connectivity) metrics from Riitters, K., J. Wickham, \pkg{R}. O'Neill, B. Jones, and E. Smith. 2000. Global-scale patterns of forest fragmentation. Conservation Ecology 4:3. URL: <https://www.jstor.org/stable/26271763>. (Also note the erratum to the paper on their classification scheme at <https://www.ecologyandsociety.org/vol4/iss2/art3/errata/january26.2001.html>).
#' @param x 2-element numeric vector, first element is pf (density), second is pff (connectivity).
#' @param undet Character. This defines what is done with "undetermined" cases (when density is >= 0.6 and density == connectivity). Possible values include (partial matching of strings is used):
#' \itemize{
#' 	\item \code{undetermined}: Undetermined cases will be assigned a value of 5 (which is not assigned to any other case; default).
#' 	\item \code{perforated}: Undetermined cases will be assigned a value of 3 ("perforated").
#' 	\item \code{edge}: Undetermined cases will be assigned a value of 4 ("edge").
#' 	\item \code{random}: Undetermined cases will be assigned a value of 3 or 4 at random ("perforated" or "edge").
#' }
#' @param ... Other arguments (not used).
#' @return Integer values from the erratum to Ritter et al. (2000):
#' \itemize{
#' 	\item \code{NA}: \code{NA}
#' 	\item \code{0}: No forest (or whatever is being evaluated)
#'	\item \code{1}: patch (\code{pf} < 0.4)
#'	\item \code{2}: transitional (0.4 <= \code{pf} < 0.6)
#'	\item \code{3}: perforated (\code{pf} >= 0.6 & \code{pf - pff} > 0)
#'	\item \code{4}: edge (\code{pf} >= 0.6 & \code{pf - pff} < 0)
#'	\item \code{5}: undetermined (\code{pf} >= 0.6 & \code{pf == pff})
#'	\item \code{6}: interior (\code{pf} == 1)
#' }
#' Note that this differs somewhat from the numbering scheme presented by Riitters et al. (2000) and their errata.
#' @examples
#' .fragClassify(c(NA, 1)) # NA
#' .fragClassify(c(0, 0)) # nothing 0
#' .fragClassify(c(1, 1)) # interior 6
#' .fragClassify(c(0.2, 1)) # patch 1
#' .fragClassify(c(0.5, 1)) # transitional 2
#' .fragClassify(c(0.7, 0.2)) # perforated 3
#' .fragClassify(c(0.7, 0.9)) # edge 4
#' .fragClassify(c(0.7, 0.7)) # undetermined 5
#' @seealso \code{\link[fasterRaster]{fragmentation}}, \code{\link[fasterRaster]{fasterFragmentation}}
#' @keywords internal
.fragClassify <- compiler::cmpfun(function(x, undet='undetermined', ...) {

	pf <- x[1]
	pff <- x[2]

	if (anyNA(x)) {
		NA
	} else if (pf == 0) { # no thing but not nothing
		0
	} else if (pf > 1 - omnibus::eps()) { # interior
		6
	} else if (pf < 0.4) { # patch
		1
	} else if (pf >= 0.4 & pf < 0.6) { # transitional
		2
	} else if (pf >= 0.6 & pf > pff) { # perforated
		3
	} else if (pf >= 0.6 & pf < pff) { # edge
		4
	} else if (pf >= 0.6 & pf == pff) { # undetermined
		if (pmatch(undet, c('undetermined', 'perforated', 'edge', 'random')) == 1) { # undetermined --> undetermined
			5
		} else if (pmatch(undet, c('undetermined', 'perforated', 'edge', 'random')) == 2) { # undetermined --> perforated
			3
		} else if (pmatch(undet, c('undetermined', 'perforated', 'edge', 'random')) == 3) { # undetermined --> edge
			4
		} else if (pmatch(undet, c('undetermined', 'perforated', 'edge', 'random')) == 4) { # undetermined --> perforated or edge
			sample(c(3, 4), 1)
		}
	}

})

#' Calculate pff (connectivity between cells) of a binary matrix
#' 
#' Calculates connectivity between cells from a binary raster. Two cells are connected if they both have a value of 1 and are neighbors in a  cardinal direction of one another.  From Riitters, K., J. Wickham, \pkg{R}. O'Neill, B. Jones, and E. Smith. 2000. Global-scale patterns of forest fragmentation. Conservation Ecology 4:3. URL: https://www.jstor.org/stable/26271763. (Also note the erratum to the paper on their classification scheme at https://www.ecologyandsociety.org/vol4/iss2/art3/errata/january26.2001.html).
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

#' Calculate density (pf, or proportion of cells occupied) from a binary matrix
#' 
#' Calculates proportion of cells occupied (values = 1). Riitters, K., J. Wickham, \pkg{R}. O'Neill, B. Jones, and E. Smith. 2000. Global-scale patterns of forest fragmentation. Conservation Ecology 4:3. URL: https://www.jstor.org/stable/26271763. (Also note the erratum to the paper on their classification scheme at https://www.ecologyandsociety.org/vol4/iss2/art3/errata/january26.2001.html).
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
