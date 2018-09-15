#' Classify fragmentation from pf and pff
#' 
#' This function classifies fragmentation based on pf (density) and pff (conditional connectivity) metrics from Riitters, K., J. Wickham, R. O'Neill, B. Jones, and E. Smith. 2000. Global-scale patterns of forest fragmentation. Conservation Ecology 4:3. URL: https://www.jstor.org/stable/26271763. (Also note the erratum to the paper on their classification scheme at https://www.ecologyandsociety.org/vol4/iss2/art3/errata/january26.2001.html).
#' @param x 2-element numeric vector, first element is pf, second is pff.
#' @return Integer values from the erratum to Ritter et al. (2000):
#' \itemize{
#' 	\item \code{NA}: \code{NA}
#' 	\item \code{0}: No forest (or whatever is being evaluated)
#'	\item \code{1}: interior (\code{pf} == 1)
#'	\item \code{2}: patch (\code{pf} < 0.4)
#'	\item \code{3}: transitional (0.4 <= \code{pf} < 0.6)
#'	\item \code{4}: perforated (\code{pf} >= 0.6 & \code{pf - pff} > 0)
#'	\item \code{5}: edge (\code{pf} >= 0.6 & \code{pf - pff} < 0)
#'	\item \code{6}: undetermined (\code{pf} >= 0.6 & \code{pf == pff})
#' }
#' @example
#' .classifyFrag(c(NA, 1)) # NA
#' .classifyFrag(c(0, 1)) # nothing 0
#' .classifyFrag(c(1, 1)) # interior 1
#' .classifyFrag(c(0.2, 1)) # patch 2
#' .classifyFrag(c(0.5, 1)) # transitional 3
#' .classifyFrag(c(0.7, 0.2)) # perforated 4
#' .classifyFrag(c(0.7, 0.9)) # edge 5
#' .classifyFrag(c(0.7, 0.7)) # undetermined 6
#' @seealso \code{\link[fasterRaster]{.pf}},  \code{\link[fasterRaster]{.pff}}, \code{\link[fasterRaster]{frag}}, \code{\link[fasterRaster]{fasterFrag}}
#' @export
.classifyFrag <- compiler::cmpfun(function(x) {

	pf <- x[1]
	pff <- x[2]

	if (anyNA(x)) {
		NA
	} else if (pf == 0) { # no thing but not nothing
		0
	} else if (pf > 1 - omnibus::eps()) { # interior
		1
	} else if (pf < 0.4) { # patch
		2
	} else if (pf >= 0.4 & pf < 0.6) { # transitional
		3
	} else if (pf >= 0.6 & pf - pff > 0) { # perforated
		4
	} else if (pf >= 0.6 & pf - pff < 0) { # edge
		5
	} else if (pf >= 0.6 & pf - pff == 0) { # undetermined
		6
	}

})

