#' Scatterplot of values in each GRaster layer against the others
#'
#' @description `pairs()` generates a scatterplot between values of cells in each layer of a `GRaster` against all the other layers.
#' 
#' @param x A `GRaster` with two or more layers.
#' 
#' @param n A numeric integer, integer, or `NULL` (default): Number of cells to sample. If `NULL`, 50% of the total number of cells will be used.
#' 
#' @param ... Arguments to send to [graphics::pairs()] (which typically sends them to [graphics::plot()]). Special arguments for affecting certain panels include:
#' * Correlation panels -- `cor.cex`: Size of the values of the correlation coefficients.
#' * Histogram panels -- `hist.col`: Histogram color.
#' * Dot-plot panels -- `colramp`: Function taking an integer `n` as input and returning `n` names of colors. The default is: `colorRampPalette(c("white", blues9))`.
#' 
#' @returns Nothing (creates a plot).
#' 
#' @example man/examples/ex_GRaster.
#' 
#' @aliases pairs
#' @rdname pairs
#' @exportMethod pairs
methods::setMethod(
	f = "pairs",
	signature = c(x = "GRaster"),
	function(x, n = NULL, ...) {

	.restore(x)
	region(x)

 	if (is.null(n)) n <- round(0.5 * ncell(x))

	samps <- spatSample(x, size = n, values = TRUE, xy = FALSE, as.points = FALSE)

	dots <- list(...)
	if (!("cex" %in% names(dots))) dots$cex <- 0.5
	if (!("pch" %in% names(dots))) dots$pch <- "."

	args <- list(
		x = samps,
		lower.panel = .panelSS,
		upper.panel = .panelCor,
		diag.panel = .panelHist
	)
	args <- c(args, dots)

	do.call(graphics::pairs, args = args)

	} # EOF
)

#' From [graphics::pairs()] help
#' @noRd
.panelCor <- function(x, y, cor.cex = 2, digits = 2, ...) {

	usr <- par(usr = c(0, 1, 0, 1))
	on.exit(graphics::par(usr = usr), add = TRUE)
	r <- abs(stats::cor(x, y))
	txt <- format(c(r, 0.123456789), digits = digits)[1L]
	text(0.5, 0.5, txt, cex = max(1, r * cor.cex))

}

#' From [graphics::pairs()] help
#' @noRd
.panelHist <- function(x, breaks = 20, hist.col = "firebrick3", ...) {

    usr <- par("usr")
    par(usr = c(usr[1L:2L], 0, 1.5))

    h <- graphics::hist(x, breaks = breaks, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y / max(y)
    graphics::rect(breaks[-nB], 0, breaks[-1L], y, col = hist.col)

}

#' @noRd
.panelSS <- function(x, y, colramp = colorRampPalette(c("white", blues9)), ...) {

	usr <- par(usr = c(0, 1, 0, 1), new = TRUE)
	on.exit(graphics::par(usr = usr), add = TRUE)
  graphics::smoothScatter(x, y, colramp = colramp, nrpoints = Inf, ...)

}
