#' Neighborhood matrix from a polygons GVector
#'
#' @description This function returns a neighborhood matrix from a polygons `GVector`, which represents which geometries touch one another. It is useful for implementing geostatistical analyses that require indicators about which area features are next to one another.
#'
#' Polygons must share more than one point for them to be considered a neighbors (i.e., same as `spdep::poly2nb(x, queen = FALSE)`).
#'
#' This function needs the **GRASS** addon `v.neighborhoodmatrix`. If it is not installed, it will try to install it.
#'
#' @param x A polygons `GVector.
#'
#' @returns A `list`. Each element represents a polygon. If an element is empty, it has no neighbors. Otherwise, it is a vector of integers, which represent the indices of the polygon(s) to which it is a neighbor.
#'
#' @example man/examples/ex_neighborhoodMatrix.r
#'
#' @aliases neighborhoodMatrix
#' @rdname neighborhoodMatrix
#' @exportMethod neighborhoodMatrix
methods::setMethod(
	f = "neighborhoodMatrix",
	signature = c(x = "GVector"),
	definition = function(x) {

	if (geomtype(x) != "polygons") stop("The input must be a GVector representing polygons.")

	.addons('v.neighborhoodmatrix')

	neighs <- rgrass::execGRASS(
		cmd = "v.neighborhoodmatrix",
		input = sources(x),
		separator = 'pipe',
		flags = c(.quiet(), "b"),
		intern = TRUE
	)

	out <- vector(mode = "list", length = ngeom(x))
	for (i in seq_along(out)) out[[i]] <- numeric()

	neighs <- strsplit(neighs, split = "\\|")
	neighs <- lapply(neighs, as.integer)

	for (i in seq_along(neighs)) {

		first <- neighs[[i]][1L]
		remainder <- neighs[[i]][2L:length(neighs[[i]])]

		remainder <- remainder[remainder != first]

		out[[first]] <- c(out[[first]], remainder)

	}
	out


	} # EOF
)

#' @aliases neighbourhoodMatrix
#' @rdname neighborhoodMatrix
#' @exportMethod neighbourhoodMatrix
methods::setMethod(
	f = "neighbourhoodMatrix",
	signature = c(x = "GVector"),
	definition = function(x) neighborhoodMatrix(x)
)
