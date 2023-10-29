#' Number of geometries and subgeometries in a vector
#'
#' @description `GVector`s represent two types of "geometries". In "singlepart" geometries, each point, set of connected line segments, or polygon is treated like its own feature and has its own row in an attribute table. For example, a province might be composed of islands. In this case, each island would be represented as its own feature and could have its own row in the attribute indicating, say, the name and area of each island.
#' 
#' In "multipart" geometries, features are collected together and thu manipulated as if they were a single feature and have a singe line in an attribute table. Each multipart feature can contain one or more singlepart features. For example, all of the islands comprising  province would be collated together and have a single row in the attribute table indicating the name of the province and the area of the entire province.
#' 
#' `ngeom()` returns the number of geometries. Singlepart features are treated as one geometry each, and multipart features are treated as one geometry each.
#' 
#' `nsubgeom()` Returns the number of subgeometries. Singlepart geometries each represent a single subgeometry. Multipart geometries represent one or more subgeometries. The number of subgeometries will thus always be the same as or more than the number of geometries.
#'
#' @param x A `GVector`.
#'
#' @returns An integer.
#'
#' @example man/examples/ex_GVector.r
#'
#' @seealso [nrow()], [dim()]
#'
#' @aliases ngeom
#' @rdname ngeom
#' @exportMethod ngeom
methods::setMethod(
	f = "ngeom",
	signature = c(x = "GVector"),
	function(x, type = "fasterRaster") x@nGeometries
)

#' @aliases nsubgeom
#' @rdname ngeom
#' @exportMethod nsubgeom
methods::setMethod(
	f = "nsubgeom",
	signature = c(x = "GVector"),
	function(x) length(.vCats(x, table = FALSE))
)
