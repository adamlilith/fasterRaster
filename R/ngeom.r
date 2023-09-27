#' Number of geometries in a vector
#'
#' @description `ngeom()` returns the number of geometries (points, lines, or polygons) in a `GVector`.
#'
#' @param x A `GVector`.
#' @param type Character: Either "`fasterRaster`" (default) or "`GRASS`". Most users will want to know how many geometries `fasterRaster` thinks a vector has. **GRASS** counts geometries somewhat differently since multipart geometries have one **GRASS** geometry per part. Partial matching is used, and case is ignored.
#'
#' @returns An integer.
#'
#' @example man/examples/ex_GVector.r
#'
#' @seealso [nrow()]
#'
#' @aliases ngeom
#' @rdname ngeom
#' @exportMethod ngeom
methods::setMethod(
	f = "ngeom",
	signature = c(x = "GVector"),
	function(x, type = "fasterRaster") {
	
	type <- pmatchSafe(type, c("fasterRaster", "GRASS"))
	
	if (type == "fasterRaster") {
		x@nGeometries
	} else {
		sum(.vGeometries(x))
	}

)
