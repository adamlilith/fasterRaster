#' Topographic wetness index raster
#'
#' This function creates a raster where cell values represent the Topographic Wetness Index (TPI), a measure of how much water drains or pools into a cell. It utilizes the \code{GRASS} function \code{r.topidx}.
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when calculating horizon height (i.e., function \code{r.horizon} in \code{GRASS}).
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same coordinate reference system, extent, and resolution as \code{rast}. Otherwise, a raster is written into the \code{GRASS} session. The name of this vector is given by \code{outGrassName}.
#' @details See the documentation for the \code{GRASS} module \code{r.topidx}{https://grass.osgeo.org/grass82/manuals/r.topidx.html}.
#' @seealso \code{\link[terra]{terrain}}
#'
#' @examples man/examples/ex_fasterTopidx.r
#'
#' @export

fasterTopidx <- function(
	rast,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	outGrassName = 'topoWetnessIndex',
	...
) {

	out <- faster(
		mod = 'r.topidx',
		rast = rast,
		input = 'rast',
		outType = 'raster',
		output = outGrassName,
		grassDir = grassDir,
		grassToR = grassToR
	)
	
	out
	
}
