#' Topographic wetness index raster
#'
#' This function creates a raster where cell values represent the Topographic Wetness Index (TPI), a measure of how much water drains or pools into a cell. It utilizes the \code{GRASS} function \code{r.topidx}.
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when calculating horizon height (i.e., function \code{r.horizon} in \code{GRASS}).
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same coordinate reference system, extent, and resolution as \code{rast}. Regardless, a raster is written into the \code{GRASS} session. The name of this vector is given by \code{outGrassName}.
#'
#' @seealso \code{\link[terra]{terrain}} in \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/r.topidx.html}{\code{r.topidx}} in \code{GRASS} 
#'
#' @example man/examples/ex_fasterTopidx.r
#'
#' @export

fasterTopidx <- function(
	rast,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	inRastName = ifelse(is.null(names(rast)), 'rast', names(rast)),
	outGrassName = 'twiRast',
	...
) {

	out <- faster(
		mod = 'r.topidx',
		rast = rast,
		input = 'rast',
		outType = 'raster',
		inRastName = inRastName,
		output = outGrassName,
		grassDir = grassDir,
		grassToR = grassToR
	)
	
	out
	
}
