#' "Stack" 'GRaster's
#'
#' `GRaster`s can be "stacked" using this function, effectively creating a mult-layered raster set.
#'
#' @param ... Two or more `GRaster`s in the same **GRASS** [location] and mapset.
#'
#' @return A `GRaster`.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export

if (!isGeneric('c')) c.GRaster <- setGeneric(name='c', def=function(x, ...) { standardGeneric('c') })

setMethod(f = 'c',
	signature = 'GRaster',
	definition = function(x, ...) {
	
	out <- x
	dots <- list(...)
	
	xloc <- fastLocation(x)
	xmapset <- fastMapset(x)
	xres <- res(x)
	xdim <- dim(x)
	xext <- ext(x)
	
	for (i in seq_along(dots)) {
	
		if (!inherits(dots[[i]], 'GRaster')) stop('Can only combine GRasters.')
		
		dloc <- fastLocation(dots[[i]])
		dmapset <- fastMapset(dots[[i]])
		dres <- res(dots[[i]])
		ddim <- dim(dots[[i]])
		dext <- ext(dots[[i]])
		
		if (xloc != dloc | xmapset != dmapset) stop('Can only combine GRasters in the same location and mapset.')
		if (compareFloat(xres[1L], dres[1L], '!=') | compareFloat(xres[2L], dres[2L], '!=')) stop('Can only combine GRasters with the same resolution.')
		if (xdim[1L] != ddim[1L] | xdim[2L] != ddim[2L]) stop('Can only combine GRasters with the same x- and y-dimensions.')
		if (compareFloat(xext[1L], dext[1L], '!=') |
			compareFloat(xext[2L], dext[2L], '!=') |
			compareFloat(xext[3L], dext[3L], '!=') |
			compareFloat(xext[4L], dext[4L], '!=')) stop('Can only combine GRasters with the same extents.')
		
		gname <- .gname(dots[[i]])
		rname <- rname(dots[[i]])
		info <- .rastInfo(gname)
		
		out <- GRaster(
			location = out@location,
			mapset = out@mapset,
			crs = crs(out),
			gname = c(out@gname, gname),
			rname = c(out@rname, rname),
			topology = c(out@topology, info[['topology']][1L]),
			extent = out@extent,
			ztop = c(out@ztop, info[['ztop']]),
			zbottom = c(out@zbottom, info[['zbottom']]),
			datatypeGRASS = c(out@datatypeGRASS, info[['grassDataType']]),
			dimensions = c(out@dimensions[1L:3L], out@dimensions[4L] + nlyr(dots[[i]])),
			resolution = out@resolution,
			numCategories = c(out@numCategories, info[['numCategories']]),
			minVal = c(out@minVal, info[['minVal']]),
			maxVal = c(out@maxVal, info[['maxVal']])
		)
	
	} # next GRaster to combine
	
	out
	
	} # EOF
)
