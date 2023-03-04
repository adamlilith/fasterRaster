#' Contour lines from a 'GRaster'
#'
#' Create a spatial vector of contour lines from a raster.
#'
#' @seealso [terra::as.contour()]; **GRASS** module \code{\href{https://grass.osgeo.org/grass82/manuals/r.contour.html}{r.contour}}
#'
#' @example man/examples/example_asContour.r
#'
#' @export

if (!isGeneric('as.contour')) as.contour.GRaster <- setGeneric('as.contour', function(x, nlevels, levels) standardGeneric('as.contour'))

setMethod(
	'as.contour',
	signature(x = 'GRaster'),
	function(x, nlevels, levels) {
	
	### commons
	###########
	
	.restore(x)
	regionReshape(x)

	### end commons
	###############

	if (!missing(nlevels) & !missing(levels)) stop('Please specify either <nlevels> or <levels>, but not both.')
	if (missing(nlevels) & missing(levels)) stop('Please specify either <nlevels> or <levels>.')

	if (!missing(nlevels)) {
		minMax <- minmax(x)
		levels <- seq(minMax[1L, 1L], minMax[2L, 1L], length.out=nlevels)
	}


	input <- .gname(x)
	output <- .makeGname(rastOrVect = 'vector')

	### execute
	args <- list(
		cmd = 'r.contour',
		input = input,
		output = output,
		levels = levels,
		cut = 2,
		flags = 'quiet'
	)
	
	do.call(rgrass::execGRASS, args=args)
	
	info <- .vectInfo(output)
	out <- GVector(
		location = getFastOptions('location'),
		mapset = getFastOptions('mapset'),
		crs = crs(x),
		gname = output,
		extent = c(info[['west']][1L], info[['east']][1L], info[['south']][1L], info[['north']][1L]),
		topology = info[['topology']][1L],
		geometry = info[['geometry']][1L],
		bottom = info[['bottom']],
		top = info[['top']],
		fields = info[['fields']],
		fieldClasses = info[['fieldClasses']]
	)
	out
	
	} # EOF
)

