#' Plot a 'GRaster' or 'GVector'
#'
#' Create a plot of a 'GRaster' or 'GVector'. This function starts the **GRASS** GUI and displays the desired image. *NOTE* that it is possible to do manipulations on rasters/vectors in the GUI, but they will not necessarily be reflected in **RR** objects that represent them. So, only viewing (zooming, panning, etc.) is advised.  All other changes made are at the user's risk.
#'
#' @seealso [plot()] and [terra::plot()]; **GRASS** module [https://grass.osgeo.org/grass82/manuals/g.gui.html](g.gui)
#'
#' @example man/examples/example_plot.r
#'
#' @export

if (!isGeneric('plot')) setGeneric('plot', function(x, y, ...) standardGeneric('plot'))

setMethod(
	'plot',
	signature(x = 'GRaster', y='missing'),
	function(x, y, ...) {
	
	# rgrass::execGRASS('g.gui', ui='wxpython', flags='quiet')
	# rgrass::execGRASS('d.rast', map=.gname(x), flags='quiet')

	rgrass::execGRASS('d.mon', flags='x', start='wx0')
	rgrass::execGRASS('d.rast', map=.gname(x), flags='quiet')
	
	} # EOF
)
