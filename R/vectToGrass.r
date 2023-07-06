#' Convert a SpatVector to a GRASS vector
#'
#' This is a utility function that sends a `SpatVector` to an existing **GRASS** connection. It is not of use to most users. The function is based on the [rgrass::write_VECT()] function, except that it works when **fasterRaster** is attached. The reason for (more or less) duplicating this function is that the use of [methods::getMethod()] looks at the **fasterRaster** definitions for `writeVector`, and cannot find one for a `SpatVector`. To make the raster an actual `GVector`, you need to use this function then [makeGVector()] using `gn` as the argument to that function.
#'
#' @param x A `SpatVector`.
#' @param gn Character: Name of the file in **GRASS**.
#' @param flags Character vector: Flags to send to **GRASS** module `v.in.ogr`.
#'
#' @seealso [rgrass::write_VECT()], [rgrass::read_VECT()]
#' @returns `TRUE` (invisibly). Exports a `SpatVector` to an open **GRASS** connection and names it the value in `gn`.
#'
#' @example man/examples/ex_GVector.r
#'
#' @export
vectToGrass <- function(
    x,
    gn,
    flags = c('quiet', 'overwrite')
) {
    
	if (!inherits(x, 'SpatVector')) x <- terra::vect(x)

    ignore.stderr <- rgrass::get.ignore.stderrOption()
    stopifnot(is.logical(ignore.stderr))
    if (rgrass::get.suppressEchoCmdInFuncOption()) inEchoCmd <- rgrass::set.echoCmdOption(FALSE)

	gt <- terra::geomtype(x)
    type <- if (gt == 'points') {
        'point'
    } else if (gt == 'lines') {
        'line'
    } else if (gt == 'polygons') {
        'boundary'
	} else {
        stop('Unknown vector data type.')
	}
	
    tempfile <- tempfile(fileext = '.gpkg')
    terra::writeVector(x, filename=tempfile, filetype='GPKG', overwrite=TRUE)
    
    # import from disk and project
    if (crs(x) != crs()) {

        args <- list(
            cmd = 'v.import',
            input = tempfile,
            output = gn,
            extent = 'input',
            flags = flags,
            ignore.stderr = ignore.stderr,
            intern = TRUE
        )

    # import from disk, no projecting
    } else {

        args <- list(
            cmd = 'v.in.ogr',
            flags = flags,
            input = tempfile,
            output = gn,
            type = type,
            ignore.stderr = ignore.stderr,
            intern = TRUE
        )

    }
    
    do.call(rgrass::execGRASS, args=args)

	if (rgrass::get.suppressEchoCmdInFuncOption()) tull <- rgrass::set.echoCmdOption(inEchoCmd)
	invisible(TRUE)
	
}
