#' Convert a SpatRaster to a GRASS raster
#'
#' This is a utility function that sends a `SpatRaster` to an existing **GRASS** connection. It is not of use to most users. The function is based on the [rgrass::write_RAST()] function, except that it works when **fasterRaster** is attached. The reason for (more or less) duplicating this function is that the use of [methods::getMethod()] looks at the **fasterRaster** definitions for `writeVector`, and cannot find one for a `SpatVector`. To make the raster an actual `GRaster`, you need to use this function then [makeGRaster()] using `gn` as the argument to that function.
#'
#' @param x A `SpatRaster`.
#' @param gn Character: Name of the file in **GRASS**.
#' @param flags Character vector: Flags to send to **GRASS** module `r.in.gdal`.
#'
#' @seealso [rgrass::write_RAST()], [rgrass::read_RAST()]
#' @returns `TRUE` (invisibly). Exports a `SpatRaster` to an open **GRASS** connection and names it the value in `gn`.
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @export
rastToGrass <- function(x, gn, flags = c('quiet', 'overwrite')) {

	if (!(requireNamespace('terra', quietly=TRUE))) stop('Package terra is required for handling SpatRasters.')
    stopifnot(is.character(gn))
    if (!is.null(flags)) stopifnot(is.character(flags))

	if (inherits(x, 'SpatRaster')) {
        
# Suggestion https://github.com/rsbivand/rgrass/pull/45#discussion_r816113064 Floris Vanderhaeghe
        srcs <- terra::sources(x)
        mems <- terra::inMemory(x)
        if (length(srcs) == 1L && !mems[1]) {
			tf <- srcs[1]
        } else {
			tf <- ''
		}
		
        if (!file.exists(tf)) {

            drv <- 'RRASTER'
            fxt <- '.grd'
            gdalver <- gsub('[A-Za-z]', '', strsplit(terra::gdal(), '-')[[1]][1])
            if (gdalver < '2.3.0') {
                drv <- 'GTiff'
                fxt <- '.tif'
            }
            tf <- tempfile(fileext=fxt)
            res <- terra::writeRaster(x, filename=tf, overwrite=TRUE, filetype=drv)
            tmpfl <- TRUE

        } else {
            res <- x
            tmpfl <- FALSE
        }
        rgrass::execGRASS('r.in.gdal', flags=flags, input=tf, output=gn)

#        if (tmpfl) unlink(tf)
        if (terra::nlyr(x) == 1L) {
            xcats <- terra::cats(x)[[1L]]
            if (!is.null(xcats)) {
                tfc <- tempfile()
                write.table(xcats, tfc, sep=':', row.names=FALSE, col.names=FALSE, quote=FALSE)
                rgrass::execGRASS('r.category', map=gn, rules=tfc, separator=':')
            }
        }
    } else {
        stop('Object is not a SpatRaster.')
    }
    invisible(res)

}
