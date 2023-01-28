#' Save a raster exported from GRASS to R to disk
#'
#' For some reason, if you create or export a raster to \code{GRASS}, then import it back to \code{R}, saving it using \pkg{terra}'s \code{\link[terra]{writeRaster}} function automatically forces the \code{datatype} of the raster to a low-bit integer. As a result, values are truncated to integers and forced to a specific range (or coerced to \code{NA}). You can overcome this simply by saving these rasters using, for example:\cr
#'
#' \code{writeRaster(rasterToSave, 'C:/pathToSave/fileName.tif', datatype='FLT4S')}\cr or
#' \code{writeRaster(rasterToSave, 'C:/pathToSave/fileName.tif', datatype='FLT8S')}\cr
#'
#' Here, the important part is the \code{datatype} argument, which in these two examples says to save the cell values as a 32-bit or 64-bit floating point values that are signed (can be negative or positive). Alternatively, you can also use these functions as shorthand to save in 32- or 64-bit signed, floating format:\cr
#' 
#' \code{writeRaster4(rasterToSave, 'C:/pathToSaveTo/fileName.tif')}\cr
#' \code{writeRaster8(rasterToSave, 'C:/pathToSaveTo/fileName.tif')}\cr
#'
#' You can also use \code{\link{fasterWriteRaster}} to save a raster directly from \code{GRASS} to disk.
#'
#' @param x \code{SpatRaster}.
#' @param filename Name of the path and file to save to.
#' @param overwrite If \code{FALSE} (default), do not overwrite existing files.
#' @param ... Additional arguments to send to \code{\link[terra]{writeRaster}}.
#'
#' @return A \code{SpatRaster}. Also saves one or more \code{SpatRaster}s to disk.
#'
#' @seealso \code{\link{fasterWriteRaster}} and \code{\link{fasterInfo}} in \pkg{fasterRaster}. Also see \code{\link[terra]{writeRaster}} for more details and examples.
#'
#' @export

writeRaster4 <- function(
	x,
	filename,
	overwrite = FALSE,
	datatype = 'FLT4S',
	...
) {
	terra::writeRaster(x=x, filename=filename, overwrite=overwrite, datatype=datatype, ...)
}

#' @name writeRasterFaster8
#' @title Save a raster exported from GRASS to disk
#' @rdname fasterRaster4
#' @export
writeRaster8 <- function(
	x,
	filename,
	overwrite = FALSE,
	datatype = 'FLT8S',
	...
) {
	terra::writeRaster(x=x, filename=filename, overwrite=overwrite, datatype=datatype, ...)
}
