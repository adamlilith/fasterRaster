#' Save one or more rasters to disk directly from a GRASS session
#'
#' This function saves a raster to disk directly from a \code{GRASS} session. If you want to save a raster from \code{R}, use \code{\link[terra]{writeRaster}} in the \pkg{terra} package.  However, if the raster in \code{R} was produced in \code{GRASS}, please consult \code{\link{writeRaster4}} for important details! Please note that rasters saved by \code{GRASS} can sometimes be much larger than their counterparts saved from \code{R}.\cr
#' The filetype will be attempted to be ascertained from the file extension, but you can specify the format using the \code{format} argument (see entry for \code{...}). You can see a list of supported formats by simply using this function with no arguments, as in \code{fasterWriteRaster()}, or also at \href{https://grass.osgeo.org/grass82/manuals/r.out.gdal.html}{\code{r.out.gdal}}.
#'
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_autoRegion
#' @inheritParams .sharedArgs_grassDir
#'
#' @param rast The name(s) of one or more rasters in the active \code{GRASS} session.
#' @param filename Path and file name(s). If \code{rast} has more than one name, then you should state multiple file names, one per raster. The function will attempt to ascertain the type of the raster from the filename extension, but you can also set it using \code{format} (see the \code{...} argument).
#' @param overwrite If \code{FALSE} (default), do not save over existing file(s).
#' @param datatype The datatype of the values stored in non-ASCII rasters. If \code{NULL}, this will be taken from the raster (see \code{\link{fasterInfo}}. Alternatively, users can state the data type they desire.  This can any of:
#' \itemize{
#'	\item \code{'INT1U'} or \code{'Byte'}: Integer values from 0 to 255
#'	\item \code{'INT2U'} or \code{'UInt16'}: Integer values from 0 to 65,534
#'	\item \code{'INT2S'} or \code{'Int16'}: Integer values from -32,767 to -32,767
#'	\item \code{'INT4S'} or \code{'Int32'}: Integer values from -2,147,483,647 to 2,147,483,647
#'	\item \code{'FLT4S'} or \code{'Float32'}: Values from -3.4e+38 to 3.4e+38, including decimal values
#'	\item \code{'FLT8S'} or \code{'Float64'}: Values from -1.7e+308 to 1.7e+308, including decimal values
#'}
#' Note that the there are more types available. See \code{\href{https://grass.osgeo.org/grass82/manuals/r.out.gdal.html}{r.out.gdal}}.
#' @param compressTiff Type of compression for GeoTIFF files. The default is \code{'DEFLATE'}, but can be any of \code{'LZW'}, \code{'PACKBITS'}, \code{'LZMA'}, or \code{NULL}. If \code{NULL}, then no compression is conducted, but the file can be reduced in size by using zip, gzip, or other compressions.
#' @param bigTiff If \code{TRUE}, and the file format is a GeoTIFF and would be larger than 4 GB (regardless of compression), then the file will be saved in BIGTIFF format.
#' @param ... Arguments to send to \code{\href{https://grass.osgeo.org/grass82/manuals/r.out.gdal.html}{thisRast.out.gdal}} and \code{\href{https://grass.osgeo.org/grass82/manuals/r.out.ascii.html}{r.out.ascii}}. These include a \code{format} argument, which explicitly states the file format. Some common formats include:
#' \itemize{
#' 	\item \code{'GTiff'} (default): GeoTIFF (\code{filename} ends in \code{.tif})
#' 	\item \code{'ASC'}: ASCII (\code{filename} ends in \code{.asc})
#' }
#' For the full list, please see \code{\href{https://grass.osgeo.org/grass82/manuals/r.out.gdal.html}{r.out.gdal}} and \code{\href{https://grass.osgeo.org/grass82/manuals/r.out.ascii.html}{r.out.ascii}}. When using other formats, you may have to specify the \code{createopts} argument, too. \cr
#' For ASCII files, you may also need to state the number of significant digits saved using a \code{precision} argument. 32-bit values have 7 digits and 64-bit values have 16. So in these cases the argument would be \code{precision=7} or \code{precision=16}.
#'
#' @return A connecton to the written raster (invisibly). Also writes one or more files to disk.
#'
#' @seealso \code{\link{writeRaster4}} and \code{\link{writeRaster8}} for saving a raster created in \code{GRASS} then imported to \code{R} and \code{\link{rastFromGrass}} for importing rasters from \code{GRASS}; \code{\link[terra]{writeRaster}} in \pkg{terra}; \code{GRASS} modules \code{\href{https://grass.osgeo.org/grass82/manuals/r.out.gdal.html}{r.out.gdal}} and \code{\href{https://grass.osgeo.org/grass82/manuals/r.out.ascii.html}{r.out.ascii}}
#'
#' @example man/examples/ex_fasterWriteRaster.r
#'
#' @export

fasterWriteRaster <- function(
	rast,
	filename,
	overwrite = FALSE,
	datatype = NULL,
	compressTiff = 'DEFLATE',
	bigTiff = FALSE,
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	...
) {

	### display supported formats
	if (missing(rast)) {
	
		forms <- rgrass::execGRASS('r.out.gdal', flags='l', intern=TRUE)
		forms <- forms[forms != 'Supported formats:']
		forms <- trimws(forms)
		forms <- sort(forms)
		forms <- c('Supported raster file formats:', forms)
		cat(paste(forms, collapse='\n'))
		cat('\n')
		flush.console()
	
	### if wanting to write a raster
	} else {

		flags <- c('quiet')
		if (overwrite) flags <- c(flags, 'overwrite')

		### commons
		###########

			### restore
			# on.exit(.restoreLocation(), add=TRUE) # return to starting location
			if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		###############
		### end commons

		# going to overwrite anything?
		if (!overwrite) {
			for (i in seq_along(rast)) {
				if (file.exists(filename[i])) stop(paste0('File already exists and "overwrite" is FALSE:\n ', filename[i]))
			}
		}

		dots <- list(...)
		ascii <- if (exists('format', where=dots, inherits=FALSE)) {
			if (tolower(dots$format) == 'asc') { TRUE } else { FALSE }
		} else {
			FALSE
		}

		# save
		for (i in seq_along(rast)) {

			thisRast <- rast[i]
			thisFileName <- filename[i]
			
			if (autoRegion) regionReshape(thisRast)

			nch <- nchar(filename)
			extension <- tolower(substr(thisFileName, nch - 3, nch))

			if (ascii | extension %in% c('.asc', '.asci', '.ascii')) {
				rgrass::execGRASS('r.out.ascii', input=rast, output=thisFileName, flags=flags, intern=TRUE, ...)
			} else {

				thisFlags <- c(flags, 'c')

				if (!exists('createopt', inherits = FALSE)) createopt <- NULL

				# GeoTIFF options
				if (extension == '.tif') {

					# createopt
					createopt <- c(createopt, 'PROFILE=GeoTIFF')
					if (!is.null(compressTiff)) {
						createopt <- c(createopt, paste0('COMPRESS=', toupper(compressTiff)))
					}
					if (bigTiff) createopt <- c(createopt, 'BIGTIFF=YES')
					createopt <- unique(createopt)
					createopt <- paste(createopt, collapse=',')

				}

				# data type
				thisDataType <- if (is.null(datatype)) {
					fasterInfo(thisRast, rastOrVect='raster', temps=TRUE)$gdalDataType
				} else if (datatype == 'INT1U') {
					'Byte'
				} else if (datatype == 'INT2U') {
					'UInt16'
				} else if (datatype == 'INT2S') {
					'Int32'
				} else if (datatype == 'FLT4S') {
					'Float32'
				} else if (datatype == 'FLT8S') {
					'Float64'
				} else {
					datatype
				}

				# save
				success <- rgrass::execGRASS('r.out.gdal', input=thisRast, output=thisFileName, type=thisDataType, createopt=createopt, flags=thisFlags, intern=TRUE, ...)

			}

		} # next raster

		out <- terra::rast(filename[length(filename)])
		if (trimRast) out <- terra::trim(out)
		out <- terra::setMinMax(out)
		invisible(out)
		
	} # if wanting to write a raster

}

#' @name rastFromGrass
#' @title Get raster from 'GRASS'
#' @rdname fasterWriteRaster
#' @export
rastFromGrass <- function(rast) {
	out <- fasterWriteRaster(
		rast,
		filename=paste0(tempfile(), '.tif'),
		overwrite=TRUE,
		trimRast=fasterGetOptions('trimRast', TRUE)
	)
	out
}

