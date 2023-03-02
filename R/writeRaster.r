#' Save one or more 'GRaster's to disk
#'
#' This function saves a raster to disk directly from a **GRASS** session. If the `GRaster` was produced in `GRASS` (i.e., not created using `terra` or `stars`), and you convert it to a `SpatRaster` or `stars` raster, please consult [writeRaster4()] for important details!
#'
#' The file type will be attempted to be ascertained from the file extension, but you can specify the format using the `format` argument (see entry for `...`). You can see a list of supported formats by simply using this function with no arguments, as in `writeRaster()`, or also at [https://grass.osgeo.org/grass82/manuals/r.out.gdal.html](r.out.gdal).
#'
#' @param x A `GRaster`.
#' @param filename Path and file name.
#' @param overwrite If `FALSE` (default), do not save over existing file(s).
#' @param datatype The datatype of the values stored in non-ASCII rasters. If `NULL`, this will be ascertained from the raster . This can any of:
#'	* `'INT1U'` or `'Byte'`: Integer values from 0 to 255
#'	* `'INT2U'` or `'UInt16'`: Integer values from 0 to 65,534
#'	* `'INT2S'` or `'Int16'`: Integer values from -32,767 to -32,767
#'	* `'INT4S'` or `'Int32'`: Integer values from -2,147,483,647 to 2,147,483,647
#'	* `'FLT4S'` or `'Float32'`: Values from -3.4e+38 to 3.4e+38, including decimal values
#'	* `'FLT8S'` or `'Float64'`: Values from -1.7e+308 to 1.7e+308, including decimal values
#'
#' @param compressTiff Character or `NULL`: Type of compression for GeoTIFF files. The default is `'DEFLATE'`, but can be any of `'LZW'`, `'PACKBITS'`, `'LZMA'`, or `NULL`. If `NULL`, then no compression is used, but the file can still be reduced in size by using zip, gzip, or other compressions.
#' @param bigTiff If `TRUE`, and the file format is a GeoTIFF and would be larger than 4 GB (regardless of compression), then the file will be saved in BIGTIFF format.
#' @param ... Arguments to send to [https://grass.osgeo.org/grass82/manuals/r.out.gdal.html](r.out.gdal) and [https://grass.osgeo.org/grass82/manuals/r.out.ascii.html](r.out.ascii). These include a `format` argument, which explicitly states the file format. Some common formats include:
#' 	* `'GTiff'` (default): GeoTIFF `filename` ends in `.tif`
#' 	* `'ASC'`: ASCII `filename` ends in `.asc`
#' For the full list, please see [https://grass.osgeo.org/grass82/manuals/r.out.gdal.html](r.out.gdal) and [https://grass.osgeo.org/grass82/manuals/r.out.ascii.html](r.out.ascii). When using other formats, you may have to specify the `createopts` argument, too.
#'
#' For ASCII files, you may also need to state the number of significant digits saved using a `precision` argument. 32-bit values have 7 digits and 64-bit values have 16. So in these cases the argument would be `precision=7` or `precision=16`.
#'
#' @return A `SpatRaster` or a `stars` raster.
#'
#' @seealso [writeRaster4()] and [writeRaster8()] for saving a raster created in **GRASS** then imported to **R** using [rast()]; [terra::writeRaster()]; **GRASS** modules [https://grass.osgeo.org/grass82/manuals/r.out.gdal.html](r.out.gdal) and [https://grass.osgeo.org/grass82/manuals/r.out.ascii.html](r.out.ascii).
#'
#' @example man/examples/ex_writeRaster.r
#'
#' @export

writeRaster <- function(
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

