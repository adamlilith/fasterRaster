#' Save one or more 'GRaster's to disk
#'
#' This function saves a raster to disk directly from a **GRASS** session. If the `GRaster` was produced in `GRASS` (i.e., not created using `terra` or `stars` then converted to a `GRaster`), and you convert it to a `SpatRaster` or `stars` raster, please consult [writeRaster4()] for important details!
#'
#' The file type will be attempted to be ascertained from the file extension, but you can specify the format using the `format` argument (see entry for `...`). You can see a list of supported formats by simply using this function with no arguments, as in `writeRaster()`, or also at [https://grass.osgeo.org/grass82/manuals/r.out.gdal.html](r.out.gdal). Only the `GeoTIFF` file format is guaranteed to work for multi-layered rasters.
#'
#' @param x A `GRaster` or missing.
#' @param filename Path and file name.
#' @param overwrite If `FALSE` (default), do not save over existing file(s).
#' @param ... Additional arguments. These can include:
#' * `datatype` The datatype of the values stored in non-ASCII rasters. If `NULL`, this will be ascertained from the raster . This can any of:
#'    | **`terra`** | **`GRASS`** | **`GDAL`** | Values |
#'    | `INT1U`     | `CELL`      | `Byte`     | Integer values from 0 to 255 |
#'    | `INT2U`     | `CELL`      | `UInt16`   | Integer values from 0 to 65,534 |
#'    | `INT2S`     | `CELL`     | `Int16`    | Integer values from -32,767 to -32,767 |
#'    | `INT4S`     | `CELL`     | `Int32`    | Integer values from -2,147,483,647 to 2,147,483,647 |
#'    | `FLT4S`     | `FCELL`   | `Float32`    | Values from -3.4e+38 to 3.4e+38, including decimal values |
#'    | `FLT8S`     | `DCELL`   | `Float64`    | Values from -1.7e+308 to 1.7e+308, including decimal values |
#' * `compressTiff`: Character or `NULL`: Type of compression for GeoTIFF files. The default is `'DEFLATE'`, but can be any of `'LZW'`, `'PACKBITS'`, `'LZMA'`, or `NULL`. If `NULL`, then no compression is used, but the file can still be reduced in size by using zip, gzip, or other compressions.
#' * `bigTiff`: Logical: If `TRUE`, and the file format is a GeoTIFF and would be larger than 4 GB (regardless of compression), then the file will be saved in BIGTIFF format.
#' * `format`: Character, indicating file format. This is usually ascertained from the file extension, but in case this fails, it can be stated explicitly. When using other formats, you may have to specify the `createopts` argument, too (see [https://grass.osgeo.org/grass82/manuals/r.out.gdal.html](r.out.gdal)). Some common formats include:
#'    * `'GTiff'` (default): GeoTIFF `filename` ends in `.tif`
#'    * `'ASC'`: ASCII `filename` ends in `.asc`
#' * Additional arguments to send to [https://grass.osgeo.org/grass82/manuals/r.out.gdal.html](r.out.gdal) and [https://grass.osgeo.org/grass82/manuals/r.out.ascii.html](r.out.ascii).
#' * `precision`: Numeric: For ASCII files, you may need to state the number of significant digits. 32-bit values have 7 digits and 64-bit values have 16. So in these cases the argument would be `precision=7` or `precision=16`.
#'
#' @return A `GRaster` or a `stars` raster. A raster is also saved to disk.
#'
#' @seealso [writeRaster4()] and [writeRaster8()] for saving a raster created in **GRASS** then imported to **R** using [rast()]; [terra::writeRaster()]; **GRASS** modules [https://grass.osgeo.org/grass82/manuals/r.out.gdal.html](r.out.gdal) and [https://grass.osgeo.org/grass82/manuals/r.out.ascii.html](r.out.ascii).
#'
#' @example man/examples/ex_writeRaster.r
#'
#' @export

if (!isGeneric('writeRaster')) setGeneric('writeRaster', function(x, ...) standardGeneric('writeRaster'))

setMethod(
	'writeRaster',
	signature(x = 'missing'),
	function(x) {
	
	forms <- rgrass::execGRASS('r.out.gdal', flags='l', intern=TRUE)
	forms <- forms[forms != 'Supported formats:']
	forms <- trimws(forms)
	forms <- sort(forms)
	forms <- c('Supported raster file formats:', forms)
	cat(paste(forms, collapse='\n'))
	cat('\n')
	flush.console()
	
	} # EOF
)


setMethod(
	'writeRaster',
	signature(x = 'missing'),
	function(
		x,
		filename,
		overwrite = FALSE,
		...
	) {
	
	### commons
	###########
	
	.restore(x)
	regionReshape(x)

	### end commons
	###############

	dots <- list(...)

	flags <- c('quiet')
	if (overwrite) flags <- c(flags, 'overwrite')

	### going to overwrite anything?
	if (!overwrite) {
		if (file.exists(x)) stop('File already exists and "overwrite" is FALSE:\n  ', filename)
	}

	### format
	nch <- nchar(filename)
	extension3 <- tolower(substr(filename, nch - 3, nch))
	extension4 <- tolower(substr(filename, nch - 4, nch))
	extension5 <- tolower(substr(filename, nch - 5, nch))
	
	ascii <- if ('format' %in% names(dots)) {
		if (('format' %in% names(dots) && tolower(dots$format) == 'asc') | extension3 == '.asc' | estension4 == '.asci' | extension5 == '.ascii') { TRUE } else { FALSE }
	} else {
		FALSE
	}

	geotiff <- if (exists('format', where=dots, inherits=FALSE)) {
		if (('format' %in% names(dots) && tolower(dots$format) == 'gtiff') | extension3 == '.tif') { TRUE } else { FALSE }
	} else {
		FALSE
	}

	numLayers <- nlyr(x)

	### save
	if (ascii) {
		if (nlyr(x) > 1L) stop('Cannot save multi-layer GRaster as a single ASCII file. Save each layer individually.')
		rgrass::execGRASS('r.out.ascii', input=x, output=filename, flags=flags, intern=TRUE, ...)
	} else {

		thisFlags <- c(flags, 'c')

		## if multi-layered raster stack, then group first... only guaranteed to work with GeoTIFFs
		if (numLayers > 1L) {
			groupName <- .makeGName(rastOrVect='group')
			input <- .gname(x)
			rgrass::execGRASS('i.group', group=groupName, input=input, intern=TRUE)
			gname <- groupName
		} else {
			gname <- .gname(x)
		}
		
		if (!('createopt' %in% names(dots))) createopt <- NULL

		# GeoTIFF options
		if (geotiff) {

			# createopt
			createopt <- c(createopt, 'PROFILE=GeoTIFF')
			if ('compressTiff' %in% names(dots) && dots$compressTiff) createopt <- c(createopt, paste0('COMPRESS=', toupper(compressTiff)))
			if ('bigTiff' %in% names(dots) && bigTiff) createopt <- c(createopt, 'BIGTIFF=YES')
			createopt <- unique(createopt)
			createopt <- paste(createopt, collapse=',')

		}

		# data type
		thisDataType <- if (!('datatype' %in% names(dots))) {
			datatype(x, type='GDAL')
		} else if (dots$datatype == 'INT1U') {
			'Byte'
		} else if (dots$datatype == 'INT2U') {
			'UInt16'
		} else if (dots$datatype == 'INT2S') {
			'Int32'
		} else if (dots$datatype == 'FLT4S') {
			'Float32'
		} else if (dots$datatype == 'FLT8S') {
			'Float64'
		} else {
			datatype
		}

		# save
		success <- rgrass::execGRASS('r.out.gdal', input=gname, output=filename, type=thisDataType, createopt=createopt, flags=thisFlags, intern=TRUE, ...)

	}

	out <- terra::rast(filename)
	out <- terra::trim(out)
	# out <- terra::setMinMax(out)
	invisible(out)
	
	} # EOF
	
)
