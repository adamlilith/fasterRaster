#' Save one or more vectors to disk directly from a GRASS session
#'
#' This function saves a spatial vector to disk directly from a \code{GRASS} session. If you want to save a vector from \code{R}, use \code{\link{save}} or \code{\link{saveRDS}}.
#'
#' @param vect The name(s) or one or more spatial vectors in the active \code{GRASS} session.
#' @param filename Path(s) and file name(s). If \code{vect} has more than one name, then you should state multiple file names, one per vector. The function will attempt to ascertain the type of the vector file from the filename extension, but you can also set it using \code{format} (see the \code{...} argument).
#' @param overwrite If \code{FALSE} (default), do not save over existing file(s).
#' @param ... Arguments to send to \href{https://grass.osgeo.org/grass82/manuals/v.out.ogr.html}{v.out.ogr}. This \code{GRASS} modole includes a \code{format} argument, which explicitly states the file format. Some common formats include:
#' \itemize{
#' }
#' For the full list, please see \href{https://grass.osgeo.org/grass82/manuals/v.out.ogr.html}{v.out.ogr}. When using other formats, you may have to specify the \code{createopts} argument, too. \cr
#'
#' @return Nothing. Writes one or more files to disk.
#'
#' @seealso \code{\link{importFromGrass}} in \pkg{fasterRaster}; \code{\link[terra]{writeVector}} in package \pkg{terra}; \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/v.out.ogr.html}{\code{v.out.ogr}}
#'
#' @example man/examples/ex_fasterWriteVector.r
#'
#' @export

fasterWriteVector <- function(
	vect,
	filename,
	overwrite = FALSE,
	...
) {

	flags <- c('quiet')
	if (overwrite) flags <- c(flags, 'overwrite')

	# going to overwrite anything?
	if (!overwrite) {
		for (i in seq_along(vect)) {
			if (file.exists(basename(filename[i]))) stop(paste0('File already exists and "overwrite" is FALSE:\n ', filename[i]))
		}
	}

	# save
	for (i in seq_along(vect)) {

		thisVect <- vect[i]
		thisFileName <- filename[i]

		nch <- nchar(filename)
		extension <- tolower(substr(thisFileName, nch - 3, nch))
say(TBD)
		if (ascii | extension %in% c('.asc', '.asci', '.ascii')) {
			rgrass::execGRASS('thisVect.out.ascii', input=vect, output=thisFileName, flags=flags, ...)
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
			gdalDataType <- fasterInfo(thisVect)$gdalDataType

			thisDataType <- if (is.null(datatype)) {
				gdalDataType
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

			# trim
			if (trim) regionResize(thisVect, rastOrVect='vector')

			# save
			rgrass::execGRASS('r.out.ogr', input=thisVect, output=thisFileName, type=thisDataType, createopt=createopt, flags=thisFlags, ...)

			# restore region
			if (trim) success <- .revertRegion()

		}

	}

	out <- terra::vect(filename)
	invisible(out)

}


