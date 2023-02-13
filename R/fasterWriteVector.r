#' Save one or more spatial vectors to disk directly from a 'GRASS' session
#'
#' This function saves one or more spatial vectors to disk directly from a \code{GRASS} session.\cr
#' By default, files will be of OGC GeoPackage format (extension "\code{.gpkg}"). However, but you can specify the format using the \code{format} argument (see entry for \code{...}). You can see a list of supported formats by simply using this function with no arguments, as in \code{fasterWriteVector()}, or also at \href{https://grass.osgeo.org/grass82/manuals/v.out.ogr.html}{\code{v.out.ogr}}.
#'
#' @inheritParams .sharedArgs_outVectClass
#'
#' @param vect The name(s) or one or more spatial vectors in the active \code{GRASS} session.
#' @param filename Path(s) and file name(s), one per vector named in \code{vect}.
#' @param overwrite If \code{FALSE} (default), do not save over existing file(s).
#' @param format File format. Some common formats include:
#' \itemize{
#'	\item \code{'GPKG'} OGC GeoPackage (default).
#'	\item \code{'CSV'}: Comma-separated value... saves the data table only, not the geometries.
#'	\item \code{'ESRI_Shapefile'}: ESRI shapefile... \code{filename} should not end in an extension.
#'	\item \code{'GeoJSON'}: GeoJSON
#'	\item \code{'KML'}: Keyhole Markup Language (KML)
#'	\item \code{'netCDF'}: NetCDF... \code{filename} should not end in an extension.
#'	\item \code{'XLSX'}: MS Office Open XML spreadsheet
#' }
#' See \href{https://grass.osgeo.org/grass82/manuals/v.out.ogr.html}{\code{v.out.ogr}} for more formats.
#' @param ... Additional arguments to send to \href{https://grass.osgeo.org/grass82/manuals/v.out.ogr.html}{\code{v.out.ogr}}.
#'
#' @return Invisibly returns the last vector saved. Importantly, the function also writes one or more files to disk.
#'
#' @seealso \code{\link{vectFromGrass}} in \pkg{fasterRaster} to import vectors in \code{GRASS} to \code{R}; \code{\link[terra]{writeVector}} in package \pkg{terra}; \code{\link[sf]{st_write}} in package \pkg{sf}; \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/v.out.ogr.html}{\code{v.out.ogr}}
#'
#' @example man/examples/ex_fasterWriteVector.r
#'
#' @export

fasterWriteVector <- function(
	vect,
	filename,
	overwrite = FALSE,
	format = 'GPKG',
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector'),
	...
) {

	### display supported formats
	if (missing(vect)) {
	
		forms <- rgrass::execGRASS('v.out.ogr', flags='l', intern=TRUE)
		forms <- forms[forms != 'Supported formats:']
		forms <- trimws(forms)
		forms <- sort(forms)
		forms <- c('Supported vector file formats:', forms)
		cat(paste(forms, collapse='\n'))
		cat('\n')
		flush.console()
	
	### if wanting to write a vector
	} else {	

		### commons v1
		##############

			### errors?
			inGrass <- fasterExists(vect, rastOrVect='vector', temps=TRUE)
			if (any(!inGrass)) stop('These vectors are not in the current GRASS session:\n ', paste(vect[!inGrass], collapse='\n'))

		###############
		### end commons

		### going to overwrite anything?
		if (!overwrite) {
			for (i in seq_along(vect)) {
				if (file.exists(basename(filename[i]))) stop(paste0('File already exists and "overwrite" is FALSE:\n ', filename[i]))
			}
		}

		### general arguments
		args <- list(...)
		args$cmd <- 'v.out.ogr'
		args$flags <- c('quiet', 's', 'm')
		# args$flags <- c('quiet', 'm')
		args$ignore.stderr <- TRUE
		if (overwrite) args$flags <- c(args$flags, 'overwrite')

		# save
		for (i in seq_along(vect)) {

			thisVect <- vect[i]
			thisFileName <- filename[i]

			thisArgs <- args
			thisArgs$input <- thisVect
			thisArgs$output <- thisFileName

			### save
			do.call(rgrass::execGRASS, thisArgs)
			
		} # next vector

		out <- terra::vect(filename)
		if (outVectClass == 'sf') out <- sf::st_as_sf(out)
		invisible(out)
		
	} # if wanting to write a vector

}

#' @name vectFromGrass
#' @title Get vector from 'GRASS'
#' @rdname fasterWriteVector
#' @export
vectFromGrass <- function(vect) fasterWriteVector(vect, filename=paste0(tempfile(), '.gpkg'), overwrite=TRUE)
