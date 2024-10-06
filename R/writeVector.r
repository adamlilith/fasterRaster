#' Save a GVector to disk
#'
#' @description This function saves a `GVector` to disk directly from a **GRASS** session.
#'
#' By default, files will be of OGC GeoPackage format (extension "`.gpkg`"), but this can be changed with the `format` argument. You can see a list of supported formats by simply using this function with no arguments, as in `writeVector()`, or by consulting the online help page for **GRASS** module [`v.out.ogr`](https://grass.osgeo.org/grass84/manuals/v.out.ogr.html).
#'
#' Note that if the vector has a data table attached and at least one numeric or integer column has an `NA` or `NaN` value, the function will yield a warning like:
#' ```
#' Warning 1: Invalid value type found in record 2 for field column_with_NA_or_NaN. This warning will no longer be emitted.
#' ```
#' Also note that despite the promise, this warning will be displayed again.
#'
#' @param x A `GVector`.
#'
#' @param filename Character: Path and file name.
#'
#' @param overwrite Logical: If `FALSE` (default), do not save over existing files.
#'
#' @param format Character or `NULL`: File format. If `NULL` (default), then the function will attempt to get the format from the file name extension. Partial matching is used and case is ignored. You can see a list of formats using `writeVector()` (no arguments). Some common formats include:
#'	* `"GPKG"`: OGC GeoPackage (extension `.gpkg`).
#'	* `"CSV"`: Comma-separated value... saves the data table only, not the geometries (extension `.csv`).
#'	* `"ESRI Shapefile"`: ESRI shapefile (extension `.shp`).
#'	* `"GeoJSON"`: GeoJSON (extension `GeoJSON`)
#'	* `"KML"`: Keyhole Markup Language (extension `.kml`)
#'	* `"netCDF"`: NetCDF (extension `.ncdf`)
#'	* `"XLSX"`: MS Office Open XML spreadsheet (extension `.xlsx`).
#'
#' @param attachTable Logical: If `TRUE` (default), attach the attribute to table to the vector before saving it. If `FALSE`, the attribute table will not be attached.
#'
#' @param ... Additional arguments to send to **GRASS** module [`v.out.ogr`](https://grass.osgeo.org/grass84/manuals/v.out.ogr.html) in **GRASS**.
#'
#' @returns Invisibly returns a `GRaster` (the input, `x`). Also saves the vector to disk.
#'
#' @seealso [terra::writeVector()], [sf::st_write()], module [`v.out.ogr`](https://grass.osgeo.org/grass84/manuals/v.out.ogr.html) in **GRASS**
#'
#' @example man/examples/ex_writeVector.r
#'
#' @seealso [terra::writeVector()]
#'
#' @aliases writeVector
#' @rdname writeVector
#' @exportMethod writeVector
setMethod(
	"writeVector",
	signature(x = "GVector", filename = "character"),
	function(
		x,
		filename,
		overwrite = FALSE,
		format = NULL,
		attachTable = TRUE,
		...
	) {

	### going to overwrite anything?
	if (!overwrite && file.exists(filename)) stop(paste0("File already exists and `overwrite` is FALSE:\n ", filename))

	.locationRestore(x)

	filename <- normalizePath(filename, mustWork = FALSE)

	if (attachTable & nrow(x) > 0L) {

		table <- as.data.frame(x)
		.vAttachDatabase(x, table = table, replace = TRUE)

	}

	### general arguments
	args <- list(
		cmd = "v.out.ogr",
		input = sources(x),
		output = filename,
		flags = c(.quiet(), "s")
		# flags = c(.quiet(), "s", "c") # "c" ==> save geometries lacking a cat number
	)
	if (overwrite) args$flags <- c(args$flags, "overwrite")

	if (!is.null(format)) {

		formats <- writeVector()
		formats <- formats$extension

		format <- gsub(format, pattern = " ", replacement = "_")
		formats <- gsub(formats, pattern = " ", replacement = "_")

		format <- omnibus::pmatchSafe(format, formats, nmax = 1L)

	} else if (is.null(format)) {
	
		fileExt <- .fileExt(filename)
		fileExt <- tolower(fileExt)
		
		if (fileExt == "shp") {
			format <- "ESRI_Shapefile"
		} else if (fileExt == "gml") {
			format <- "GML"
		} else if (fileExt == "kml") {
			format <- "KML"
			args$type <- "face"
		} else if (fileExt == "ncdf") {
			format <- "netCDF"
		} else if (fileExt == "gpkg") {
			format <- "GPKG"
		} else if (fileExt == "csv") {
			format <- "CSV"
		} else if (fileExt == "pdf") {
			format <- "PDF"
		}

	}
	args$format <- format

	if (is.3d(x) & !is.null(format)) {

		if (format == "ESRI_Shapefile") {

			gtype <- geomtype(x)
			if (gtype == "points") {
				args$lco <- "SHPT=POINTZ"
			} else if (gtype == "lines") {
				args$lco <- "SHPT=ARCZ"
			} else if (gtype == "polygons") {
				args$lco <- "SHPT=POLYGONZ"
			}
		
		} else if (format == "KML") {
			args$dsco = "AltitudeMode=absolute"
		}
	
	}

	do.call(rgrass::execGRASS, args)
	
	# out <- suppressWarnings(terra::vect(filename))
	# if (nrow(x) > 0L && any(names(out) == "cat")) out$cat <- NULL
	# if (nrow(x) > 0L && any(names(out) == "cat_")) out$cat_ <- NULL

	# invisible(out)
	invisible(x)

	} # EOF
)

#' @aliases writeVector
#' @rdname writeVector
#' @exportMethod writeVector
setMethod(
	"writeVector",
	signature(x = "missing", filename = "missing"),
	function(x, filename) {

		forms <- rgrass::execGRASS("v.out.ogr", flags = "l", intern = TRUE, ignore.stderr = TRUE)

		forms <- forms[forms != "Supported formats:"]
		forms <- trimws(forms)
		forms <- sort(forms)
		forms <- strsplit(forms, ": ")
		forms <- do.call(rbind, forms)
		forms <- as.data.frame(forms)
		names(forms) <- c("extension", "filetype")
		forms$extension <- gsub(forms$extension, pattern = " \\(rw\\+\\)", replacement = "")
		forms$extension <- gsub(forms$extension, pattern = " \\(rw\\)", replacement = "")
		
		forms
		
	} # EOF
)
