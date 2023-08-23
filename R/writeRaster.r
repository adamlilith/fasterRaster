#' Save a GRaster to disk
#'
#' @description
#' This function saves a `GRaster` to disk directly from a **GRASS** session. It is faster than using [rast()], then saving the output of that to disk (because `rast()` actually save the raster to disk, anyway).
#'
#' The function will attempt to ascertain the file type to be ascertained from the file extension, but you can specify the format using the `format` argument (see entry for `...`). You can see a list of supported formats by simply using this function with no arguments, as in `writeRaster()`, or by consulting the online help page for the **GRASS** module `r.out.gdal`. Only the `GeoTIFF` file format is guaranteed to work for multi-layered rasters.
#'
#' **Note**: Rasters will be trimmed to their smallest extent such that no rows or columns are all `NA`s. You can add `NA` rows and columns to a `SpatRaster` using [terra::extend()].
#'
#' @param x A `GRaster` or missing: If missing, a table of supported file types is reported.
#' @param filename Character: Path and file name.
#' @param overwrite Logical: If `FALSE` (default), do not save over existing file(s).
#' @param ... Additional arguments. These can include:
#' * `datatype` The datatype of the values stored in non-ASCII rasters. If `NULL`, this will be ascertained from the raster . This can any of:
#'    | **`terra`** | **`GRASS`** | **`GDAL`** | **Values** |
#'    | ----------- | ----------- | ---------- | ------ |
#'    | `INT1U`     | `CELL`      | `Byte`     | Integer values from 0 to 255 |
#'    | `INT2U`     | `CELL`      | `UInt16`   | Integer values from 0 to 65,534 |
#'    | `INT2S`     | `CELL`     | `Int16`    | Integer values from -32,767 to -32,767 |
#'    | `INT4S`     | `CELL`     | `Int32`    | Integer values from -2,147,483,647 to 2,147,483,647 |
#'    | `FLT4S`     | `FCELL`   | `Float32`    | Values from -3.4e+38 to 3.4e+38, including decimal values |
#'    | `FLT8S`     | `DCELL`   | `Float64`    | Values from -1.7e+308 to 1.7e+308, including decimal values |
#' * `compressTiff`: Character or `NULL`: Type of compression for GeoTIFF files:
#'    * `"DEFLATE"` (default)
#'    * `"LZW"`
#'    * `"PACKBITS"`
#'    * `"LZMA"`
#'    * `NULL`: No compression is used, but the file can still be reduced in size by using zip, gzip, or other compressions.
#' * `bigTiff`: Logical: If `TRUE`, and the file format is a GeoTIFF and would be larger than 4 GB (regardless of compression), then the file will be saved in BIGTIFF format.
#' * `format`: Character, indicating file format. This is usually ascertained from the file extension, but in case this fails, it can be stated explicitly. When using other formats, you may have to specify the `createopts` argument, too (see help page for **GRASS** module `r.out.gdal`). Two common formats include:
#'    * `"GTiff"` (default): GeoTIFF `filename` ends in `.tif`
#'    * `"ASC"`: ASCII `filename` ends in `.asc`
#' * Additional arguments to send to **GRASS** modules `r.out.gdal` and `r.out.ascii`.
#' * `precision`: Numeric: For ASCII files, you may need to state the number of significant digits. 32-bit values have 7 digits and 64-bit values have 16. So in these cases the argument would be `precision=7` or `precision=16`.
#'
#' @return A `GRaster` or a `stars` raster. A raster is also saved to disk.
#'
#' @seealso [terra::writeRaster()]
#'
#' @example man/examples/ex_writeRaster.r
#'
#' @aliases writeRaster
#' @rdname writeRaster
#' @export
#' @exportMethod writeRaster
setMethod(
	"writeRaster",
	signature(x = "GRaster", filename = "character"),
	function(
		x,
		filename,
		overwrite = FALSE,
		...
	) {
	
	### commons
	###########
	
	.restore(x)
	region(x)

	### end commons
	###############

	dots <- list(...)

	flags <- c("quiet")
	if (overwrite) flags <- c(flags, "overwrite")

	### going to overwrite anything?
	if (!overwrite) {
		if (file.exists(filename)) stop("File already exists and ", sQuote("overwrite"), " is FALSE:\n  ", filename)
	}

	### format
	nch <- nchar(filename)
	extension3 <- tolower(substr(filename, nch - 3, nch))
	extension4 <- tolower(substr(filename, nch - 4, nch))
	extension5 <- tolower(substr(filename, nch - 5, nch))
	
	ascii <- if ("format" %in% names(dots)) {
		if (("format" %in% names(dots) && tolower(dots$format) == "asc") | extension3 == ".asc" | extension4 == ".asci" | extension5 == ".ascii") { TRUE } else { FALSE }
	} else {
		FALSE
	}

	geotiff <- (("format" %in% names(dots) && tolower(dots$format) == "gtiff") | extension3 == ".tif")

	nLayers <- nlyr(x)

	### save
	if (ascii) {
		if (nlyr(x) > 1L) stop("Cannot save multi-layer GRaster as a single ASCII file. Save each layer individually.")
		rgrass::execGRASS("r.out.ascii", input=x, output=filename, flags=flags, intern=TRUE, ...)
	} else {

		thisFlags <- c(flags, "c")

		## if multi-layered raster stack, then group first... only guaranteed to work with GeoTIFFs
		if (nLayers > 1L) {

			groupName <- .makeGName(rastOrVect="group")
			input <- .gnames(x)
			
			args <- list(
				cmd = "i.group",
				group = groupName,
				input = input,
				flags = "quiet",
				intern = TRUE
			)

			do.call(rgrass::execGRASS, args=args)
			gn <- groupName

		} else {
			gn <- .gnames(x)
		}
		
		# data type
		thisDataType <- if (!("datatype" %in% names(dots))) {
			datatype(x, type = "GDAL")
		} else if (dots$datatype == "INT1U") {
			"Byte"
		} else if (dots$datatype == "INT2U") {
			"UInt16"
		} else if (dots$datatype == "INT2S") {
			"Int32"
		} else if (dots$datatype == "FLT4S") {
			"Float32"
		} else if (dots$datatype == "FLT8S") {
			"Float64"
		} else {
			datatype
		}
		
		if (length(thisDataType) > 1L) {
			thisDataType <- if (any(thisDataType == "Float64")) {
				"Float64"
			} else if (any(thisDataType == "Float32")) {
				"Float32"
			} else if (any(thisDataType == "Int32")) {
				"Int32"
			} else if (any(thisDataType == "UInt16")) {
				"UInt16"
			} else if (any(thisDataType == "Byte")) {
				"Byte"
			}
		}

		if (!("createopt" %in% names(dots))) createopt <- NULL

		# GeoTIFF options
		if (geotiff) {

			# createopt
			createopt <- c(createopt, "PROFILE=GeoTIFF")
			if ("compressTiff" %in% names(dots) && dots$compressTiff) createopt <- c(createopt, paste0("COMPRESS=", toupper(dots$compressTiff)))
			# if ("bigTiff" %in% names(dots) && bigTiff) createopt <- c(createopt, "BIGTIFF=YES")
			createopt <- c(createopt, "BIGTIFF=IF_NEEDED")
			if (thisDataType %in% c("Byte", "UInt16", "Int32")) createopt <- c(createopt, "PREDICTOR=2")
			if (thisDataType %in% c("Float32", "Float64")) createopt <- c(createopt, "PREDICTOR=3")
			
			createopt <- unique(createopt)
			createopt <- paste(createopt, collapse=",")

			# mm <- minmax(x)
			# metaopt <- paste0("TIFFTAG_MINSAMPLEVALUE=", paste(mm[1L, ], collapse=" "))
			# metaopt <- c(metaopt, paste0("TIFFTAG_MAXSAMPLEVALUE=", paste(mm[2L, ], collapse=" ")))
			# metaopt <- paste0("STATISTICS_MINIMUM=", paste(mm[1L, ], collapse=" "))
			# metaopt <- c(metaopt, paste0("STATISTICS_MAXIMUM=", paste(mm[2L, ], collapse=" ")))
			# metaopt <- unique(metaopt)
			# metaopt <- paste(metaopt, collapse=",")
			
		}

		# save
		# rgrass::execGRASS("r.out.gdal", input=gnames, output=filename, type=thisDataType, createopt=createopt, metaopt=metaopt, flags=thisFlags, intern=TRUE, ...)
		args <- list(
			cmd = "r.out.gdal",
			input = gn,
			output = filename,
			type = thisDataType,
			createopt = createopt,
			flags = thisFlags,
			intern = TRUE
		)

		do.call(rgrass::execGRASS, args=args)

	}

	out <- terra::rast(filename)
	out <- terra::trim(out)
	names(out) <- names(x)
	invisible(out)
	
	} # EOF
	
)

#' @aliases writeRaster
#' @rdname writeRaster
#' @export
#' @exportMethod writeRaster
setMethod(
	"writeRaster",
	signature(x = "missing", filename = "missing"),
	function(x, filename) {
	
	forms <- rgrass::execGRASS("r.out.gdal", flags="l", intern=TRUE)
	forms <- forms[forms != "Supported formats:"]
	forms <- trimws(forms)
	forms <- sort(forms)
	forms <- c("Supported raster file formats:", forms)
	cat(paste(forms, collapse="\n"))
	cat("\n")
	utils::flush.console()
	
	} # EOF
)
