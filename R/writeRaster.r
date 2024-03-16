#' Save a GRaster to disk
#'
#' @description
#' This function saves a `GRaster` to disk directly from a **GRASS** session. It is faster than using [rast()], then saving the output of that to disk (because `rast()` actually save the raster to disk, anyway).
#'
#' The function will attempt to ascertain the file type to be ascertained from the file extension, but you can specify the format using the `format` argument (see entry for `...`). You can see a list of supported formats by simply using this function with no arguments, as in `writeRaster()`, or by consulting the online help page for the **GRASS** module [`r.out.gdal`](https://grass.osgeo.org/grass84/manuals/r.out.gdal.html). Only the `GeoTIFF` file format is guaranteed to work for multi-layered rasters.
#'
#' The function will attempt to optimize the `datatype` argument, but this can take a long time. You can speed this up by setting `datatype` manually. Note that if you are saving a "stack" of `GRaster`s with different `datatype`s, the one with the highest information density will be used (e.g., low-bit integer < high-bit integer < floating-point < double-floating point). This can make rasters with lower datatypes much larger on disk. In these cases, it make be best to save rasters with similar `datatype`s together.
#'
#' @param x A `GRaster` or missing: If missing, a table of supported file types is reported.
#' @param filename Character: Path and file name.
#' @param overwrite Logical: If `FALSE` (default), do not save over existing file(s).
#' @param datatype `NULL` (default) or character: The datatype of the values stored in non-ASCII rasters. If `NULL`, this will be ascertained from the raster. This can any of:
#'
#'    | **`fasterRaster`** | **`terra`** | **`GRASS`** | **`GDAL`** | **Values** |
#'    | ------------------ | ----------- | ----------- | ---------- | ------ |
#'    | `integer`          | `INT1U`     | `CELL`      | `Byte`     | Integer values from 0 to 255 |
#'    | `integer`          | `INT2U`     | `CELL`      | `UInt16`   | Integer values from 0 to 65,534 |
#'    | `integer`          | `INT2S`     | `CELL`      | `Int16`    | Integer values from -32,767 to -32,767 |
#'    | `integer`          | na          | `CELL`      | `Int32`    | Integer values from 0 to 4,294,967,295 |
#'    | `integer`          | `INT4S`     | `CELL`      | `Int32`    | Integer values from -2,147,483,647 to 2,147,483,647 |
#'    | `float`            | `FLT4S`     | `FCELL`     | `Float32`  | Values from -3.4e+38 to 3.4e+38, including decimal values |
#'    | `double`           | `FLT8S`     | `DCELL`     | `Float64`  | Values from -1.79e+308 to 1.79e+308, including decimal values |
#'    | `factor`           | `INT`*      | `CELL`      | `INT*`     | Integer values corresponding to categories
#'
#' `*` Depends on the integers (signed/unsigned, range of values). Categorical rasters will have a CSV file with category values and labels saved with them. The file name will be the same as the raster's file name, but end in extension ".csv".
#'
#' @param warn Logical: If `TRUE` (default), display a warning if the `datatype` argument does not match the value given by `datatype(x, "GDAL")`.
#'
#' @param ... Additional arguments. These can include:
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
#' @return A `SpatRaster` (invisibly). A raster is also saved to disk.
#'
#' @seealso [terra::writeRaster()], module [`r.out.gdal`](https://grass.osgeo.org/grass84/manuals/r.out.gdal.html) in **GRASS**
#'
#' @example man/examples/ex_writeRaster.r
#'
#' @importFrom tools file_ext
#'
#' @aliases writeRaster
#' @rdname writeRaster
#' @exportMethod writeRaster
setMethod(
	"writeRaster",
	signature(x = "GRaster", filename = "character"),
	function(
		x,
		filename,
		overwrite = FALSE,
		datatype = NULL,
		warn = TRUE,
		...
	) {
	
	.locationRestore(x)
	.region(x)

	dots <- list(...)
	filename <- trimws(filename)

	flags <- c(.quiet())
	if (overwrite) flags <- c(flags, "overwrite")

	### going to overwrite anything?
	if (!overwrite) {
		if (file.exists(filename)) stop("File already exists and ", sQuote("overwrite"), " is FALSE:\n  ", filename)
	}

	### format
	extension <- .fileExt(filename)
	extension <- tolower(extension)
	
	ascii <- if ("format" %in% names(dots)) {
		if (("format" %in% names(dots) && tolower(dots$format) %in% c("asc", "asci", "ascii")) || extension %in% c("asc", "asci", "ascii")) { TRUE } else { FALSE }
	} else {
		FALSE
	}

	geotiff <- ("format" %in% names(dots) && tolower(dots$format) == "gtiff") | extension == "tif"

	nLayers <- nlyr(x)

	### save
	if (ascii) {
		
		if (nlyr(x) > 1L) stop("Cannot save multi-layer GRaster as a single ASCII file. Save each layer individually.")
		
		rgrass::execGRASS(
			dmc = "r.out.ascii",
			input = x,
			output = filename,
			flags = flags,
			...
		)

	} else {

		thisFlags <- c(flags, "c")

		## if multi-layered raster stack, then group first... only guaranteed to work with GeoTIFFs
		if (nLayers > 1L) {

			srcGroup <- .makeSourceName("i_group", type = "group")
			input <- sources(x)
			
			rgrass::execGRASS(
				cmd = "i.group",
				group = srcGroup,
				input = input,
				flags = .quiet()
			)

			src <- srcGroup

		} else {
			src <- sources(x)
		}
		
		# data type
		if (is.null(datatype)) datatype <- datatype(x, "GDAL")
		
		# if (any(datatype == "INT1U")) datatype[datatype == "INT1U"] <- "Byte" # will not write, for some reason
		if (any(datatype == "INT1U")) datatype[datatype == "INT1U"] <- "UInt16"
		if (any(datatype == "INT2U")) datatype[datatype == "INT2U"] <- "UInt16"
		if (any(datatype == "INT2S")) datatype[datatype == "INT2S"] <- "Int32"
		if (any(datatype == "FLT4S")) datatype[datatype == "FLT4S"] <- "Float32"
		if (any(datatype == "FLT8S")) datatype[datatype == "FLT8S"] <- "Float64"
		
		if (length(datatype) > 1L) {
			datatype <- if (any(datatype == "Float64")) {
				"Float64"
			} else if (any(datatype == "Float32")) {
				"Float32"
			} else if (any(datatype == "Int32")) {
				"Int32"
			} else if (any(datatype == "UInt16")) {
				"UInt16"
			} else if (any(datatype == "Byte")) {
				# "Byte"
				"UInt16"
			}
			
		}

		# if (any(!(datatype(x, "GDAL") %in% datatype))) {
		# 	flags <- c(flags, "f")
		# 	if (warn) warning("Argument ", sQuote("datatype"), " does not match the data type of the raster. Data may be lost.")
		# }

		if (!("createopt" %in% names(dots))) createopt <- NULL

		# GeoTIFF options
		if (geotiff) {

			# createopt
			createopt <- c(createopt, "PROFILE=GeoTIFF")
			if ("compressTiff" %in% names(dots) && dots$compressTiff) createopt <- c(createopt, paste0("COMPRESS=", toupper(dots$compressTiff)))
			# if ("bigTiff" %in% names(dots) && bigTiff) createopt <- c(createopt, "BIGTIFF=YES")
			createopt <- c(createopt, "BIGTIFF=IF_NEEDED")
			if (datatype %in% c("Byte", "UInt16", "Int32")) createopt <- c(createopt, "PREDICTOR=2")
			if (datatype %in% c("Float32", "Float64")) createopt <- c(createopt, "PREDICTOR=3")
			
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
		rgrass::execGRASS(
			cmd = "r.out.gdal",
			input = src,
			output = filename,
			type = datatype,
			format = "GTiff",
			createopt = createopt,
			flags = thisFlags
		)

	}
	isFact <- is.factor(x)
	if (any(isFact)) {

		extension <- paste0(".", extension)
		levelFileName <- substr(filename, 1L, nchar(filename) - nchar(extension))
		levelFileName <- paste0(levelFileName, ".csv")
		utils::write.csv(cats(x), levelFileName, row.names = FALSE)

	}

	out <- terra::rast(filename)
	names(out) <- names(x)
	
	if (any(isFact)) {
		for (i in which(isFact)) {
			out[[i]] <- terra::categories(out[[i]], layer = i, value = cats(x)[[i]], active = activeCat(x)[i])
		}
	}
	
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
	
	forms <- rgrass::execGRASS(
		cmd = "r.out.gdal",
		flags = "l",
		intern = TRUE
	)
	
	forms <- forms[forms != "Supported formats:"]
	forms <- trimws(forms)
	forms <- sort(forms)
	formsHeader <- c("Supported raster file formats:", forms)
	cat(paste(formsHeader, collapse = "\n"))
	cat("\n")
	utils::flush.console()
	invisible(forms)
	
	} # EOF
)
