#' Save a GRaster to disk
#'
#' @description
#' This function saves a `GRaster` to disk directly from a **GRASS** session. It is faster than using [rast()], then saving the output of that to disk (because `rast()` actually save the raster to disk, anyway).
#'
#' The function will attempt to ascertain the file type to be ascertained from the file extension, but you can specify the format using the `format` argument (see entry for `...`). You can see a list of supported formats by simply using this function with no arguments, as in `writeRaster()`, or by consulting the online help page for the **GRASS** module `r.out.gdal` (see `grassHelp("r.out.gdal")`). Only the `GeoTIFF` file format is guaranteed to work for multi-layered rasters.
#'
#' The function will attempt to optimize the `datatype` argument, but this can take a long time. You can speed this up by setting `datatype` manually. Note that if you are saving a "stack" of `GRaster`s with different `datatype`s, the one with the highest information density will be used (e.g., low-bit integer < high-bit integer < floating-point < double-floating point). This can make rasters with lower datatypes much larger on disk. In these cases, it make be best to save rasters with similar `datatype`s together.
#'
#' @param x A `GRaster` or missing: If missing, a table of supported file types is reported.
#' @param filename Character: Path and file name.
#' @param overwrite Logical: If `FALSE` (default), do not save over existing file(s).
#' @param datatype `NULL` (default) or character: The datatype of the values stored in non-ASCII rasters. If `NULL`, this will be ascertained from the raster, and the function usually does a good job at it. However, you can force it manually, but note that in some cases, trying to save a `GRaster` using an inappropriate `datatype` for its values can result in an error or in the function exiting without an error but also without having written the raster to disk. The argument can take any of those shown below under the first four columns, but whatever is used, it will be converted to the **GDAL** version.
#'
#'    | **fasterRaster**   | **terra**   | **GRASS**   | **GDAL**   | **Values** |
#'    | ------------------ | ----------- | ----------- | ---------- | ------ |
#'    | `integer`          | `INT1U`     | `CELL`      | `Byte`     | Integer values from 0 to 255 |
#'    | `integer`          | `INT2U`     | `CELL`      | `UInt16`   | Integer values from 0 to 65,534 |
#'    | `integer`          | `INT2S`     | `CELL`      | `Int16`    | Integer values from -32,767 to -32,767 |
#'    | `integer`          | `INT4S`     | `CELL`      | `Int32`    | Integer values from -2,147,483,647 to 2,147,483,647 |
#'    | `float`            | `FLT4S`     | `FCELL`     | `Float32`  | Values from -3.4E+38 to 3.4E+38, including decimal values |
#'    | `double`           | `FLT8S`     | `DCELL`     | `Float64`  | Values from -1.79E+308 to 1.79E+308, including decimal values |
#'    | `factor`           | `INT`*      | `CELL`      | `INT*`     | Integer values corresponding to categories
#'
#' `*` Depends on the integers (signed/unsigned, range of values). Categorical rasters will have an associated file saved with them that has category values and labels. The file name will be the same as the raster's file name, but end with the extension given by `levelsExt` (`.csv` by default).
#'
#' @param byLayer Logical: If `FALSE` (default), multi-layer rasters will be saved in one file. If `TRUE`, the each layer will be saved in a separate file. The filename from `filename` will be amended so that it ends with `_<name>` (then the file extension), where `<name>` is give by [names()]. Note that if any characters in raster names will not work in a file name, then the function will fail (e.g., a backslash or question mark).
#'
#' @param names Logical: If `TRUE` (default), save a file with raster layer names. The file will have the same name as the raster file but end with "`_names.csv`". Currently, the [names()] attribute of rasters cannot be saved in the raster, which can create confusion when multi-layered rasters are saved. Turning on this option will save the ancillary file with layer names. If it exists, this file will be read by [fast()] so layer names are assigned when the raster is read by that function. The absence of a "names" file will not create any issues with this function or [fast()], other than not having the metadata on layer names.
#' 
#' @param levelsExt Character, logical, or `NULL` (default): Name of the file extension for the "levels" file that accompanies a categorical `GRaster`. When saving categorical rasters, the raster file is accompanied with a "levels" file that contain information on the levels of the raster. This file is the same as `filename`, except it has a different extension. Valid values depend on how many raster layers are saved at a time (case is ignored):
#' * DefaultOne raster layer: `".csv"`
#' * Two or more layers, with at least one categorical raster: `".rds"`, `".rda"`, `".rdat"`, `".rdata"`
#' * Any: `NULL` or `TRUE` automatically selects either `".csv"` (one raster layer) or `".rds` (two or more)
#' * Any: `FALSE` disables saving of a levels file.
#'
#' @param compress Character: Type of compression to use for GeoTIFF files:
#'    * `"LZW"` (default)
#'    * `"DEFLATE"`
#'    * `"PACKBITS"`
#'    * `"LZMA"`
#'    * `NULL`: No compression is used, but the file can still be reduced in size by using zip, gzip, or other compressions.
#'
#' @param warn Logical: If `TRUE` (default), display a warning if the `datatype` argument does not match the value given by `datatype(x, "GDAL")`, or if the `fileExt` argument will not work with the given raster and so has been automatically changed.
#'
#' @param ... Additional arguments. These can include:
#' * `bigTiff`: Logical: If `TRUE`, and the file format is a GeoTIFF and would be larger than 4 GB (regardless of compression), then the file will be saved in BIGTIFF format.
#' * `format`: Character, indicating file format. This is usually ascertained from the file extension, but in case this fails, it can be stated explicitly. When using other formats, you may have to specify the `createopts` argument, too (see help page for **GRASS** module `r.out.gdal`). Two common formats include:
#'    * `"GTiff"` (default): GeoTIFF `filename` ends in `.tif`.
#'    * `"ASC"`: ASCII `filename` ends in `.asc`
#' * Additional arguments to send to **GRASS** modules `r.out.gdal` and `r.out.ascii`.
#' * `precision`: Numeric: For ASCII files, you may need to state the number of significant digits. 32-bit values have 7 digits and 64-bit values have 16. So in these cases the argument would be `precision=7` or `precision=16`.
#'
#' @returns A `GRaster` (invisibly). A raster is also saved to disk.
#'
#' @seealso [terra::writeRaster()], **GRASS** module `r.out.gdal` (see `grassHelp("r.out.gdal")`)
#'
#' @example man/examples/ex_writeRaster.r
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
		byLayer = FALSE,
		names = TRUE,
		levelsExt = NULL,
		compress = "LZW",
		warn = TRUE,
		...
	) {
	
	if (!is.null(levelsExt) & !is.logical(levelsExt)) {

		levelsExts <- c(".csv", ".rds", ".rda", ".rdata", "csv", "rds", "rda", "rdata", "rdat")
		levelsExt <- omnibus::pmatchSafe(levelsExt, levelsExts, nmax = 1L)
		if (substr(levelsExt, 1L, 1L) != ".") levelsExt <- paste0(".", levelsExt)
		
	}

	.locationRestore(x)
	.region(x)

	nLayers <- nlyr(x)

	# save each layer separately
	if (byLayer) {
	
		for (i in seq_len(nLayers)) {
		
			xx <- x[[i]]
			extension <- .fileExt(filename)
			fn <- substr(filename, 1L, nchar(filename) - nchar(extension) - 1L)
			fn <- paste0(fn, "_", names(xx), ".", extension)
			writeRaster(xx, filename = fn, overwrite = overwrite, datatype = datatype, byLayer = FALSE, names = names, levelsExt = levelsExt, compress = compress, warn = warn, ...)
		
		}
	
	# save all layers in a single file
	} else {

		dots <- list(...)
		filename <- trimws(filename)

		flags <- c(.quiet())
		if (overwrite) flags <- c(flags, "overwrite")

		filename <- normalizePath(filename, mustWork = FALSE)

		### going to overwrite anything?
		if (!overwrite) {
			if (file.exists(filename)) stop("File already exists and `overwrite` is FALSE:\n  ", filename)
		}

		### format
		extension <- .fileExt(filename)
		extension <- tolower(extension)
		
		ascii <- if ("format" %in% names(dots)) {
			if (("format" %in% names(dots) && tolower(dots$format) %in% c("asc", "asci", "ascii")) || extension %in% c("asc", "asci", "ascii")) { TRUE } else { FALSE }
		} else {
			FALSE
		}

		geotiff <- ("format" %in% names(dots) && tolower(dots$format) == "gtiff") | extension == "tif" | extension == "tiff"

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

			if (names) .saveNames(x = x, filename = filename)

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
			if (is.null(datatype)) datatype <- datatype(x, "GRASS")

			bounds <- minmax(x)
			mins <- bounds["min", ]
			maxs <- bounds["max", ]

			if (any(datatype %in% c("double", "FLT8S", "DCELL"))) {

				datatype <- "Float64"

			} else if (any(datatype %in% c("float", "FLT4S", "FCELL"))) {

				datatype <- "Float32"

			} else if (any(datatype %in% c("factor", "integer", "logical", "CELL"))) {

				if (all(mins >= 0L & maxs <= 255L)) {
					datatype <- "Byte"
				} else if (all(mins >= 0L & maxs <= 65534L)) {
					datatype <- "UInt16"
				} else if (any(mins < 0L) & all(maxs >= -32767L) & all(maxs <= 32767L)) {
					datatype <- "Int16"
				} else if (any(mins < 0L) & all(maxs >= -2147483647L) & all(maxs <= -2147483647)) {
					datatype <- "Int32"
				# } else if (all(mins >= -3.4E+38) & all(maxs <= 3.4E+38)) {
					# datatype <- "Float32" # causes r.out.gdal not to write some DCELL rasters
				} else {
					datatype <- "Float64"
				}

			} else {
			
				# if (all(mins >= -3.4E+38) & all(maxs <= 3.4E+38)) {
					# datatype <- "Float32"
				# } else {
					datatype <- "Float64"
				# }
			
			}
			
			if (any(datatype == "INT1U")) datatype[datatype == "INT1U"] <- "Byte" # will not write, for some reason
			# if (any(datatype == "INT1U")) datatype[datatype == "INT1U"] <- "UInt16"
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
					"Byte" # Will not write, for some reason
					# "UInt16"
				}
				
			}

			if (any(datatype(x, "fasterRaster") %in% c("float", "double")) & datatype %in% c("Byte", "UInt16", "Int32")) {

				stop("Trying to save non-integer rasters with an integer datatype. You can:\n  * Change the datatype of the raster to integer using as.int(), trunc(), round(), floor(), or ceiling();\n  * Change the raster to a non-integer type using as.float() or as.doub();\n  * When using writeRaster(), set argument `datatype` to `FLT4S` or `FLT8S`, depending on their minimum and maximum values.")
				
			}

			# if (any(!(datatype(x, "GDAL") %in% datatype))) {
			# 	flags <- c(flags, "f")
			# 	if (warn) warning("Argument `datatype` does not match the data type of the raster. Data may be lost.")
			# }

			if (!("createopt" %in% names(dots))) createopt <- NULL

			# GeoTIFF options
			if (geotiff) {

				createopt <- c(createopt, "PROFILE=GeoTIFF")
				if (!is.null(compress)) {
					compress <- omnibus::pmatchSafe(compress, c("LZW", "DEFLATE", "PACKBITS", "LZMA"), nmax = 1L)
					createopt <- c(createopt, paste0("COMPRESS=", toupper(compress)))
				}
				if ("bigTiff" %in% names(dots) && dots$bigTiff) {
					createopt <- c(createopt, "BIGTIFF=YES")
				} else {
					createopt <- c(createopt, "BIGTIFF=IF_NEEDED")
				}
				if (datatype %in% c("Byte", "UInt16", "Int32")) createopt <- c(createopt, "PREDICTOR=2")
				if (datatype %in% c("Float32", "Float64")) createopt <- c(createopt, "PREDICTOR=3")
				
				createopt <- unique(createopt)
				createopt <- paste(createopt, collapse=",")

				# mm <- minmax(x)
				# metaopt <- paste0("TIFFTAG_MINSAMPLEVALUE=", paste(mm[1L, ], collapse=","))
				# metaopt <- c(metaopt, paste0("TIFFTAG_MAXSAMPLEVALUE=", paste(mm[2L, ], collapse=",")))
				# metaopt <- paste0("STATISTICS_MINIMUM=", paste(mm[1L, ], collapse=","))
				# metaopt <- c(metaopt, paste0("STATISTICS_MAXIMUM=", paste(mm[2L, ], collapse=",")))
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
				# metaopt = 'TIFFTAG_IMAGEDESCRIPTION="TEST',
				flags = thisFlags
			)

			if (names) .saveNames(x = x, filename = filename)

		}
		
		isFact <- is.factor(x)
		if (any(isFact)) {

			if (is.logical(levelsExt)) {
				if (levelsExt) {
					levelsExt <- NULL
					saveLevels <- TRUE
				} else {
					saveLevels <- FALSE
				}
			} else {
				saveLevels <- TRUE
			}

			if (saveLevels) {

				categs <- cats(x)
				if (is.null(levelsExt)) {
					if (length(categs) > 1L) {
						levelsExt <- ".rds"
					} else {
						levelsExt <- ".csv"
					}
				}
				
				if (length(categs) > 1L & tolower(levelsExt) == ".csv") {

					if (warn) warning("You cannot save levels files of multi-layered rasters using a `levelsExt` value of `.csv`.\n  The file extension has been changed to `.rds`, which saves files that can be read using `readRDS()`.")

					levelsExt <- ".rds"

				}

				levelFileName <- substr(filename, 1L, nchar(filename) - nchar(levelsExt))
				levelFileName <- paste0(levelFileName, levelsExt)
				
				# save
				if (tolower(levelsExt) %in% ".rds") {
					saveRDS(categs, levelFileName)
				} else if (tolower(levelsExt) %in% c(".rds", ".rda", ".rdat", ".rdata")) {
					save(categs, levelFileName)
				} else if (tolower(levelsExt) %in% ".csv") {
					utils::write.csv(categs, levelFileName, row.names = FALSE)
				}

			} # "yes" to saving levels

		} # save levels

		# out <- terra::rast(filename)
		# names(out) <- names(x)
		
		# if (any(isFact)) {
		# 	for (i in which(isFact)) {
		# 		cats <- as.data.frame(cats(x)[[i]])
		# 		out <- terra::categories(out, layer = i, value = cats, active = activeCat(x, layer = i))
		# 	}
		# }
		
		# if (mm) out <- terra::setMinMax(out)
		# invisible(out)

	} # save all layers in a single file
	
	invisible(x)
	
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

### save "names" file for each layer of a raster
.saveNames <- function(x, filename) {

	namesFile <- filename
	n <- nchar(filename)
	ne <- nchar(.fileExt(filename))
	namesFile <- substr(namesFile, 1L, n - ne - 1L)
	namesFile <- paste0(namesFile, '_names.csv')

	namesDf <- data.table::data.table(
		layer = 1:nlyr(x),
		name = names(x)
	)

	data.table::fwrite(namesDf, namesFile, row.names = FALSE)

}


