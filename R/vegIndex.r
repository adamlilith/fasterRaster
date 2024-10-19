#' Vegetation indices from surface reflectance
#'
#' @description This function calculates one of many types of vegetation indices from a raster with four bands representing blue (B), green (G), red (R), and near infrared (NIR), plus possibly channels 5 and 7. The function requires rasters that represent surface reflectance, so should have values that fall in the range 0 to 1, unless they are digital number rasters (e.g., integers in the range 0 to 255). If digital number format is used, then the `bits` argument should be defined.
#' 
#' @param x A `GRaster` with one layer per required band. Values should be between 0 and 1.
#' 
#' @param index Character or character vector: The vegetation index or indices to calculate. You can find a list of available indices using [fastData("vegIndices")][fastData] (also see [vegIndices]). The first column, "`index`" provides the name of the index, and these are the values that this argument will accept (e.g., "NDVI", "EVI2"). Partial matching is used, and case is ignored. You can also use these shortcuts:
#' * `"*"`: Calculate *all* indices
#' * `"RNIR"`: Calculate all indices that use R and NIR channels (but not other channels).
#' * `"NotSoil"`: Calculate all indices that use any channels but do not require `soilSlope` or `soilIntercept`.
#'
#' *Note*: A near-comprehensive table of indices can be found on the [Index Database: A Database for Remote Sensing Indices](https://www.indexdatabase.de).
#' 
#' @param r,g,b,nir Numeric or character: Index or [names()] of the layers in `x` that represent the red, green, blue, and near infrared channels. Values must be in the range from 0 to 1 or integers.
#' 
#' @param b5,b7 Numeric or character: Index of names of the layers representing bands 5 and 7. These are used only for GVI and PVI. Values must be in the range from 0 to 1 or integers.
#' 
#' @param bits Either `NULL` (default) or numeric integer or integer with a value of 7, 8, 10, or 16: If the rasters are represented by integers (so do not fall in the range of 0 to 1), then the number of bits can be supplied using `bits`. If this is the case, then they will range from 0 to 2^`n`, where `n` is 7, 8, 10, or 16. If bit rasters are supplied, they must be of [datatype()] "integer". If raster values are in the range from 0 to 1, then `bits` should be `NULL` (default).
#' 
#' @param soilSlope,soilIntercept,soilNR Numeric: Values of the soil slope, intercept, and soil noise reduction factor (0.08, by default). Used only for calculation of MSAVI.
#' 
#' @returns A `GRaster`.
#' 
#' @seealso **GRASS** manual page for module `i.vi` (see `grassHelp("i.vi")`)
#' 
#' @example man/examples/ex_vegIndex.r
#' 
#' @aliases vegIndex
#' @rdname vegIndex
#' @exportMethod vegIndex
methods::setMethod(
	f = "vegIndex",
	signature = c(x = "GRaster"),
	function(
		x,
		index = "NDVI",
		r = NULL,
		g = NULL,
		b = NULL,
		nir = NULL,
		b5 = NULL,
		b7 = NULL,
		soilSlope = NULL,
		soilIntercept = NULL,
		soilNR = 0.08,
		bits = NULL
	) {

	mm <- minmax(x)
	if (any(mm < 0)) stop("Raster values cannot be < 0.")

	if (is.null(bits)) {

		if (any(mm > 1)) stop("Raster values should be between 0 and 1 if `bits` is NULL.")

	} else {

		if (!all(is.int(x))) stop("If argument `bits` is defined, then rasters must be of type `integer`.")

		maxVal <- 2^bits
		if (any(mm > maxVal)) stop("Maximum value for ", bits, " bits is ", maxVal, ".\n  At least one raster surpasses this.")

	}

	.locationRestore(x)
	.region(x)

	vegIndices <- NULL
	utils::data("vegIndices", envir = environment(), package = "fasterRaster")

	index <- tolower(index)
	if ("*" %in% index) {
		index <- vegIndices$index
	} else if ("notsoil" %in% index) {
    	index <- c(
			index,
			vegIndices[!vegIndices$soilSlope & !vegIndices$soilIntercept, "index", drop = TRUE]
		)
	} else if ("rnir" %in% index) {
    	index <- c(
			index,
			vegIndices[vegIndices$R & !vegIndices$G & !vegIndices$B & vegIndices$NIR & !vegIndices$channel5 & !vegIndices$channel7 & !vegIndices$soilSlope & !vegIndices$soilIntercept, "index", drop = TRUE]
		)
		index <- index[index != "RNIR"]
		index <- index[index != "rnir"]

	}

	# index <- sort(index)
	indexOrder <- order(index)
	index <- sort(index)
	index <- omnibus::pmatchSafe(index, vegIndices$index, useFirst = TRUE)
	index <- index[indexOrder]

	index <- tolower(index)

	if (!is.null(r) && is.character(r)) r <- match(names(x), r)
	if (!is.null(g) && is.character(g)) g <- match(names(x), g)
	if (!is.null(b) && is.character(b)) b <- match(names(x), b)
	if (!is.null(nir) && is.character(nir)) nir <- match(names(x), nir)
	if (!is.null(b5) && is.character(b5)) b5 <- match(names(x), b5)
	if (!is.null(b7) && is.character(b7)) b7 <- match(names(x), b7)

	srcs <- .makeSourceName(index, "raster")
	for (i in seq_along(index)) {

		args <- list(
			cmd = "i.vi",
			viname = index[i],
			output = srcs[i],
			flags = c(.quiet(), "overwrite"),
			intern = TRUE
		)

		if (!is.null(bits)) args$storage_bit <- bits

		if (!is.null(r)) args$red <- sources(x)[r]
		if (!is.null(g)) args$green <- sources(x)[g]
		if (!is.null(b)) args$blue <- sources(x)[b]
		if (!is.null(nir)) args$nir <- sources(x)[nir]
		
		if (!is.null(b5)) args$band5 <- sources(x)[b5]
		if (!is.null(b7)) args$band7 <- sources(x)[b7]
		
  		if (!is.null(soilSlope)) args$soil_line_slope <- soilSlope
  		
		if (!is.null(soilIntercept)) args$soil_line_slope <- soilIntercept

  		if (!is.null(soilNR)) args$soil_noise_reduction <- soilNR

		do.call(rgrass::execGRASS, args = args)

	} # next metric
	.makeGRaster(srcs, toupper(index))

	} #OF
)
