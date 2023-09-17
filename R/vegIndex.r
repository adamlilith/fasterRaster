#' Vegetation indices from surface reflectance
#'
#' @description This function calculates one of many types of vegetation indices from a raster with four bands representing blue (B), green (G), red (R), and near infrared (NIR), plus possibly channels 5 and 7. The function requires rasters that represent reflectance, so should have values that fall in the range 0 to 1.
#' 
#' The [toarLandsat()] or [toarAster()] functions can convert LANDSAT and ASTER digital-number rasters to radiance rasters, and then to reflectance rasters.
#'
#' @param x A `GRaster` with one layer per required band. Values should be between 0 and 1.
#' 
#' @param metric Character or character vector: The vegetation index or indices to calculate. Partial matching is used, and case is ignored. More infomration on each index can be found on the **GRASS** help page for the `i.vi` module.
#' 
#' * `ARVI`: Atmospherically Resistant Vegetation Index - Less sensitive to atmospheric interference than NDVI. Requires RGB channels.
#' * `CI`: Crust Index - Suitable for detecting biogenic crust on soil. Requires RB channels.
#' * `DVI`: Difference Vegetation Index - Requires R and NIR channels.
#' * `EVI`: Enhanced Vegetation Index - Saturates more slowly than NDVI in high-biomass areas.  Requires RB an NIR channels.
#' * `EVI2`: Enhanced Vegetation Index 2 - As EVI.  Requires R and NIR channels.
#' * `GARI`: Green atmospherically resistant vegetation index.  Requires RGB and NIR channels.
#' * `GEMI`: Global Environmental Monitoring Index.  Requires R and NIR channels.
#' * `GVI`: Green Vegetation Index - Requires RGB, NIR, plus channels 5 and 7.
#' * `IPVI`: Infrared Percentage Vegetation Index.  Requires R and NIR channels.
#' * `MSAVI`: Modified Soil Adjusted Vegetation Index. Requires R and NIR channels, plus soil line slope, intercept, and a noise reduction factor.
#' * `MSAVI2`: Second Modified Soil Adjusted Vegetation Index. Requires R and NIR channels.
#' * `NDVI`: Normalized Difference Vegetation Index.  Requires R and NIR channels.
#' * `NDWI`: Normalized Difference Water Index. Requires G and NIR channels.
#' * `PVI`: Perpendicular Vegetation Index. Requires R and NIR channels.
#' * `SAVI`: Soil Adjusted Vegetation Index. Requires R and NIR channels.
#' * `SR`: Simple Vegetation ratio. Requires R and NIR channels.
#' * `WDVI`: Weighted Difference Vegetation Index. Requires R, NIR, and soil line weight channels.
#'
#' @param r,g,b,nir Numeric or character: Index or [names()] of the layers in `x` that represent the red, green, blue, and near infrared channels.
#' 
#' @param b5,b7 Numeric or character: Index of names of the layers representing bands 5 and 7. These are used only for GVI.
#' 
#' @param soilSlope,soilIntercept,soilNR Numeric: Values of the soil slope, intercept, and soil noise reduction factor (0.08, by default). Used only for calculation of MSAVI.
#' 
#' @returns A `GRaster`.
#' 
#' @example man/examples/ex_vegIndex.r
#' 
#' @aliases vegIndex
#' @rdname vegIndex
#' @exportMethod vegIndex
methods::setMethod(
	f = "vegIndex",
	signature = c(x = "GRaster"),
	function(x, metric = "NDVI", r = NULL, g = NULL, b = NULL, nir = NULL, b5 = NULL, b7 = NULL, soilSlope = NULL, soilIntercept = NULL, soilNR = 0.08) {

	mm <- minmax(x)
	if (any(mm < 0) | any(mm > 1)) stop("Values in the raster should be between 0 and 1.")

	.restore(x)
	region(x)

	metrics <- c("arvi", "ci", "dvi", "evi", "evi2", "gvi", "gari", "gemi", "ipvi", "msavi", "msavi2", "ndvi", "ndwi", "pvi", "savi", "sr", "vari", "wdvi")

	metric <- pmatchSafe(metric, metrics)

	if (!is.null(r) && is.character(r)) r <- match(names(x), r)
	if (!is.null(g) && is.character(g)) g <- match(names(x), g)
	if (!is.null(b) && is.character(b)) b <- match(names(x), b)
	if (!is.null(nir) && is.character(nir)) nir <- match(names(x), nir)
	if (!is.null(b5) && is.character(b5)) b5 <- match(names(x), b5)
	if (!is.null(b7) && is.character(b7)) b7 <- match(names(x), b7)

	srcs <- .makeSourceName(metric, "raster")
	for (i in seq_along(metric)) {

		args <- list(
			cmd = "i.vi",
			viname = metric[i],
			output = srcs[i],
			flags = c("quiet", "overwrite"),
			intern = TRUE
		)

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
	.makeGRaster(srcs, metric)

	} #OF
)
