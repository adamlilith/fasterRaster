#' Sine wave rasters
#'
#' @description This function creates one or more rasters with sine waves in the north-south and east-west directions.
#'
#' @param x A `GRaster`.
#'
#' @param ns,ew Numeric: Number of complete sine waves (i.e., wavelengths) in the north-south and east-west directions. A wavelength of 1 creates a "full" sine wave (e.g., starting at 0 at one end and ending at 0 at the other). A wavelength of 2 would create two such waves, and so on. A value of 0 creates no waves in the given direction (i.e., each row or column has constant values). The default value is 1.
#'
#' @param nsOffset,ewOffset Numeric: Offset of the sine waves from the edges of the raster, expressed as a proportion of the length of the raster. The default is 0, so the values of the outermost cells will be close to 0 (but not exactly 0 because centers of cells at the raster edges are not on the actual edge). If an offset value is 0.2, for example, then it will be pushed "inward" toward the middle of the raster by 20% of the raster's extent.
#'
#' @param nsAmp,ewAmp Numeric: Amplitude (minimum and maximum of the sine wave) in the north-south and east-west directions. The default is 1. Note that when north-south and east-west waves are created (i.e., `ns` and `ew` are both > 0), the effective amplitude is halved so that the sum is equal to `nsAmp + ewAmp`.
#'
#' @param combos Logical: If `TRUE` (default), create sine rasters using all possible combinations of values of `ns`, `ew`, `nsOffset`, `ewOffset`, and `amp`. If `FALSE`, you can only supply either one value per parameter, or the same number of values per parameter. In this latter case, one raster will be created per pairwise set of the unique parameters. For example, you could specify 3 values for `ns` and either one or three values for any of the other parameters, and three rasters would be created.
#'
#' @param mask Either `NULL` (default), or a `GRaster` or `GVector`: Used as a mask for the output. If this is a `GVector`, then only cells that are not `NA` in the mask have values. If this is a `GVector`, then only cells that overlap the vector have values assigned. All other values will be `NA`.
#'
#' @param verbose Logical: If `TRUE`, display progress.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_sineRast.r
#'
#' @aliases sineRast
#' @rdname sineRast
#' @exportMethod sineRast
methods::setMethod(
	f = "sineRast",
	signature = c(x = "GRaster"),
	function(
		x,
		ns = 1,
		ew = 1,
		nsOffset = 0,
		ewOffset = 0,
		nsAmp = 1,
		ewAmp = 1,
		combos = FALSE,
		mask = NULL,
		verbose = FALSE
	) {

	if (any(ns < 0 | ew < 0)) stop("Arguments `ns` and `ew` must be >= 0.")

	if (combos) {

		settings <- expand.grid(ns = ns, ew = ew, nsOffset = nsOffset, ewOffset = ewOffset, nsAmp = nsAmp, ewAmp = ewAmp)
		settings <- data.table::as.data.table(settings)

	} else {

		lengths <- c(length(ns), length(ew), length(nsOffset), length(ewOffset), length(nsAmp), length(ewAmp))
		maxLength <- max(lengths)
	
		bad <- any(lengths != 1 & lengths != maxLength)
		if (bad) stop("If `combo` is FALSE, then arguments `ns`, `ew`, `nsOffset`,  `ewOffset`,\n  `nsAmp`, and `ewAmp` must be a single value or the same length. You can mix\n  and match. For example, `ns` can have a single value, and `ew` can have\n  3 values. But in this case, the other parameters must have 1 or 3 values.")

		settings <- data.table::data.table(ns = ns, ew = ew, nsOffset = nsOffset, ewOffset = ewOffset, nsAmp = nsAmp, ewAmp = ewAmp)

	}

	ns <- settings$ns
	ew <- settings$ew
	nsOffset <- settings$nsOffset
	ewOffset <- settings$ewOffset
	nsAmp <- settings$nsAmp
	ewAmp <- settings$ewAmp
	nRastsToMake <- nrow(settings)

	.locationRestore(x)
	.region(x)

	if (nlyr(x) > 1) x <- x[[1L]]
	extent <- ext(x, vector = TRUE)

	if ((verbose | faster("verbose")) & nRastsToMake > 1)  {
		pb <- utils::txtProgressBar(min = 0, max = nRastsToMake, initial = 0, style = 3, width = 30)
	}

	srcs <- .makeSourceName("sineRast", "raster", nrow(settings))
	for (i in seq_len(nRastsToMake)) {

		if ((verbose | faster("verbose")) & nRastsToMake > 1) utils::setTxtProgressBar(pb, i)

		### create expression
		# no waves
		if (omnibus::compareFloat(ns[i], 0, "==") & omnibus::compareFloat(ew[i], 0, "==")) {
			ex <- paste0(srcs[i], " = 0")
		# waves in at least on direction
		} else {

			# scale wavelength
			# set cell values to [0, 2* pi] along extent
			ymin <- extent[3L] + 0.5 * res(x)[2L]
			ymax <- extent[4L] - 0.5 * res(x)[2L]
			yext <- ymax - ymin

			xmin <- extent[1L] + 0.5 * res(x)[1L]
			xmax <- extent[2L] - 0.5 * res(x)[1L]
			xext <- xmax - xmin

			ewOffset <- 360 * ewOffset
			nsOffset <- 360 * nsOffset

			if (ns[i] > 0 & ew[i] > 0) {
				halfNsAmp <- nsAmp[i] / 2
				halfEwAmp <- ewAmp[i] / 2
				ex <- paste0(srcs[i], " = ", halfNsAmp, " * sin(", ewOffset[i] + ew[i] * 360, " * ((x() - ", xmin, ") / ", xext, ")) + ", halfEwAmp, " * sin(", nsOffset[i] + ns[i] * 360, " * ((y() - ", ymin, ") / ", yext, "))")
			} else if (ns[i] > 0 & omnibus::compareFloat(ew[i], 0, "==")) {
				ex <- paste0(srcs[i], " = ", nsAmp[i], " * sin(", nsOffset[i] + ns[i] * 360, " * ((y() - ", ymin, ") / ", yext, "))")
			} else if (omnibus::compareFloat(ns[i], 0, "==") & ew[i] > 0) {
				ex <- paste0(srcs[i], " = ", ewAmp[i], " * sin(", ewOffset[i] + ew[i] * 360, " * ((x() - ", xmin, ") / ", xext, "))")
			}

		}
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# mask
		if (!is.null(mask)) {

			if (inherits(mask, "GRaster")) {
				maskType <- "raster"
			} else if (inherits(mask, "GVector")) {
				maskType <- "vector"
			} else {
				stop("Argument `mask` must be a `GRaster` or `GVector`.")
			}

			srcs[i] <- .mask(x = srcs[i], mask = sources(mask), maskType = maskType, inverse = FALSE, maskvalues = NA, updatevalue = NA)
		
		}

	}
	if ((verbose | faster("verbose")) & nRastsToMake > 1)  close(pb)

	names <- paste0("ns", ns, "_ew", ew, "_nsOffset", nsOffset, "_ewOffset", ewOffset, "_nsAmp", nsAmp, "_ewAmp", ewAmp)
	.makeGRaster(srcs, names)

	} # EOF
)
