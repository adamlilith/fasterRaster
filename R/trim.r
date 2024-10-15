#' Remove rows and columns from a raster that are all NA
#'
#' @description This function removes any rows and columns from  a `GRaster` that are all `NA`.
#'
#' If the `GRaster` is a stack of rasters, then the rasters will all be trimmed to the same extent such that none have any rows or columns that are all `NA`. In other words, if at least one raster in the stack has a non-`NA` cell in a row or column, all rasters will retain that row or column.
#'
#' @param x A `GRaster`.
#'
#' @param pad Numeric integer: Number of `NA` rows and columns to retain. The default is 0.
#'
#' @returns A `GRaster`.
#'
#' @seealso [terra::trim()], [extend()], and **GRASS** module `g.region`
#'
#' @example man/examples/ex_trim.r
#'
#' @aliases trim
#' @rdname trim
#' @exportMethod trim
methods::setMethod(
	f = "trim",
	signature = c(x = "GRaster"),
	function(x, pad = 0) {
	
	if (pad < 0) stop("Argument `pad` must be >= 0.")

	.locationRestore(x)
	.region(x)

	nLayers <- nlyr(x)

	# for each raster, find smallest extent necessary to encompass all non-NA rows/columns
	n <- s <- e <- w <- rep(NA_real_, nLayers)
	for (i in seq_len(nLayers)) {
	
		rgrass::execGRASS(
			cmd = "g.region",
			raster = sources(x)[i],
			zoom = sources(x)[i],
			flags = c(.quiet(), "overwrite")
		)

		reg <- .region()
		w[i] <- W(reg)
		e[i] <- E(reg)
		s[i] <- S(reg)
		n[i] <- N(reg)
	
	}

	wDelta <- w - W(x)
	eDelta <- e - E(x)
	sDelta <- s - S(x)
	nDelta <- n - N(x)

	w <- w[which.min(wDelta)]
	e <- e[which.min(eDelta)]
	s <- s[which.min(sDelta)]
	n <- n[which.min(nDelta)]

	ewres <- xres(x)
	nsres <- yres(x)

	if (pad > 0) {
	
		w <- max(w - pad * ewres, W(x))
		e <- min(e + pad * ewres, E(x))
  		s <- max(s - pad * nsres, S(x))
		n <- min(n + pad * nsres, N(x))
	
	}

	w <- as.character(w)
	e <- as.character(e)
	s <- as.character(s)
	n <- as.character(n)

	ewres <- as.character(ewres)
	nsres <- as.character(nsres)

	rgrass::execGRASS(
		cmd = "g.region",
		w = w, e = e, s = s, n = n,
		ewres = ewres, nsres = nsres,
		flags = c(.quiet(), "overwrite")
	)

	src <- .copyGSpatial(x, reshapeRegion = FALSE)
	.makeGRaster(src, names(x), levels = cats(x), ac = activeCats(x))
	
	} # EOF
)
