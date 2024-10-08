#' Remove unused rasters and vectors from the GRASS cache
#'
#' @description **fasterRaster** attempts to delete rasters and vectors in the **GRASS** cache. This function will a) search the current **GRASS** "project/location" cache for all rasters/vectors there; and b) remove any of them that are not pointed to by an object in the active **R** environment. Only objects in the currently active **GRASS** project/location will be removed (see `vignette("project_mapset", package = "fasterRaster")`).
#'
#' Note that calling this function inside another function's environment without providing an argument for `x` can be very **dangerous**, as it will detect objects outside that environment, and thus delete any rasters/vectors outside that environment.
#'
#' @param x Either missing (default) or an environment.
#'
#' @param type Either `NULL` or a character vector. If `NULL`, all rasters and vectors in the **GRASS** cache are candidates for deletion. Otherwise, this can be either `"raster"`, `"vector"`, or both.
#'
#' @param keeps Either `NULL` (default) or a `list()` of `GRaster`s and/or `GVector`s that you want to retain. The rasters and vectors in **GRASS** pointed to by these objects will not be deleted.
#'
#' @param verbose Logical: If `TRUE` (default), report progress.
#'
#' @param ask Logical: If `TRUE` (default), prompt for reassurance.
#'
#' @returns Invisibly returns a list with the number of rasters and vectors deleted.
#'
#' @seealso Option `clean` in [faster()]; [grass()]
#'
#' @example man/examples/ex_mow.r
#'
#' @aliases mow
#' @rdname mow
#' @export mow
mow <- function(x, type = NULL, keeps = NULL, verbose = TRUE, ask = TRUE) {
	
	if (ask) {
		response <- readline("Are you sure you want to clean the GRASS cache? (y/n) ")
		if (response != "y") return(invisible(list(rasters = 0, vectors = 0)))
	}

	if (!is.null(keeps)) {
		if (!is.list(keeps)) stop("Argument `keeps` must be a list or NULL.")
	}

	if (missing(x)) {
		x <- ls(envir = parent.frame(n = 1L))
	} else {
		x <- ls(x)
	}

	if (is.null(type)) type <- c("raster", "vector")

	# make data frame of GSpatial objects in environment
	GSpatials <- data.table::data.table()
	for (i in seq_along(x)) {
	
		xThis <- get(x[i])
		if (inherits(xThis, "GSpatial")) {

			GSpatials <- rbind(
				GSpatials,
				data.table::data.table(
					rObject = x[i],
					sources = sources(xThis)
				)
			)

		} # is GSpatial

	} # next object in environment

	if (!is.null(GSpatials)) {
		GSpatials <- GSpatials[!(GSpatials$rObject %in% keeps)]
	}

	if (nrow(GSpatials) == 0L) return(invisible(list(rasters = 0, vectors = 0)))

	# see what's in GRASS
	grassObjs <- .ls(type = paste0(type, "s"))

	# any GRASS objects not in the calling environment?
	grassObjs <- grassObjs[!(grassObjs %in% GSpatials$sources)]
	if (length(grassObjs) == 0L) return(invisible(list(rasters = 0, vectors = 0)))

	### delete
	out <- list(rasters = 0L, vectors = 0L)

	if ("raster" %in% type & "raster" %in% names(grassObjs)) {
	
		toDelete <- grassObjs[names(grassObjs) == "raster"]
		if (verbose) omnibus::say("Deleting ", length(toDelete), " raster(s) from the GRASS cache...")
		.rm(toDelete, type = "raster", warn = TRUE, verify = FALSE)
		out$rasters <- out$rasters + length(toDelete)

	}
	
	if ("vector" %in% type & "vector" %in% names(grassObjs)) {
	
		toDelete <- grassObjs[names(grassObjs) == "vector"]
		if (verbose) omnibus::say("Deleting ", length(toDelete), " vector(s) from the GRASS cache...")
		.rm(toDelete, type = "vector", warn = TRUE, verify = FALSE)
		out$vectors <- out$vectors + length(toDelete)

	}

	invisible(out)

} # EOF

