#' Remove rasters and vectors from the GRASS cache
#'
#' @description **fasterRaster** functions attempt to delete rasters and vectors in the **GRASS** cache, but not all intermediate files can be removed. This function will a) search the current **GRASS** "project/location" cache for all rasters/vectors there; and b) remove any of them that are not pointed to by an object in the active **R** environment. Only objects in the currently active **GRASS** project/location will be removed (see `vignette("project_mapset", package = "fasterRaster")`). This is a *very powerful function* that can "disconnect" **R** objects from the **GRASS** objects to which they point.
#'
#' Calling this function inside another function's environment without providing an argument for `x` can be very **dangerous**, as it will detect objects outside of that environment, and thus delete any rasters/vectors outside that environment.
#'
#' @param x Either missing (default) or an environment. If left as missing, the environment in which this function was called will be searched for `GRaster`s and `GVector`s for removal of their associated **GRASS** rasters and vectors. Otherwise, the named environment will be searched.
#'
#' @param type Either `NULL` or a character vector. If `NULL`, all rasters and vectors in the **GRASS** cache are candidates for deletion. Otherwise, this can be either `"raster"`, `"vector"`, or both.
#'
#' @param keep Either `NULL` (default) or a `list()` of `GRaster`s and/or `GVector`s that you want to retain. The rasters and vectors in **GRASS** pointed to by these objects will not be deleted.
#'
#' @param verbose Logical: If `TRUE` (default), report progress.
#'
#' @param ask Logical: If `TRUE` (default), prompt for reassurance.
#'
#' @returns Invisibly returns a list with the number of rasters and vectors deleted.
#'
#' @seealso Option `clean` in [faster()]
#'
#' @example man/examples/ex_mow.r
#'
#' @aliases mow
#' @rdname mow
#' @export mow
mow <- function(x, type = NULL, keep = NULL, verbose = TRUE, ask = TRUE) {
	
	if (ask) {
		response <- readline("Are you sure you want to clean the GRASS cache? (Y/n) ")
		if (response != "Y") {
			if (verbose) omnibus::say("Nothing deleted.")
			return(invisible(list(rasters = 0, vectors = 0)))
		}
	}

	if (!is.null(keep)) {
		if (!is.list(keep)) stop("Argument `keep` must be a list or NULL.")
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

	if (nrow(GSpatials) > 0L & length(keep) > 0L) {
		for (i in seq_along(keep)) {
			GSpatials <- GSpatials[!(GSpatials$rObject %in% sources(keep[[i]]))]
		}
	}

	if (nrow(GSpatials) == 0L) return(invisible(list(rasters = 0, vectors = 0)))

	# see what's in GRASS
	grassObjs <- .ls(type = paste0(type, "s"))

	# any GRASS objects not in the calling environment?
	grassObjs <- grassObjs[!(grassObjs %in% GSpatials$sources)]
	if (length(grassObjs) == 0L) {
		if (verbose) omnibus::say("Nothing deleted.")
		return(invisible(list(rasters = 0, vectors = 0)))
	}

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

