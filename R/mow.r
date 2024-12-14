#' Remove rasters and vectors from the GRASS cache
#'
#' @description **fasterRaster** functions attempt to delete rasters and vectors in the **GRASS** cache, but not all intermediate files can be removed. This function can be used to clear the cache of extraneous rasters and vectors.
#'
#' Calling this function inside another function's environment and defining `x` as `"*"` can be very **dangerous**, as it will detect objects outside of that environment, and thus delete any rasters/vectors outside that environment. Here is a guide:
#' * To delete files associated with a single `GRaster` or `GVector`, use `mow(GRaster_to_unlink)` or `mow(GVector_to_unlink)`.
# * To delete files associated with more than one `GRaster`s and/or `GVector`s, provide them as a list. For example, use `mow(list(GRaster_to_unlink, GVector_to_unlink))`.
#' To remove all rasters, all vectors, or all rasters and vectors in the **GRASS** cache that are not linked to a `GRaster` or `GVector`, use `mow("*")`.
#' To remove all rasters or all vectors in the **GRASS** cache, use `mow("*", type = "rasters")` or `mow("*", type = "vectors")`.
#' To remove all rasters or all vectors in the **GRASS** cache *except* for certain ones, use `mow("*", unlinked = FALSE, keep = list(GRaster_to_keep, GVector_to_keep))`. You can combine this with the `keep` argument to retain specific rasters or vectors. For example, you can use `mow("*", unlinked = FALSE, type = "rasters", keep = list(GRaster_to_keep))`.
#'
#' @param x Any of:
#' * `"unlinked"` (default): Delete **GRASS** rasters and/or vectors that are unlinked to `GRaster`s or `GVector`s in the environment in which the function was called, or the environment named in `pos`.
#' * A `GRaster` or `GVector`: Delete the **GRASS** raster or vector pointed to by this object.
#' * A `list` of `GRaster`s and/or `GVector`s: Delete the **GRASS** raster(s) and/or vector(s) pointed to by these objects.
#' * `"*"`: Delete *all* **GRASS** rasters and/or vectors pointed to by objects in the environment named in `pos`. Only objects in `keep` will not be deleted.
#' 
#' @param pos Either `NULL` (default), or an environment. This is used only if `x` is `"unlinked"` or `"*"`. In that case, if `pos` is `NULL`, the environment in which this function was called will be searched for `GRaster`s and `GVector`s for removal of their associated **GRASS** rasters and vectors. Otherwise, the named environment will be searched.
#'
#' @param type Either `NULL` or a character vector. This is used only if `x` is `"unlinked"` or `"*"`. If `NULL`, all rasters and vectors in the **GRASS** cache are candidates for deletion. Otherwise, this can be either `"rasters"`, `"vectors"`, or both.
#'
#' @param keep Either `NULL` (default) or a `list()` of `GRaster`s and/or `GVector`s that you want to retain. This is used only if `x` is `"unlinked"` or `"*"`. The rasters and vectors in **GRASS** pointed to by these objects will not be deleted.
#'
#' @param verbose Logical: If `TRUE` (default), report progress.
#'
#' @param ask Logical: If `TRUE` (default), prompt for reassurance. This is used only if `x` is `"unlinked"` or `"*"`.
#'
#' @returns Invisibly returns a named vector with the number of rasters and vectors deleted.
#'
#' @seealso [terra::tmpFiles()]
#'
#' @example man/examples/ex_mow.r
#'
#' @aliases mow
#' @rdname mow
#' @export mow
mow <- function(
	x = "unlinked",
	pos = NULL,
	type = NULL,
	keep = NULL,
	verbose = TRUE,
	ask = TRUE
) {
	
	### delete a single GRaster or GVector
	######################################
	if (inherits(x, "GRaster") | inherits(x, "GVector")) {
		
		.locationRestore(x)
		.rm(x)
		return(invisible(c(rasters = 0, vectors = 0)))
	
	}

	### delete a list of GRasters/GVectors
	######################################
	if (is.list(x)) {

		# make data table of objects
		GSpatials <- data.table::data.table()
		for (i in seq_along(x)) {

			thisClass <- if (inherits(x[[i]], "GRaster")) {
				"GRaster"
			} else if (inherits(x[[i]], "GVector")) {
				"GVector"
			} else {
				"other"
			}

			GSpatials <- rbind(
				GSpatials,
				data.table::data.table(
					sources = sources(x[[i]]),
					project = .location(x[[i]]),
					class = thisClass
				)
			)

		}

		out <- c(rasters = 0L, vectors = 0L)

		# delete rasters
		if (any(GSpatials$class == "GRaster")) {

			projs <- unique(GSpatials$project[GSpatials$class == "GRaster"])
			if (verbose | faster("verbose")) omnibus::say("Deleting ", sum(GSpatials$class == "GRaster"), " raster(s) from the GRASS cache...")
			
			for (proj in projs) {
			
				.locationRestore(proj)

				srcs <- GSpatials$sources[GSpatials$class == "GRaster" & GSpatials$project == proj]
				out[["rasters"]]  <- out[["rasters"]] + length(srcs)
				.rm(srcs, type = "rasters", verify = FALSE)

			}
			
		}

		# delete vectors
		if (any(GSpatials$class == "GVector")) {

			projs <- unique(GSpatials$project[GSpatials$class == "GVector"])
			if (verbose | faster("verbose")) omnibus::say("Deleting ", sum(GSpatials$class == "GVector"), " vector(s) from the GRASS cache...")
			
			for (proj in projs) {
			
				.locationRestore(proj)

				srcs <- GSpatials$sources[GSpatials$class == "GVector" & GSpatials$project == proj]
				out[["vectors"]]  <- out[["vectors"]] + length(srcs)
				.rm(srcs, type = "vectors", verify = FALSE)

			}
			
		}

		return(invisible(out))

	}

	### delete either everything except objects in `keep` or just unlinked rasters/vectors
	######################################################################################
	if (is.character(x)) {

		x <- omnibus::pmatchSafe(x, c("*", "unlinked"), nmax = 1L)

		if (is.null(type)) type <- c("rasters", "vectors")
		type <- omnibus::pmatchSafe(type, c("rasters", "vectors"), nmax = 2L)

		if (ask & x %in% c("*", "unlinked")) {
		
			response <- readline("This action will break any existing GRasters and GVectors.\nAre you sure you want to empty the entire GRASS cache? (Y/n) ")
			if (response != "Y") {
				if (verbose | faster("verbose")) omnibus::say("Nothing deleted.")
				return(invisible(c(rasters = 0, vectors = 0)))
			}
		
		}

		if (x == "*" && !is.null(keep) && !is.list(keep)) stop("Argument `keep` must be a list or NULL.")

		### get GRASS file names in each project/location
		projs <- names(.fasterRaster$locations)
		grassObjs <- list()
		for (proj in projs) {

			.locationRestore(proj)
			grassObjs[[length(grassObjs) + 1L]] <- .ls(type = type)
			names(grassObjs)[length(grassObjs)] <- proj
		
		}

		if (all(sapply(grassObjs, length) == 0L)) {
			if (verbose | faster("verbose")) omnibus::say("Nothing deleted.")
			return(invisible(c(rasters = 0, vectors = 0)))
		}

		### get just UNLINKED rasters/vectors
		if (x == "unlinked") {

			# get all R objects
			if (is.null(pos)) {
				rObjs <- ls(envir = parent.frame(n = 1L))
			} else {
				rObjs <- ls(pos)
			}

			# get sources of R objects
			srcs <- character()
			for (i in seq_along(rObjs)) {

				thisObj <- get(rObjs[[i]])
				if (inherits(thisObj, c("GRaster", "GVector"))) {
					srcs <- c(srcs, sources(thisObj))
				}
			
			}

			# remove linked ones
			for (proj in projs) {
				grassObjs[[proj]] <- grassObjs[[proj]][!(grassObjs[[proj]] %in% srcs)]
			}

		} else if (x == "*") {
		
			# remove keep from list to be deleted
			if (is.list(keep)) {
			
				srcs <- character()
				for (i in seq_along(keep)) srcs <- c(srcs, sources(keep[[i]]))

				for (proj in projs) {
					grassObjs[[proj]] <- grassObjs[[proj]][!(grassObjs[[proj]] %in% srcs)]
				}

			}
		
		}

		out <- c(rasters = 0, vectors = 0)
		for (proj in projs) {
			out[["rasters"]] <- out[["rasters"]] + sum(names(grassObjs[[proj]]) == "raster")
			out[["vectors"]] <- out[["vectors"]] + sum(names(grassObjs[[proj]]) == "vector")
		}

		if (all(sapply(grassObjs, length) == 0)) {
			if (verbose | faster("verbose")) omnibus::say("Nothing deleted.")
			return(invisible(out))
		} else {

			if (verbose | faster("verbose")) omnibus::say("Deleting ", out[["rasters"]], " raster(s) and ", out[["vectors"]], " vector(s)...")
					
			for (proj in projs) {
				if (length(grassObjs[[proj]]) > 0L) {

					if (any(names(grassObjs[[proj]]) == "raster")) {

						.locationRestore(proj)
						.rm(grassObjs[[proj]], type = "raster", warn = FALSE, verify = FALSE)
					
					}
					if (any(names(grassObjs[[proj]]) == "vector")) {

						.locationRestore(proj)
						.rm(grassObjs[[proj]], type = "vector", warn = FALSE, verify = FALSE)
					
					}
				}
			}
		}
		invisible(return(invisible(out)))

	}

} # EOF

