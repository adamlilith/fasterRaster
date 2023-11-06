#' Make a copy of an object in GRASS
#'
#' Create a copy of a `GRaster` or `GVector` in **GRASS**.  This function is used internally and is of little use to most users.  This only creates a copy of the object in the **GRASS** session--to make a `GRaster` or `GVector`, [.makeGRaster()] or [.makeGVector()] need to be called after making the copy. Note that if the object is multi-layered, then a copy is made of each layer.
#'
#' @param x `GRaster`, `GVector`, or character: The object or the `sources` of the object to be copied. Can take multi-layered objects or multiple `sources`.
#' 
#' @param reshapeRegion Logical: If `TRUE`, reshape the region to match `x` (`GRaster`s only).
#'
#' @returns Character vector representing the new `sources` of each object, plus makes a copy of the given object(s) in **GRASS**.
#'
#' @aliases .copyGSpatial
#' @noRd
methods::setMethod(
	f = ".copyGSpatial",
	signature = c(x = "GRaster"),
	function(x, reshapeRegion = TRUE) .copyGRaster(x, reshapeRegion = reshapeRegion)
)

#' @aliases .copyGSpatial
#' @noRd
methods::setMethod(
	f = ".copyGSpatial",
	signature = c(x = "GVector"),
	function(x) .copyGVector(x)
)

#' @aliases .copyGSpatial
#' @noRd
methods::setMethod(
	f = ".copyGSpatial",
	signature = c(x = "character"),
	function(x, reshapeRegion = TRUE) {
	
	srcs <- .ls()
	rastsOrVects <- names(srcs)
	rastOrVect <- rastsOrVects[match(x, srcs)]
	n <- length(x)
	srcs <- rep(NA_character_, n)

	for (i in seq_len(n)) {
		
		if (rastOrVect[i] == "raster") {
			y <- .makeGVector(x)
			srcsTo[i] <- .copyGRaster(y, reshapeRegion = reshapeRegion)
		} else if (rastOrVect[i] == "vector") {
			y <- .makeGRaster(x)
			srcsTo[i] <- .copyGVector(y)
		}
	
	}
	srcsTo
		
	} # EOF

)

#' @noRd
.copyGRaster <- function(x, reshapeRegion) {

	# NB This function could use `g.copy`, but in some cases it does not have the desired effect. For example, when a MASK raster is present, it correctly copies cells that are not masked, but when the MASK is removed, the masked cells re-appear. Similarly, it ignores the region when copying.

	nLayers <- nlyr(x)
	topo <- topology(x)
	rastOrVect <- if (topo == "2D") { "raster" } else { "raster3d" }

	.restore(x)
	if (reshapeRegion) region(x)
	srcs <- .makeSourceName(x, type = "raster", nLayers)

	for (i in seq_len(nLayers)) {

		ex <- paste0(srcs[i], " = ", sources(x)[i])

		args <- list(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c("quiet", "overwrite"),
			intern = TRUE
		)

		do.call(rgrass::execGRASS, args = args)

	}

	srcs
	
}

#' @noRd
.copyGVector <- function(x) {

	if (inherits(x, "GVector")) {
		.restore(x)
		srcFrom <- sources(x)
	} else {
		srcFrom <- x
	}

	srcTo <- .makeSourceName("g_copy", type="vector")

	fromTo <- paste0(srcFrom, ",", srcTo)
	args <- list(
		cmd = "g.copy",
		vector = fromTo,
		flags = c("quiet", "overwrite")
	)
	
	do.call(rgrass::execGRASS, args = args)

	# ### copy database file
	# gnDb <- .makeSourceName("db", rastOrVect = "vector")
	
	# opts <- getFastOptions(c("workDir", "location", "mapset"))
	# grassDB <- paste(c(opts$workDir, opts$location, opts$mapset, "/sqlite/sqlite.db"), collapse="/")

	# args <- list(
	# 	cmd = "db.copy",
	# 	# from_database = fromDatabase,
	# 	from_database = grassDB,
	# 	from_table = sources(x),
	# 	to_database = grassDB,
	# 	to_table = gnDb,
	# 	flags = c("quiet", "overwrite"),
	# 	intern = FALSE
	# )
	
	# do.call(rgrass::execGRASS, args=args)
	
	# ### connect database to vector
	# args <- list(
	# 	cmd = "v.db.connect",
	# 	map = srcTo,
	# 	driver = "sqlite",
	# 	database = grassDB,
	# 	table = srcTo,
	# 	flags = c("o", "quiet", "overwrite"),
	# 	intern = FALSE
	# )
	# do.call(rgrass::execGRASS, args=args)
	srcTo

}
