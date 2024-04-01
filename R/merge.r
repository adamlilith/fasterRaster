#' Combine two or more rasters with different extents and fill in NAs
#'
#' @description `merge()` combines two or more `GRaster`s, possibly with different extents, into a single larger `GRaster`. Where the same cell has different values in each raster, the value of the first raster's cell is used. If this is `NA`, then the value of the second raster's cell is used, and so on.
#'
#' @param x,y,... `GRaster`s.
#'
#' @returns A `GRaster`.
#' 
#' @seealso [terra::merge()], [terra::mosaic()], and **GRASS** module `r.patch`
#'
#' @example man/examples/ex_merge.r
#'
#' @aliases merge
#' @rdname merge
#' @exportMethod merge
methods::setMethod(
	f = "merge",
	signature = c(x = "GRaster", y = "GRaster"),
	definition = function(x, y, ...) {

	.locationRestore(x)
	compareGeom(x, y, lyrs=TRUE, ext=FALSE, rowcol=FALSE, depths=TRUE, res=TRUE, zres=TRUE)
	
	x <- list(x, y, ...)

	# set region to combined extent
	rasts <- paste(sapply(x, sources), collapse=",")
	rgrass::execGRASS("g.region", raster = rasts, flags=c("o", .quiet()))

	# combine
	src <- .makeSourceName("merge", "raster")
	rgrass::execGRASS(
		cmd = "r.patch",
		input = rasts,
		output = src,
		nprocs = faster("cores"),
		memory = faster("memory"),
		flags = c(.quiet(), "overwrite")
	)

	# combine levels
	if (!any(sapply(x, is.factor))) {
		levels <- NULL
	} else {

		cats <- sapply(x, cats)
		for (i in seq_along(cats)) {
			if (!inherits(cats[[i]], "data.table")) cats[[i]] <- data.table::as.data.table(cats)
		}

		levels <- data.table::data.table(NULL)
		i <- 1L
		while (nrow(levels) == 0L) {
			levels <- cats[[i]]
			ac1 <- activeCat(x[[i]])
			i <- i + 1L
		}
		ac1 <- names(levels)[ac1]

		for (j in i:length(x)) {
			nextCats <- cats[[j]]
			if (nrow(nextCats) > 0L) {
				cats2 <- cats[[j]]
				ac2 <- activeCat(x[[j]])
				ac2 <- names(cats2)[ac2]
				# levels <- merge(levels, cats2, by.x = ac1, by.y = ac2)
				levels <- merge(levels, cats2, all = TRUE)
			}
		}

	}
		
	.makeGRaster(src, "layer", levels = levels, ac = ac1)
	
	} # EOF
)
