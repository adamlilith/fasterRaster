#' Minimum convex hull around a spatial vector
#'
#' Create a minimum convex hull around a spatial vector.
#'
#' @param x A `GVector`.
#' @param by Character: If `""` (default), then a convex hull is created for all geometries together. Otherwise, this is the name of a field in the vector. Hulls will be created for each set of geometries with the same value in this column.
#'
#' @return A `GVector`.
#'
#' @seealso [terra::convHull()], [sf::st_convex_hull()], module `v.hull` in **GRASS**
#'
#' @example man/examples/ex_convHull.r
#'
#' @aliases convHull
#' @rdname convHull
#' @exportMethod convHull
methods::setMethod(
	f = "convHull",
	signature = c(x = "GVector"),
	definition = function(x, by = "") {

	if (by == "") {
		
		src <- .makeSourceName("v_hull", "vector")
	
		rgrass::execGRASS(
			cmd = "v.hull",
			input = sources(x),
			output = src,
			flags = c("quiet", "overwrite")
		)

		out <- .makeGVector(src)
		
	} else {
	
		byCol <- which(names(x) == by)
		bys <- as.data.table(x)[[by]]
		uniques <- unique(bys)
	
		n <- length(uniques)

		srcs <- .makeSourceName("convHull", "vector", n)
		vects <- list()
		for (i in seq_len(n)) {

			uniq <- uniques[i]
			selected <- which(bys == uniq)

			xx <- x[selected]

			if (ngeom(xx) < 3L) {

				warnings("Skipping ", uniq, " in column ", by, " because it has <3 locations.")

			} else {
					
				# select
				args <- list(
					cmd = "v.hull",
					input = sources(xx),
					output = srcs[i],
					flags = c("quiet", "overwrite"),
					intern = TRUE
				)
				
				do.call(rgrass::execGRASS, args=args)
				vects[[i]] <- .makeGVector(srcs[i])

			}
				
		} # next set
		
		# remove any NULL polygons
		vects <- Filter(Negate(is.null), vects)
		
		if (length(vects) == 0L) {
			out <- NULL
		} else {

			# concatenate
			args <- list(x = vects[[1L]])
			if (length(vects) > 1L) args <- c(args, list(vects[2:length(vects)]))
			out <- do.call("c", args)

		}
	
	}
	out
		
	} # EOF
)

