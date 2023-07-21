#" Minimum convex hull around a spatial vector
#"
#" Create a minimum convex hull around a spatial vector.
#"
#" @param x A `GVector`.
#" @param by Character: If `""` (default), then a convex hull is created for all geometries together. Otherwise, this is the name of a field in the vector. Hulls will be created for each set of geometries with the same value in this column.
#"
#" @return A `GVector`.
#"
#" @seealso [terra::convHull()], [sf::st_convex_hull()], module `v.hull` in **GRASS**
#"
#" @example man/examples/ex_convHull.r
#"
#" @aliases convHull
#" @rdname convHull
#" @exportMethod convHull
methods::setMethod(
	f = "convHull",
	signature = c(x = "GVector"),
	definition = function(x, by = "") {

	if (by == "") {
		
		gn <- .makeGName("convHull", "vector", 1L)
	
		args <- list(
			cmd = "v.hull",
			input = .gnames(x),
			output = gn,
			flags = c("quiet", "overwrite")
		)

		do.call(rgrass::execGRASS, args=args)
		out <- .makeGVector(gn)
		
	} else {
	
		df <- as.data.frame(x)
		uniques <- unique(df[ , by, drop=TRUE])
	
		n <- length(uniques)
		gns <- .makeGName("convHull", "vector", n)

		vects <- list()
		for (i in seq_len(n)) {

			uniq <- uniques[i]
			cats <- df$cat[df[ , by, drop = TRUE] == uniq]
			cats <- paste(cats, collapse=", ")

			# select
			args <- list(
				cmd = "v.hull",
				input = .gnames(x),
				output = gns[i],
				layer = layerName(x),
				flags = c("quiet", "overwrite"),
				cats = cats,
				intern = TRUE
			)
			
			do.call(rgrass::execGRASS, args=args)
			vects[[i]] <- .makeGVector(gns[i])
			
		} # next set
		
		# concatenate
		out <- do.call("c", vects)
	
	}
			
	out
		
	} # EOF
)

