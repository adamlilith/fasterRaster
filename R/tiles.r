#' Divide a GRaster into spatially exclusive subsets
#'
#' @description This function divides a `GRaster` into "tiles" or spatial subsets which can be used for speeding some raster calculations. Tiles can be mutually exclusive or overlap by a user-defined number of cells.
#'
#' @param x A `GRaster` with one or more layers.
#'
#' @param n Numeric vector: Number of tiles to create. This can be a single number, in which case `x` is divided into `n` × `n` tiles, or two values in which case it is divided into `n[1]` × `n[2]` tiles (rows x columns).
#'
#' @param overlap Numeric vector (default is 0): Number of rows/columns by which to expand the size of tiles so they overlap. This can be a single value or two values. If just one is provided, the tiles will be expanded by `overlap` rows and `overlaps` columns. If two numbers are provided, the tiles will be expanded by `overlap[1]` rows and `overlap[2]` columns.
#'
#' @param verbose Logical: If `TRUE`, display progress. Default is `FALSE`. Progress is only displayed if `x` is a multi-layer `GRaster`.
#'
#' @returns If `x` has just one layer, then the output is a `list` with one element per tile. The [lapply()] and [sapply()] functions can be used to apply functions to each tile in the list. If `x` has more than one layer, then the output will be a `list` of `list`s, with each sub-`list` containing the tiles for one `GRaster` layer.
#'
#' @example man/examples/ex_tiles.r
#'
#' @aliases tiles
#' @rdname tiles
#' @exportMethod tiles
methods::setMethod(
	f = "tiles",
	signature = c("GRaster"),
	function(x, n, overlap = 0, verbose = FALSE) {

	.locationRestore(x)
	.region(x)

	dims <- dim(x)
	extent <- ext(x)
	res <- res(x)

	out <- list()

	### multiple layers
	if (nlyr(x) > 1L) {

		# create tiles
		if (verbose | faster("verbose")) {
			pb <- utils::txtProgressBar(min = 0, max = nlyr(x), initial = 0, style = 3, width = 30)
		}

		for (i in 1:nlyr(x)) {

			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, i)

			srcs <- .tiles(x = x[[i]], n = n, dims = dims, extent = extent, res = res, overlap = 0)

			out[[i]] <- list()
			for (j in seq_along(srcs)) {

				out[[i]][[j]] <- .makeGRaster(srcs[j], names(x)[i], levels = cats(x, layer = i), ac = activeCat(x, layer = i), fail = TRUE)

			} # next tile
		} # next layer

		names(out) <- names(x)
		if (verbose | faster("verbose")) close(pb)

	### single layer
	} else {

		srcs <- .tiles(x = x, n = n, dims = dims, extent = extent, res = res, overlap = 0)
		for (i in seq_along(srcs)) {

			out[[i]] <- .makeGRaster(srcs[i], names(x), levels = cats(x), ac = activeCat(x), fail = TRUE)

		} # next tile

	}
	out
	
	} # EOF
)

#' Break a raster into tiles
#' @param x The [sources()] name of a **GRASS** raster.
#' @param n Integer/numeric: Number of tiles. One or two values.
#' @param dims [dim()] of `x`.
#' @param extent A `SpatExtent` object or a vector of four values describing the extent of `x` (WESN).
#' @param res Numeric vector with two values: size of a cell in the x-dimension and the y-dimension in map units.
#' @param overlap Number of rows/columns by which to overlap tiles.
#'
#' @returns Character vector: The [sources()] names of the tiles. Each has a `list` attribute with its extent and dimensions.
#' 
#' @noRd
.tiles <- function(x, n, dims, extent, res, overlap = 0) {

	if (length(n) == 1L) n <- c(n, n)
	if (length(overlap) == 1L) overlap <- c(overlap, overlap)

	if (all(overlap >= dims)) {
		warning("The number of overlap cells is >= than the number of rows and/or columns\n  in the input raster. The output is the same GRaster as the input.")
		return(x)
	}

	# number of columns/rows per tile (without expanding for overlap > 0)
	colsPerTile <- dims[2L] / n[2L]
	rowsPerTile <- dims[1L] / n[1L]
	
	cols <- c(1, seq(colsPerTile, dims[2L], by = colsPerTile))
	rows <- c(1, seq(rowsPerTile, dims[1L], by = rowsPerTile))

	cols <- round(cols)
	rows <- round(rows)

	nTiles <- length(cols) * length(rows)

	if (inherits(extent, "SpatExtent")) extent <- as.vector(extent)
	xmin <- extent[1L]
	xmax <- extent[2L]
	ymin <- extent[3L]
	ymax <- extent[4L]

	xres <- res[1L]
	yres <- res[2L]

	nTiles <- prod(n)

	out <- rep(NA_character_, nTiles) # vector storing `sources` names of raster tiles
	i <- 1L
	spatial <- list() # hold spatial attributes, one element per tile
	for (row in seq_len(n[1L])) {
		for (col in seq_len(n[2L])) {

			# start/end rows/cols
			startRow <- rows[row]
			endRow <- rows[row + 1]

			startCol <- cols[col]
			endCol <- cols[col + 1]

			# extent
			west <- xmin + startCol * xres - ifelse(startCol == 1L, xres, 0)
			east <- xmin + endCol * xres

			north <- ymax - startRow * yres + ifelse(startRow == 1L, yres, 0)
			south <- ymax - endRow * yres

			nrows <- rows[row + 1L] - rows[row] + 1L
			ncols <- cols[col + 1L] - cols[col] + 1L

			# add rows
			if (overlap[1] > 0) {
			
				delta <- overlap[1L] * yres
				if (south > ymin) {

					newBorder <- south - delta
					newBorder <- max(newBorder, ymin)
					newRows <- (south - newBorder) / yres
					nrows <- nrows + newRows
					south <- newBorder

				}
			
				if (north < ymax) {

					newBorder <- north + delta
					newBorder <- min(newBorder, ymax)
					newRows <- (newBorder - north) / yres
					nrows <- nrows + newRows
					north <- newBorder
					
				}

			}

			# add columns
			if (overlap[2] > 0) {
			
				delta <- overlap[2L] * xres
				if (west > xmin) {

					newBorder <- west - delta
					newBorder <- max(newBorder, xmin)
					newCols <- (west - newBorder) / xres
					ncols <- ncols + newCols
					west <- newBorder

				}
			
				if (east < xmax) {

					newBorder <- east + delta
					newBorder <- min(newBorder, xmax)
					newCols <- (newBorder - east) / xres
					ncols <- ncols + newCols
					east <- newBorder
					
				}

			
			}

			# obviating floating-point error
			nrows <- as.integer(nrows)
			ncols <- as.integer(ncols)
			
			westChar <- as.character(west)
			eastChar <- as.character(east)
			southChar <- as.character(south)
			northChar <- as.character(north)

			rgrass::execGRASS(
				cmd = "g.region",
				n = northChar,
				s = southChar,
				e = eastChar,
				w = westChar,
				rows = nrows,
				cols = ncols,
				flags= c("o", .quiet())
			)

			out[i] <- .copyGSpatial(x, reshapeRegion = FALSE)
			spatial[[i]] <- list(
				extent = c(west, east, south, north),
				dim = c(nrows, ncols)
			)
			i <- i + 1L

		} # next column

	} # next row
	attr(out, "spatial") <- spatial
	out

}
