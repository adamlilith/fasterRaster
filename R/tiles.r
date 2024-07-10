#' Divide a GRaster into spatially exclusive subsets
#'
#' @description This function divides a raster into "tiles" or spatial subsets which can be used for speeding some raster calculations. Tiles can be mutually exclusive or overlap by a user-defined amount.
#'
#' @param x A `GRaster`.
#' @param n Numeric vector: Number of tiles to create. This can be a single number, in which case `x` is divided into `n` × `n` tiles, or two values in which case it is divided into `n[1]` × `n[2]` tiles (rows x columns).
#'
#' @param overlap Numeric vector (default is 0): Number of rows/columns by which to expand the size of tiles so they overlap. This can be a single value or two values. If just one is provided, the tiles will be expanded by `overlap` rows and `overlaps` columns. If two numbers are provided, the tiles will be expanded by `overlap[1]` rows and `overlap[2]` columns.
#'
#' @param verbose Logical: If `TRUE`, display progress. Default is `FALSE`.
#'
#' @returns A `list` with one element per tile. The [lapply()] and [sapply()] functions can be used to apply functions to each tile in the list.
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
	
	if (length(n) == 1L) n <- c(n, n)
	if (length(overlap) == 1L) overlap <- c(overlap, overlap)

	dims <- dim(x)
	if (all(overlap >= dims)) {
		warning("The number of overlap cells is >= than the number of rows and/or columns\n  in the input raster. The output is the same GRaster as the input.")
		return(x)
	}

	.locationRestore(x)
	.region(x)

	# number of columns/rows per tile (without expanding for overlap > 0)
	colsPerTile <- dims[2L] / n[2L]
	rowsPerTile <- dims[1L] / n[1L]
	
	cols <- c(1, seq(colsPerTile, dims[2L], by = colsPerTile))
	rows <- c(1, seq(rowsPerTile, dims[1L], by = rowsPerTile))

	cols <- round(cols)
	rows <- round(rows)

	nTiles <- length(cols) * length(rows)

	xmin <- W(x)
	xmax <- E(x)
	ymin <- S(x)
	ymax <- N(x)

	res <- res(x)
	xres <- res[1L]
	yres <- res[2L]

	nTiles <- prod(n)

	# create tiles
	if (verbose | faster("verbose")) {
		pb <- utils::txtProgressBar(min = 0, max = nTiles, initial = 0, style = 3, width = 30)
	}

	out <- vector(mode = "list", length = nTiles)
	i <- 1L
	for (row in seq_len(n[1L])) {
		for (col in seq_len(n[2L])) {

			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, i)

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
			
			west <- as.character(west)
			east <- as.character(east)
			south <- as.character(south)
			north <- as.character(north)

			rgrass::execGRASS(
				cmd = "g.region",
				n = north,
				s = south,
				e = east,
				w = west,
				rows = nrows,
				cols = ncols,
				flags= c("o", .quiet())
			)

			src <- .copyGSpatial(x, reshapeRegion = FALSE)
			out[[i]] <- .makeGRaster(src, names(x), levels = cats(x), ac = activeCats(x), fail = TRUE)

			i <- i + 1L

		} # next column

	} # next row

	if (verbose | faster("verbose")) close(pb)
	out
	
	} # EOF
)

