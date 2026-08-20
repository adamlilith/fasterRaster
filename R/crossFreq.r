#' Frequencies of combinations of cells across two or more rasters
#'
#' @description `crossFreq()` tabulates the number of cells across two or more rasters for each combination of values in the rasters. Only cells that are not `NA` across all rasters will be used.
#'
#' @param x A stack of integer/categorical `GRaster`s.
#' @param na.rm Logical: If `TRUE` (default), then only cells that are not `NA` across all rasters will be used. If `FALSE`, then for each pair of `GRaster`s, all cells that are not `NA` in both rasters will be used.
#' @param cats Logical: If `TRUE` (default), then replace the values of categorical rasters with their category names in the output.
#'
#' @returns A `data.frame` or a named `list` of `data.frame`s, one per each pair of rasters in `x`.
#'
#' @seealso [freq()], **GRASS** tool `r.stats` (see `grassHelp("r.stats")`)
#'
#' @example man/examples/ex_freq_crossFreq.r
#'
#' @aliases crossFreq
#' @rdname crossFreq
#' @exportMethod crossFreq
methods::setMethod(
	f = "crossFreq",
	signature = c(x = "GRaster"),
	definition = function(x, na.rm = TRUE, cats = TRUE) {

	dtype <- datatype(x, type = "GRASS")
	if (any(dtype != "CELL")) stop("Only integer data types are supported.")
		
	nLayers <- nlyr(x)

	# mask to non-NA cells
	if (na.rm) {
			
		naMask <- maskNA(x, value = 0)
		naMask <- sum(naMask)
		naMask <- as.int(naMask)
		nms <- names(x)
		acs <- activeCats(x)
		levs <- levels(x)
		x <- x + naMask
		names(x) <- nms
		levels(x) <- levs
		for (i in 1L:nLayers) if (is.factor(x[[i]])) activeCat(x, layer = i) <- acs[i]

	}

	# calculate cross-frequencies for each pair of rasters
	out <- list()
	for (i in 1L:(nLayers - 1L)) {

		for (j in (i + 1L):nLayers) {

			inSrc <- c(sources(x)[i], sources(x)[j])

			args <- list(
				cmd = "r.stats",
				input = inSrc,
				separator = "pipe",
				flags = c("c", "n", .quiet()),
				intern = TRUE
			)
			
			data <- do.call(rgrass::execGRASS, args = args)
			
			bads <- which(grepl(data, pattern = "\b"))
			if (length(bads) > 0L) data <- data[-bads]
		
			data <- strsplit(data, split = "\\|")

			xFreqs <- do.call(rbind, data)
			xFreqs <- data.table::as.data.table(xFreqs)
			names(xFreqs) <- c(names(x)[i], names(x)[j], "count")

			xFreqs[ , c(names(x)[i], names(x)[j], "count") := lapply(.SD, as.numeric), .SDcols = c(names(x)[i], names(x)[j], "count")]

			### if any counts are <0, use brute force method to mask these cells and count them
			# We have to do this because of a bug in r.stats reported on https://github.com/OSGeo/grass/issues/7769 where an integer overflows on Windows machines

			if (any(xFreqs$count < 0)) {

				iSrc <- sources(x)[i]
				jSrc <- sources(x)[j]

				badCounts <- which(xFreqs$count < 0)

				for (k in badCounts) {

					iValue <- xFreqs[[1L]][k]
					jValue <- xFreqs[[2L]][k]

					src <- .makeSourceName("r_mapcalc", type = "raster")
					ex <- paste0(src, " = if((", iSrc, " == ", iValue, ") && (", jSrc, " == ", jValue, "), int(1), null())")

					# mask to just cells that have the given values in both rasters
					rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)

					# count
					args <- list(
						cmd = "r.univar",
						map = src,
						nprocs = faster("cores"),
						flags = c("r", .quiet()),
						Sys_show.output.on.console = FALSE,
						echoCmd = FALSE,
						intern = TRUE
					)

					thisInfo <- do.call(rgrass::execGRASS, args = args)
					.rm(src, type = "raster", warn = FALSE, verify = FALSE)

					pattern <- "sum: "
					thisCount <- thisInfo[grepl(thisInfo, pattern = pattern)]
					thisCount <- sub(thisCount, pattern = pattern, replacement = "")
					thisCount <- as.numeric(thisCount)

					# count these
					if (thisCount < 0) stop("Error: count of cells with values ", iValue, " and ", jValue, " is < 0. This is likely a bug in GRASS GIS. Please report this to the package maintainer.")
					xFreqs$count[k] <- thisCount
			
				}

			} # if any counts < 0

			# replace values with category names
			if (cats) {

				# add categories for 1st raster if it is a categorical raster
				if (is.factor(x[[i]])) {

					focalRastCol <- which(names(xFreqs) == names(x)[i])

					ac <- activeCat(x, layer = i)
					vals <- levels(x)[[i]][[1L]]
					rastCats <- levels(x)[[i]][[ac + 1L]]
					
					rastCats <- rastCats[match(xFreqs[[focalRastCol]], vals)]

					rastCats <- data.table::data.table(TEMP = rastCats)
					names(rastCats) <- names(x)[i]

					xFreqs <- cbind(rastCats, xFreqs[ , c(2L, 3L)])

				}

				# add categories for 2nd raster if it is a categorical raster
				if (is.factor(x[[j]])) {

					focalRastCol <- which(names(xFreqs) == names(x)[j])

					ac <- activeCat(x, layer = j)
					vals <- levels(x)[[j]][[1L]]
					rastCats <- levels(x)[[j]][[ac + 1L]]
					
					rastCats <- rastCats[match(xFreqs[[focalRastCol]], vals)]

					rastCats <- data.table::data.table(TEMP = rastCats)
					names(rastCats) <- names(x)[j]

					xFreqs <- cbind(xFreqs[ , 1L], rastCats, xFreqs[ , 3L])

				}

			} # replace values with category names

			out <- c(out, list(xFreqs))
			names(out)[length(out)] <- paste0(names(x)[i], '_vs_', names(x)[j])

		} # next second raster

	}

	if (!faster('useDataTable')) out <- lapply(out, as.data.frame)
	if (nlyr(x) == 2L) out <- out[[1L]]
	out

	} # #OF

)
