#' Landscape fragmentation class following Riitters et al. (2020)
#'
#' @description Riitters et al. (2020) propose a classification scheme for forest fragmentation (which can be applied to any habitat type). The scheme relies on calculating density (e.g., number of forested cells in a window around a focal cell) and connectivity (number of cases where neighboring cells are both forested). These values are used to assign classes to each cell: "patch," "transitional," "perforated," "edge," and "interior" (plus an "undetermined" class for an edge case). To this, we also add a "none" class which occurs when the window has no cells with the focal habitat. This function calculates these classes from a `GRaster` or `SpatRaster` in which the focal habitat type has cell values of 1, and non-focal habitat type has cell values of 0 or `NA`.
#'
#' *Currently, this function only operates on a 3x3 window of cells.*
#'
#' @param x A `SpatRaster` or `GRaster`.
#'
#' @param undet Character: How to assign the "undetermined" case. Valid values are `"perforated"` (default), `"edge"`, and `"undetermined"`. Partial matching is used. If `Pf` is the proportional density raster cell value and `Pff` the proportional connectivity raster cell value, the undetermined case occurs when `Pf` > 0.6 and `Pf == Pff`.
#'
#' @param restrict Logical: If `TRUE`, then fragmentation will only be assigned to cells that have the focal habitat within them. If `FALSE` (default), cells surrounding the focal habitat class can also be assigned a fragmentation class other than 0, even if they do not have the focal habitat in them. Note that the case where `restrict = TRUE` is a departure from Riitters et al. (2020), but it allows you to create a series of fragmentation rasters for different habitat classes where cells with classes(other than 0) do not overlap.
#'
#' @param cores Integer: Number of processor cores to use for when processing a `SpatRaster`. This is only used if `mask` is `TRUE`. If you want to use multiple cores for processing a `GRaster`, set the `cores` option using `faster(cores = N)` (see [faster()]).
#'
#' @returns A categorical `SpatRaster` or `GRaster`. The values assigned to each class can be seen with [levels()].
#'
#' @references Riitters, K., J. Wickham, R. O'Neill, B. Jones, and E. Smith. 2000. Global-scale patterns of forest fragmentation. *Conservation Ecology* 4:3. URL: [http://www.consecol.org/vol4/iss2/art3/](http://www.consecol.org/vol4/iss2/art3/). Also note the [errata](https://www.ecologyandsociety.org/vol4/iss2/art3/errata/january26.2001.html).
#' 
#' @example man/examples/ex_fragmentation.r
#'
#' @aliases fragmentation
#' @rdname fragmentation
#' @exportMethod fragmentation
methods::setMethod(
	f = "fragmentation",
	signature = c(x = "SpatRaster"),
	function(x, undet = "perforated", restrict = FALSE, cores = faster("cores")) {

	undet <- omnibus::pmatchSafe(undet, c("undetermined", "perforated", "edge"), nmax = 1L)

	# assess if each pair of neighboring cell in cardinal direction has 1 or 2 forested cells or not (0 = no, 1 = 1 cell, 2 = 2 cells)
	window <- 3L
	ww <- matrix(0L, window, window)

	nLayers <- terra::nlyr(x)

	# list of cell pairs in cardinal directions
	cells <- list(
		c(1L, 2L),
		c(1L, 4L),
		c(4L, 5L),
		c(4L, 7L),
		c(7L, 8L),
		c(2L, 3L),
		c(2L, 5L),
		c(5L, 6L),
		c(5L, 8L),
		c(8L, 9L),
		c(3L, 6L),
		c(6L, 9L)
	)

	# for each layer
	for (i in seq_len(nLayers)) {

		xx <- x[[i]]

		unis <- unique(xx)
		if (length(unis) > 2L || (length(unis) == 1L & unis != 1L)) {
			stop("The raster must have values of 0 and 1, NA and 1,\n  or the values must be all NA, all 0, or all 1.")
		}

		connectivities <- list()
		for (j in seq_along(cells)) {
		
			pair <- cells[[j]]
			w <- ww
			w[pair] <- 1L

			connectivities[[j]] <- focal(xx, w = w, fun = 'sum', na.rm = TRUE)

		}

		# stack pff rasters
		pff_connected <- do.call(c, connectivities)

		# indicate if at least one cell of pair is forested
		pff_either_occ <- pff_connected >= 1L

		# convert to 1 if both cells are forested
		pff_connected <- pff_connected == 2L

		# sum forested neighbors across rasters
		pff_neighs <- sum(pff_connected)

		# sum either forested across rasters
		pff_forests <- sum(pff_either_occ)

		# calculate pff
		pff <- pff_neighs / pff_forests

		# calculate cover
		pf <- terra::focal(x, w = 3, "mean", na.rm = TRUE)

		### assign classes
		thisOut <- xx * 0L

		# pre-calculate interior mask
		interiorMask <- pf == 1 & pff == 1
		notInteriorMask <- 1L - interiorMask

		# pre-calculate difference
		diff <- pf - pff

		# patch
		mask <- pf > 0 & pf <= 0.4
		thisOut <- terra::mask(thisOut, mask, maskvalues = 1L, updatevalue = 1L)

		# transitional
		mask <- pf > 0.4 & pf <= 0.6
		thisOut <- terra::mask(thisOut, mask, maskvalues = 1L, updatevalue = 2L)

		# perforated
		if (undet == 'perforated') {
			mask <- pf > 0.6 & diff >= 0 & notInteriorMask
		} else if (undet %in% c('edge', 'undetermined')) {
			mask <- pf > 0.6 & diff > 0 & notInteriorMask
		}
		thisOut <- terra::mask(thisOut, mask, maskvalues = 1L, updatevalue = 3L)

		# edge
		if (undet == 'perforated') {
			mask <- pf > 0.6 & diff > 0 & notInteriorMask
		} else if (undet == 'edge') {
			mask <- pf > 0.6 & diff >= 0 & notInteriorMask
		}
		thisOut <- terra::mask(thisOut, mask, maskvalues = 1L, updatevalue = 4L)

		# undetermined
		if (undet == 'undetermined') {
			mask <- diff == 0 & notInteriorMask
			thisOut <- terra::mask(thisOut, mask, maskvalues = 1L, updatevalue = 5L)
		}

		# interior
		thisOut <- terra::mask(thisOut, interiorMask, maskvalues = 1L, updatevalue = 6L)

		# mask
		if (restrict) {
			xxMask <- terra::not.na(xx)
			thisOut <- thisOut * xxMask
		}

		if (undet %in% c("perforated", "edge")) {

			levs <- data.frame(value = c(0L, 1L, 2L, 3L, 4L, 6L), class = c("none", "patch", "transitional", "perforated", "edge", "interior"))

		} else if (undet == "undetermined") {
		
			levs <- data.frame(value = c(0L, 1L, 2L, 3L, 4L, 5L, 6L), class = c("none", "patch", "transitional", "perforated", "edge", "undetermined", "interior"))
		
		}
		levels(thisOut) <- levs

		if (i == 1L) {
			out <- thisOut
		} else {
			out <- c(out, thisOut)
		}

	} # next layer

	out

	} # EOF

)

#' @aliases fragmentation
#' @rdname fragmentation
#' @exportMethod fragmentation
methods::setMethod(
	f = "fragmentation",
	signature = c(x = "GRaster"),
	function(x, undet = "perforated", restrict = FALSE) {

	undet <- omnibus::pmatchSafe(undet, c("undetermined", "perforated", "edge"), nmax = 1L)

	mm <- minmax(x)
	msg <- "Raster must be binary (only 1 and/or 0, with NAs allowed."
	if (any(!(mm[1L, ] %in% c(0L, 1L))) | any(mm[2L, ] != 1L)) stop(msg)

	.locationRestore(x)
	.region(x)

	# assess if each pair of neighboring cell in cardinal direction has 1 or 2 forested cells or not (0 = no, 1 = 1 cell, 2 = 2 cells)
	window <- 3L
	ww <- matrix(0L, window, window)

	nLayers <- terra::nlyr(x)

	# list of offset pairs relative to focal cell in cardinal directions
	# first 2 values are offset from focal cell in rows/cols of 1st cell
	# second 2 values are offset from focal cell in rows/cols of 2nd cell
	offsets <- list(
		c(1L, 2L),
		c(1L, 4L),
		c(4L, 5L),
		c(4L, 7L),
		c(7L, 8L),
		c(2L, 3L),
		c(2L, 5L),
		c(5L, 6L),
		c(5L, 8L),
		c(8L, 9L),
		c(3L, 6L),
		c(6L, 9L)
	)

	# for each layer
	srcs <- rep(NA_character_, nLayers)
	for (i in seq_len(nLayers)) {

		### remove NAs
		srcIn <- sources(x)
		src <- .makeSourceName("r_mapcalc", "raster")
		ex <- paste0(src, " = if(isnull(", srcIn, "), 0, ", srcIn, ")")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		### connectivities (for Pff)
		srcConnectivities <- .makeSourceName("r_mapcalc", "raster", n = length(cells))
		srcEither <- .makeSourceName("r.mapcalc", "raster", n = length(offsets))
		srcBoth <- .makeSourceName("r.mapcalc", "raster", n = length(offsets))
		for (j in seq_along(offsets)) {

			# tally occurrences of habitat in each pair of cells		
			y1 <- cells[[i]][1L]
			x1 <- cells[[i]][2L]
			
			y2 <- cells[[i]][3L]
			x2 <- cells[[i]][4L]

			ex <- paste0(srcConnectivities[j], " = ", inSrc, "[", y, ",", x, "] + ", inSrc, "[", y, ",", x, "])")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

			# does either cell in a pair have habitat?
			ex <- paste0(srcEither[j], " = if(", srcConnectivities[j], " >= 1)")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

			# do both cells have habitat?
			ex <- paste0(srcBoth[j], " = if(", srcConnectivities[j], " == 2)")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		}

		# number of neighbor cell pairs with at least one with habitat
		srcNumEithers <- .makeSourceName("r_mapcapc", "raster")
		ex <- paste0(srcNumEithers, " = ", paste(srcBoth, collapse = " + "))
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# number of neighbor cell pairs both with habitat
		srcNumBoths <- .makeSourceName("r_mapcapc", "raster")
		ex <- paste0(srcNumBoths, " = ", paste(srcBoth, collapse = " + "))
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# calculate Pff
		srcPff <- .makeSourceName("r_mapcalc", "raster")
		ex <- paste0(srcPff, " = ", srcNumBoths, " / ", srcNumEithers)

		# calculate cover
		srcPf <- .makeSourceName("r_neighbors", "raster")
		rgrass::execGRASS(
			cmd = "r.neighbors",
			input = srcIn,
			output = srcPf,
			size = 3,
			method = "average",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite")
		)

		# calculate difference between pf and pff (used for "perforated", "edge", and "indeterminate" cases)
		srcDelta <- .makeSourceName("r.mapcalc", "raster")
		ex <- paste0(srcDelta, " = ", pfSrc, " - ", pffSrc)
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		### assign classes
		src <- .makeSourceName("r_mapcalc", "raster")
		
		if (indet == "indeterminate") {
		
			ex <- paste0(src, " = if (", pfSrc, " == 0, 0, if (", pfSrc, " == 1 & ", pffSrc, " == 1, 6, if (", pfSrc, " <= 0.4, 1, if (", pfSrc, " <= 0.6, 2, if (", srcDelta, " > 0, 3, if (", srcDelta, " < 0, 4, if (", srcDelta, ", 5, null())))))))")

		} else if (indet == "edge") {
		
			ex <- paste0(src, " = if (", pfSrc, " == 0, 0, if (", pfSrc, " == 1 & ", pffSrc, " == 1, 6, if (", pfSrc, " <= 0.4, 1, if (", pfSrc, " <= 0.6, 2, if (", srcDelta, " > 0, 3, if (", srcDelta, " <= 0, 4, null()))))))")

		} else if (indet == "perfortated") {
		
			ex <- paste0(src, " = if (", pfSrc, " == 0, 0, if (", pfSrc, " == 1 & ", pffSrc, " == 1, 6, if (", pfSrc, " <= 0.4, 1, if (", pfSrc, " <= 0.6, 2, if (", srcDelta, " >= 0, 3, if (", srcDelta, " < 0, 4, null()))))))")
		
		}

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# ex <- paste0(src, " = ",
		# 	"if (pf == 0, 0,
		# 		if (pf == 1 & pff == 1, 6,
		# 			if (pf <= 0.4, 1,
		# 				if (pf <= 0.6, 2,
		# 					if (pf - pff > 0, 3,
		# 						if (pf - pff < 0, 4,
		# 							if (pf == pff, 5, null())
		# 						)
		# 					)
		# 				)
		# 			)
		# 		)
		# 	)"
		# )

		# if masking to just
		if (restrict) {
		
			# make mask
			rgrass::execGRASS(
				cmd = "r.mask",
				raster = srcIn,
				flags = c(.quiet(), "overwrite")
			)

			# copy with mask in place
			src <- .copyGRaster(src, topo = topology(x)[i], reshapeRegion = FALSE)
		    
			# remove mask
			.removeMask()

		}

		srcs[i] <- src

	} # next layer

	if (undet %in% c("perforated", "edge")) {

		levs <- data.frame(value = c(0L, 1L, 2L, 3L, 4L, 6L), class = c("none", "patch", "transitional", "perforated", "edge", "interior"))

	} else if (undet == "undetermined") {
	
		levs <- data.frame(value = c(0L, 1L, 2L, 3L, 4L, 5L, 6L), class = c("none", "patch", "transitional", "perforated", "edge", "undetermined", "interior"))
	
	}

	levels <- list()
	for (i in seq_len(nLayers)) {
		levels[[i]] <- levs
	}

	.makeGRaster(srcs, levels = levels)

	} # EOF

)
