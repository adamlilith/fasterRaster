#' Landscape fragmentation class following Riitters et al. (2020)
#'
#' @description Riitters et al. (2020) propose a classification scheme for forest fragmentation (which can be applied to any habitat type). The scheme relies on calculating density (e.g., number of forested cells in a window around a focal cell) and connectivity (number of cases where neighboring cells are both forested). These values are used to assign classes to each cell: "patch," "transitional," "perforated," "edge," and "interior" (plus an optional "undetermined" class for an edge case). To this, we also add a "none" class which occurs when the focal cell's window has no cells with the focal habitat. This function calculates these classes from a `GRaster` or `SpatRaster` in which the focal habitat type has cell values of 1, and non-focal habitat type has cell values of 0 or `NA`.
#'
#' @param x A `SpatRaster` or `GRaster`.
#'
#' @param w An odd, positive integer: Size of the window across which fragmentation is calculated (in units of "rows" and "columns"). The default is 3, meaning the function uses a 3x3 moving window to calculate fragmentation. For large rasters, compute time is ~*O*(`N`) + *O*(`N * w^2`), where `N` is the number of cells in the raster. So, even a small increase in `w` can increase compute time by a lot.
#'
#' @param undet Character: How to assign the "undetermined" case. Valid values are `"perforated"` (default), `"edge"`, and `"undetermined"`. Partial matching is used. If `Pf` is the proportional density raster cell value and `Pff` the proportional connectivity raster cell value, the undetermined case occurs when `Pf` > 0.6 and `Pf == Pff`.
#'
#' @param restrict Logical: If `TRUE`, then fragmentation will only be assigned to cells that have the focal habitat within them. If `FALSE` (default), cells surrounding the focal habitat class can also be assigned a fragmentation class other than 0, even if they do not have the focal habitat in them. Note that the case where `restrict = TRUE` is a departure from Riitters et al. (2020).
#'
#' @param cores Integer: Number of processor cores to use for when processing a `SpatRaster`.
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
	function(x, w = 3, undet = "perforated", restrict = FALSE, cores = faster("cores")) {

	# errors?
	if (!omnibus::is.wholeNumber(w) || w < 1L || w %% 2 == 0) stop("Argument ", sQuote("w"), " must be an odd, positive integer.")
	undet <- omnibus::pmatchSafe(undet, c("undetermined", "perforated", "edge"), nmax = 1L)

	# setup
	nLayers <- terra::nlyr(x)

	# zeros matrix to store each set of neighbors
	ww <- matrix(0L, w, w)

	# assess if each pair of neighboring cell in cardinal direction has 1 or 2 forested cells or not (0 = no, 1 = 1 cell, 2 = 2 cells)
	# 1st make a list of possible cell neighbors by cell index
	# 2nd assess if neighbors both have target habitat type
	cells <- list()
	w1 <- matrix(1L:(w^2), nrow = w, ncol = w)
	w2 <- w1[1L:(w - 1), 1L:(w - 1)]

	for (i in 1L:(w - 1L)^2) {
			
		len <- length(cells)
		wFrom <- w2[i]
		
		# left-to-right neighbors
		cells[[len + 1L]] <- c(wFrom, wFrom + w)

		# top-to-bottom neighbors
		cells[[len + 2L]] <- c(wFrom, wFrom + 1L)

	}

	# add left-to-right neighbors of bottom row
	for (i in w1[w, 1L:(w - 1L)]) {
	
		len <- length(cells)
		wFrom <- w1[i]
		cells[[len + 1L]] <- c(wFrom, wFrom + w)

	}

	# add top-to-bottom neighbors of right column
	for (i in w1[1L:(w - 1L), w]) {
	
		len <- length(cells)
		wFrom <- w1[i]
		cells[[len + 1L]] <- c(wFrom, wFrom + 1L)

	}

	# for each layer
	for (i in seq_len(nLayers)) {

		xx <- x[[i]]
		xx <- terra::app(xx, fun = function(x) ifelse(is.na(x), 0L, x), cores = cores)

		unis <- unique(xx)
		unis <- unis[ , 1L]
		if (length(unis) != 2L) {
			stop("The raster must have values of 0 and 1, NA and 1,\n  or the values must be all NA, all 0, or all 1.")
		}

		connectivities <- list()
		for (j in seq_along(cells)) {
		
			pair <- cells[[j]]
			www <- ww
			www[pair] <- 1L

			connectivities[[j]] <- focal(xx, w = www, fun = 'sum', na.rm = TRUE)

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
		pf <- terra::focal(xx, w = w, "mean", na.rm = TRUE)

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
		} else {
			mask <- pf > 0.6 & diff > 0 & notInteriorMask
		}
		thisOut <- terra::mask(thisOut, mask, maskvalues = 1L, updatevalue = 3L)

		# edge
		if (undet == 'edge') {
			mask <- pf > 0.6 & diff <= 0 & notInteriorMask
		} else {
			mask <- pf > 0.6 & diff < 0 & notInteriorMask
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
	function(x, w = 3, undet = "perforated", restrict = FALSE) {

	undet <- omnibus::pmatchSafe(undet, c("perforated", "edge", "undetermined"), nmax = 1L)

	mm <- minmax(x)
	msg <- "Raster must be binary (only 1 and/or 0, with NAs allowed."
	if (any(!(mm[1L, ] %in% c(0L, 1L))) | any(mm[2L, ] != 1L)) stop(msg)

	# force to integer to obviate issues with floating-point precision
	dtype <- datatype(x)
	if (any(dtype != "integer")) {
		nonIntegers <- which(dtype != "integer")
		for (i in nonIntegers) {
			x[[i]] <- as.int(x[[i]])
		}
	}

	# setup
	.locationRestore(x)
	.region(x)

	nLayers <- terra::nlyr(x)

	# assess if each pair of neighboring cell in cardinal direction has 1 or 2 forested cells or not (0 = no, 1 = 1 cell, 2 = 2 cells)
	ww <- matrix(0L, w, w)

	#### calculate connectivities for Pff

	# list of offset pairs relative to focal cell in cardinal directions
	offsets <- list()
	center <- (w + 1L) / 2L
	
	# get left-right cell pairs referenced by offset from window center
	i <- 1L
	for (row in seq_len(w)) {
		for (col in 1L:(w - 1L)) {

			offsets[[i]] <- c(row - center, col - center, row - center, col - center + 1L)
			i <- i + 1L

		}
	}

	# get up-down cell pairs referenced by offset from window center
	i <- length(offsets) + 1L
	for (row in 1L:(w - 1L)) {
		for (col in seq_len(w)) {

			offsets[[i]] <- c(row - center, col - center, row - center + 1L, col - center)
			i <- i + 1L

		}
	}

	# for each layer
	srcs <- rep(NA_character_, nLayers)
	for (i in seq_len(nLayers)) {

		### remove NAs
		srcIn <- sources(x)
		src <- .makeSourceName("fragmentation_x_noNAs", "raster")
		ex <- paste0(src, " = if(isnull(", srcIn, "), 0, ", srcIn, ")")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		### connectivities (for Pff)
		srcConnectivities <- .makeSourceName("fragmentation_connectivities", "raster", n = length(offsets))
		srcEither <- .makeSourceName("fragmentation_either", "raster", n = length(offsets))
		srcBoth <- .makeSourceName("fragmentation_both", n = length(offsets))
		for (j in seq_along(offsets)) {

			# tally occurrences of habitat in each pair of cells		
			y1 <- offsets[[j]][1L]
			x1 <- offsets[[j]][2L]
			
			y2 <- offsets[[j]][3L]
			x2 <- offsets[[j]][4L]

			ex <- paste0(srcConnectivities[j], " = ", src, "[", y1, ",", x1, "] + ", src, "[", y2, ",", x2, "]")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

			# does either cell in a pair have habitat?
			ex <- paste0(srcEither[j], " = if(", srcConnectivities[j], " >= 1)")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

			# do both cells have habitat?
			ex <- paste0(srcBoth[j], " = if(", srcConnectivities[j], " == 2)")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		}

		# number of neighbor cell pairs with at least one with habitat
		srcNumEithers <- .makeSourceName("fragmentation_sum_eithers", "raster")
		ex <- paste0(srcNumEithers, " = ", paste(srcEither, collapse = " + "))
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# number of neighbor cell pairs both with habitat
		srcNumBoths <- .makeSourceName("fragmentation_sum_both", "raster")
		ex <- paste0(srcNumBoths, " = ", paste(srcBoth, collapse = " + "))
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# calculate Pff
		srcPff <- .makeSourceName("fragmentation_pff", "raster")
		ex <- paste0(srcPff, " = ", faster("rasterPrecision"), "(", srcNumBoths, ") / ", faster("rasterPrecision"), "(", srcNumEithers, ")")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# calculate cover (Pf)
		srcPf <- .makeSourceName("fragmentation_pf", "raster")
		rgrass::execGRASS(
			cmd = "r.neighbors",
			input = src,
			output = srcPf,
			size = w,
			method = "average",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite")
		)

		# calculate difference between pf and pff (used for "perforated", "edge", and "indeterminate" cases)
		srcDelta <- .makeSourceName("fragmentation_delta", "raster")
		ex <- paste0(srcDelta, " = ", srcPf, " - ", srcPff)
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		### assign classes
		src <- .makeSourceName("fragmentation_class", "raster")
		
		if (undet == "indeterminate") {
		
			ex <- paste0(src, " = if (", srcPf, " == 0, 0, if (", srcPf, " == 1 & ", srcPff, " == 1, 6, if (", srcPf, " <= 0.4, 1, if (", srcPf, " <= 0.6, 2, if (", srcDelta, " > 0, 3, if (", srcDelta, " < 0, 4, if (", srcDelta, ", 5, null())))))))")

		} else if (undet == "edge") {
		
			ex <- paste0(src, " = if (", srcPf, " == 0, 0, if (", srcPf, " == 1 & ", srcPff, " == 1, 6, if (", srcPf, " <= 0.4, 1, if (", srcPf, " <= 0.6, 2, if (", srcDelta, " > 0, 3, if (", srcDelta, " <= 0, 4, null()))))))")

		} else if (undet == "perforated") {
		
			ex <- paste0(src, " = if (", srcPf, " == 0, 0, if (", srcPf, " == 1 & ", srcPff, " == 1, 6, if (", srcPf, " <= 0.4, 1, if (", srcPf, " <= 0.6, 2, if (", srcDelta, " >= 0, 3, if (", srcDelta, " < 0, 4, null()))))))")
		
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

		# if masking to just focal habitat
		if (restrict) {

			# multiply by focal habitat (1)/non-focal habitat (0 or NA)
			srcMask <- .makeSourceName("fragmentation_mask", "raster")
			ex <- paste0(srcMask, " = if (", srcIn, " == 1, 1, null())")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

			srcIn <- src
			src <- .makeSourceName("fragmentation_class_masked")
			ex <- paste0(src, " = ", srcIn, " * ", srcMask)
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
			
			# make mask, copy with mask in place, remove mask
			# # rgrass::execGRASS(cmd = "r.mask", raster = srcIn, flags = c(.quiet(), "overwrite"))
			# # src <- .copyGRaster(src, topo = topology(x)[i], reshapeRegion = FALSE)
			# # .removeMask()

		}# else {
		
		# 	# mask to non-NA cells in input
		# 	srcIn <- src
		# 	src <- .makeSourceName("fragmentation_mask_NAs", "raster")
		# 	ex <- paste0(src, " = if(isnull(", sources(x)[i], "), null(), ", srcIn, ")")
		# 	rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# }

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
