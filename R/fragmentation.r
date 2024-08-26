#' Landscape fragmentation class following Riitters et al. (2020)
#'
#' @description Riitters et al. (2020) propose a classification scheme for forest fragmentation (which can be applied to any habitat type). The scheme relies on calculating density (e.g., number of forested cells in a window around a focal cell) and connectivity (number of cases where neighboring cells are both forested). This function calculates these classes from a `GRaster` or `SpatRaster` in which the focal habitat type has cell values of 1, and non-focal habitat type has cell values of 0 or `NA`.
#'
#' Note that by default, the `SpatRaster` and `GRaster` versions will create different results around the border of the raster. The `SpatRaster` version uses the [terra::focal()] function, which will not return an `NA` value when its window overlaps the raster border if the `na.rm` argument is `TRUE`. However, the `GRaster` version uses the **GRASS** module `r.neighbors`, which does return `NA` values in these cases.
#'
#' The fragmentation classes are:
#' * Value provided by `none`: None (i.e., no forest; default is `NA`).
#' * 1: Patch
#' * 2: Transitional
#' * 3: Perforated
#' * 4: Edge
#' * 5: Undetermined (not possible to obtain when `w = 3`)
#' * 6: Interior
#'
#' @param x A `SpatRaster` or `GRaster`.
#'
#' @param w An odd, positive integer: Size of the window across which fragmentation is calculated (in units of "rows" and "columns"). The default is 3, meaning the function uses a 3x3 moving window to calculate fragmentation. For large rasters, compute time is ~*O*(`N`) + *O*(`N * w^2`), where `N` is the number of cells in the raster. So, even a small increase in `w` can increase compute time by a lot.
#'
#' @param undet Character: How to assign the "undetermined" case. Valid values are `"perforated"` (default), `"edge"`, and `"undetermined"`. Partial matching is used. If `Pf` is the proportional density raster cell value and `Pff` the proportional connectivity raster cell value, the undetermined case occurs when `Pf` > 0.6 and `Pf == Pff`.
#'
#' @param none Integer or `NA` (default): Value to assign to a cell with no focal habitat. Riitters et al. use `NA`. This will be forced to an integer if it is not an actual integer.
#'
#' @param na.rm Logical: If `TRUE` (default) and `x` is a `SpatRaster`, then cells near the edge of the raster where the window overlaps the edge can still be assigned a fragmentation class. If `FALSE`, these cells will be assigned a value of `none`.
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
	function(x, w = 3, undet = "undetermined", none = NA, na.rm = TRUE, cores = faster("cores"), verbose = TRUE) {

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
	nSteps <- nLayers * (1 + length(cells) + 17)
	if (verbose | faster("verbose")) {
		pb <- utils::txtProgressBar(min = 0, max = nSteps, initial = 0, style = 3, width = 30)
	}
	steps <- 0

	for (i in seq_len(nLayers)) {

		xx <- x[[i]]

		# convert NAs to 0s
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		# xx <- terra::app(xx, fun = function(x) ifelse(is.na(x), 0L, x), cores = cores) # VERY slow!!!
		xx[is.na(x)] <- 0L

		unis <- unique(xx)
		unis <- unis[ , 1L]
		if (length(unis) != 2L) {
			stop("The raster must have values of 0 and 1, NA and 1,\n  or the values must be all NA, all 0, or all 1.")
		}

		connectivities <- list()
		for (j in seq_along(cells)) {

			steps <- steps + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		
			pair <- cells[[j]]
			www <- ww
			www[pair] <- 1L

			connectivities[[j]] <- focal(xx, w = www, fun = "sum", na.rm = TRUE)

		}

		# stack pff rasters
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		pffConnected <- do.call(c, connectivities)

		# indicate if at least one cell of pair is forested
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		pffEither <- pffConnected >= 1L

		# convert to 1 if both cells are forested
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		pffConnected <- pffConnected == 2L

		# sum forested neighbors across rasters
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		pffNeighs <- sum(pffConnected)

		# sum either forested across rasters
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		pffForests <- sum(pffEither)

		# calculate pff
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		pff <- pffNeighs / pffForests

		# calculate cover
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		pf <- terra::focal(xx, w = w, "mean", na.rm = na.rm)

		### assign classes
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		thisOut <- xx * 0L

		# pre-calculate interior mask
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		interiorMask <- pf == 1 & pff == 1
		notInteriorMask <- 1L - interiorMask

		# pre-calculate difference
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		delta <- pf - pff

		# patch
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		mask <- pf > 0 & pf <= 0.4
		thisOut <- terra::mask(thisOut, mask, maskvalues = 1L, updatevalue = 1L)

		# transitional
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		mask <- pf > 0.4 & pf <= 0.6
		thisOut <- terra::mask(thisOut, mask, maskvalues = 1L, updatevalue = 2L)

		# perforated
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		if (undet == 'perforated') {
			mask <- pf > 0.6 & delta >= 0
		} else {
			mask <- pf > 0.6 & delta > 0
		}
		thisOut <- terra::mask(thisOut, mask, maskvalues = 1L, updatevalue = 3L)

		# edge
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		if (undet == 'edge') {
			mask <- pf > 0.6 & delta <= 0
		} else {
			mask <- pf > 0.6 & delta < 0
		}
		thisOut <- terra::mask(thisOut, mask, maskvalues = 1L, updatevalue = 4L)

		# undetermined
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		if (undet == 'undetermined') {
			mask <- pf > 0.6 & delta == 0
			thisOut <- terra::mask(thisOut, mask, maskvalues = 1L, updatevalue = 5L)
		}

		# interior
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		thisOut <- terra::mask(thisOut, interiorMask, maskvalues = 1L, updatevalue = 6L)

		# no focal habitat in focal cell
		# Riitters et al.: "If the center pixel was not forest, then a null value was assigned to that location."
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		thisOut <- terra::mask(thisOut, xx, maskvalues = 0L, updatevalue = none)

		levs <- .fragmentationLevels(undet = undet, none = none)
		levels(thisOut) <- levs

		if (i == 1L) {
			out <- thisOut
		} else {
			out <- c(out, thisOut)
		}

	} # next layer
	if (verbose | faster("verbose")) close(pb)

	out

	} # EOF

)

#' @aliases fragmentation
#' @rdname fragmentation
#' @exportMethod fragmentation
methods::setMethod(
	f = "fragmentation",
	signature = c(x = "GRaster"),
	function(x, w = 3, undet = "undetermined", none = NA, verbose = TRUE) {

	undet <- omnibus::pmatchSafe(undet, c("perforated", "edge", "undetermined"), nmax = 1L)

	mm <- minmax(x)
	msg <- "Raster must be binary (only 1 and/or 0, with NAs allowed."
	if (any(!(mm %in% c(0L, 1L)))) stop(msg)

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
	nSteps <- nLayers * (1 + length(offsets) + 6)
	if (verbose | faster("verbose")) {
		pb <- utils::txtProgressBar(min = 0, max = nSteps, initial = 0, style = 3, width = 30)
	}
	steps <- 0

	# to keep `sources` of temporary files for deletion
	created <- character()

	srcs <- rep(NA_character_, nLayers)
	for (i in seq_len(nLayers)) {

		### remove NAs
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		srcIn <- sources(x)[i]
		srcXZeros <- .makeSourceName("fragmentation_xSansNAs", "raster")
		ex <- paste0(srcXZeros, " = if(isnull(", srcIn, "), 0, ", srcIn, ")")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		created <- c(created, srcXZeros)

		### connectivities (for Pff)
		srcConnectivities <- .makeSourceName("fragmentation_connectivities", "raster", n = length(offsets))
		srcEither <- .makeSourceName("fragmentation_either", "raster", n = length(offsets))
		srcBoth <- .makeSourceName("fragmentation_both", n = length(offsets))
		for (j in seq_along(offsets)) {

			steps <- steps + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)

			# tally occurrences of habitat in each pair of cells		
			y1 <- offsets[[j]][1L]
			x1 <- offsets[[j]][2L]
			
			y2 <- offsets[[j]][3L]
			x2 <- offsets[[j]][4L]

			# sum of pairs of cells
			ex <- paste0(srcConnectivities[j], " = ", srcXZeros, "[", y1, ",", x1, "] + ", srcXZeros, "[", y2, ",", x2, "]")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

			# does either cell in a pair have habitat?
			ex <- paste0(srcEither[j], " = if(", srcConnectivities[j], " >= 1)")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

			# do both cells have habitat?
			ex <- paste0(srcBoth[j], " = if(", srcConnectivities[j], " == 2)")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		}
		created <- c(created, srcConnectivities, srcEither, srcBoth)


		# number of neighbor cell pairs with at least one with habitat
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		srcNumEithers <- .makeSourceName("fragmentation_sum_eithers", "raster")
		ex <- paste0(srcNumEithers, " = ", paste(srcEither, collapse = " + "))
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		created <- c(created, srcNumEithers)

		# number of neighbor cell pairs both with habitat
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		srcNumBoths <- .makeSourceName("fragmentation_sum_both", "raster")
		ex <- paste0(srcNumBoths, " = ", paste(srcBoth, collapse = " + "))
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		created <- c(created, srcNumBoths)

		# calculate Pff
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		srcPff <- .makeSourceName("fragmentation_pff", "raster")
		ex <- paste0(srcPff, " = double(", srcNumBoths, ") / double(", srcNumEithers, ")")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		created <- c(created, srcPff)

		# calculate cover (Pf)
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		srcPf <- .makeSourceName("fragmentation_pf", "raster")
		rgrass::execGRASS(
			cmd = "r.neighbors",
			input = srcXZeros,
			output = srcPf,
			size = w,
			method = "average",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite")
		)

		created <- c(created, srcPf)

		# calculate difference between Pf and Pff (used for "perforated", "edge", and "undetermined" cases)
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
		srcDelta <- .makeSourceName("fragmentation_delta", "raster")
		ex <- paste0(srcDelta, " = ", srcPf, " - ", srcPff)
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		created <- c(created, srcDelta)

		### assign classes
		# NB: The first condition in each forces cases where focal cell does not have focal habitat to NA as per Riitters et al.: "If the center pixel was not forest, then a null value was assigned to that location."
		src <- .makeSourceName("fragmentation_class", "raster")
		
		if (is.na(none)) {
			noneValue <- "null()"
		} else {
			noneValue <- as.int(none)
		}

		if (undet == "undetermined") {
		
			ex <- paste0(src, " = if (", srcXZeros, " == 0, ", noneValue, ", if (", srcPf, " == 0, 0, if (", srcPf, " == 1 & ", srcPff, " == 1, 6, if (", srcPf, " <= 0.4, 1, if (", srcPf, " <= 0.6, 2, if (", srcDelta, " > 0, 3, if (", srcDelta, " < 0, 4, if (", srcDelta, " == 0, 5, null()))))))))")

		} else if (undet == "edge") {
		
			ex <- paste0(src, " = if (", srcXZeros, " == 0, ", noneValue, ", if (", srcPf, " == 0, 0, if (", srcPf, " == 1 & ", srcPff, " == 1, 6, if (", srcPf, " <= 0.4, 1, if (", srcPf, " <= 0.6, 2, if (", srcDelta, " > 0, 3, if (", srcDelta, " <= 0, 4, null())))))))")

		} else if (undet == "perforated") {
		
			ex <- paste0(src, " = if (", srcXZeros, " == 0, ", noneValue, ", if (", srcPf, " == 0, 0, if (", srcPf, " == 1 & ", srcPff, " == 1, 6, if (", srcPf, " <= 0.4, 1, if (", srcPf, " <= 0.6, 2, if (", srcDelta, " >= 0, 3, if (", srcDelta, " < 0, 4, null())))))))")
		
		}
		
		steps <- steps + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, steps)
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

		# # if masking to just focal habitat
		# if (restrict) {

		# 	# multiply by focal habitat (1)/non-focal habitat (0 or NA)
		# 	srcMask <- .makeSourceName("fragmentation_mask", "raster")
		# 	ex <- paste0(srcMask, " = if (", srcIn, " == 1, 1, null())")
		# 	rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# 	srcIn <- src
		# 	src <- .makeSourceName("fragmentation_class_masked")
		# 	ex <- paste0(src, " = ", srcIn, " * ", srcMask)
		# 	rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
			
		# 	# make mask, copy with mask in place, remove mask
		# 	# # rgrass::execGRASS(cmd = "r.mask", raster = srcIn, flags = c(.quiet(), "overwrite"))
		# 	# # src <- .copyGRaster(src, topo = topology(x)[i], reshapeRegion = FALSE)
		# 	# # .removeMask()

		# } # else {
		
		# 	# mask to non-NA cells in input
		# 	srcIn <- src
		# 	src <- .makeSourceName("fragmentation_mask_NAs", "raster")
		# 	ex <- paste0(src, " = if(isnull(", sources(x)[i], "), null(), ", srcIn, ")")
		# 	rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# }

		srcs[i] <- src

		if (faster("clean")) .rm(created, type = "raster", warn = FALSE)
		created <- character()

	} # next layer

	if (verbose | faster("verbose")) close(pb)

	levs <- .fragmentationLevels(undet = undet, none = none)
	levels <- list()
	for (i in seq_len(nLayers)) {
		levels[[i]] <- levs
	}

	.makeGRaster(srcs, names = "fragmentation", levels = levels)

	} # EOF
	
)

#' Create levels table for `fragmentation()`.
#'
#' @param undet Value of `undet`.
#' @param none Value of `none`.
#'
#' @returns A `data.frame`.
#'
#' @noRd
.fragmentationLevels <- function(undet, none) {

	if (undet %in% c("perforated", "edge")) {

		if (is.na(none)) {

			levs <- data.frame(value = c(1L:4L, 6L), class = c("patch", "transitional", "perforated", "edge", "interior"))
		
		} else {
		
			levs <- data.frame(value = c(none, 1L:4L, 6L), class = c("none", "patch", "transitional", "perforated", "edge", "interior"))

		}

	} else if (undet == "undetermined") {
	
		if (is.na(none)) {
			
			levs <- data.frame(value = 1L:6L, class = c("patch", "transitional", "perforated", "edge", "undetermined", "interior"))

		} else {
		
			levs <- data.frame(value = c(none, 1L:6L), class = c("none", "patch", "transitional", "perforated", "edge", "undetermined", "interior"))

		}
	
	}
	levs

}
