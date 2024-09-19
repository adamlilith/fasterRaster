#' @aliases bioclims
#' @rdname bioclims
#' @exportMethod bioclims
methods::setMethod(
	f = "bioclims",
	signature = c(ppt = "SpatRaster"),
	function(
		ppt,
		tmin,
		tmax,
		tmean = NULL,
		bios = NULL,
		sample = TRUE,
		quarter = 3,
		pptDelta = 1,
		verbose = TRUE
	) {

	# BIOCLIM numbers
	classicBios <- 1L:19L
	extendedBios <- 41L:60L
	allBios <- c(classicBios, extendedBios)

	meanTempBios <- c(1, 4, 8:11, 18:19, 41:46, 53:54, 57:60) # BIOCLIMs that require mean temp
	quarterTempBios <- c(10, 11, 18, 19, 41:46, 53:54) # BIOCLIMs that require quarter temperatures
	quarterPrecipBios <- c(8, 9, 16, 17, 16:19, 45:48, 55:56)
	wettestQuarterBios <- c(8, 16, 46, 48, 55)
	driestQuarterBios <- c(9, 17, 45, 47, 56)
	warmestQuarterBios <- c(10, 18, 42, 44, 53)
	coldestQuarterBios <- c(11, 19, 41, 43, 54)
	
	if (is.null(bios)) {
		bios <- classicBios
	} else if (any(bios == "*")) {
		bios <- allBios
	} else if (any(bios == "+")) {
		bios <- bios[bios != "+"]
		bios <- c(bios, extendedBios)
	}

	bios <- unique(bios)
	bios <- sort(bios)
	bios <- as.integer(bios)

	if (quarter <= 1) stop("Argument `quarter` must be > 1.")

	nLayers <- terra::nlyr(ppt)

	if (any(omnibus::notIn(bios, allBios))) stop("Argument `bios` has at least one invalid value. Valid values include:\n  ", paste(allBios, collapse = " "))

	if (nLayers != terra::nlyr(tmin) | nLayers != terra::nlyr(tmax)) stop("The input rasters must have the same number of layers.")

	if (quarter > nLayers) stop("Argument `quarter` cannot be more than the number of raster layers in any input data.")

	### number of tasks (used for progress bar
	nTasks <- length(bios) +
		(any(meanTempBios %in% bios) & !is.null(tmean)) +
		any(bios %in% quarterTempBios) +
		any(bios %in% quarterPrecipBios) +
		(any(bios %in% warmestQuarterBios) & !(53 %in% warmestQuarterBios)) +
		(any(bios %in% coldestQuarterBios) & !(54 %in% coldestQuarterBios)) +
		(any(bios %in% wettestQuarterBios) & !(55 %in% wettestQuarterBios)) +
		(any(bios %in% driestQuarterBios) & !(55 %in% driestQuarterBios))
		
	if (verbose | faster("verbose")) {
		pb <- utils::txtProgressBar(min = 0, max = nTasks, initial = 0, style = 3, width = 30)
	}
	tasks <- 0

	### calculate tmean if needed
	if (is.null(tmean) & any(bios %in% meanTempBios)) {
		
		tmean <- (tmin + tmax) / 2

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	### quarter temperature rasters
	if (any(bios %in% quarterTempBios)) {

		tmeanByQuarter <- .calcQuarterSR(tmean, fun = "mean", quarter = quarter)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	### quarter precipitation rasters
	if (any(bios %in% quarterPrecipBios)) {

		pptByQuarter <- .calcQuarterSR(ppt, fun = "sum", quarter = quarter)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	### MAIN
	########

	out <- NULL

	### BIO53 (warmest quarter)
	if (any(bios %in% warmestQuarterBios)) {

		bio53 <- app(tmeanByQuarter, which.max)
		warmestQuarters <- terra::freq(bio53)$value
		names(bio53) <- "bio53"
		if (53 %in% bios) out <- c(out, bio53)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
		
	}

	### BIOCLIM 54 (coldest quarter)
	if (any(bios %in% coldestQuarterBios)) {

		bio54 <- app(tmeanByQuarter, which.min)
		coldestQuarters <- terra::freq(bio54)$value
		names(bio54) <- "bio54"
		if (54 %in% bios) out <- c(out, bio54)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### BIOCLIM 55 (wettest quarter)
	if (any(bios %in% wettestQuarterBios)) {

		bio55 <- app(pptByQuarter, which.max)
		wettestQuarters <- terra::freq(bio55)$value
		names(bio55) <- "bio55"
		if (55 %in% bios) out <- c(out, bio55)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
		
	}

	### BIOCLIM 56 (driest quarter)
	if (any(bios %in% driestQuarterBios)) {

		bio56 <- app(pptByQuarter, which.min)
		driestQuarters <- terra::freq(bio56)$value
		names(bio56) <- "bio56"
		if (56 %in% bios) out <- c(out, bio56)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
		
	}

	### BIOCLIM 1
	if (any(1 %in% bios)) {
		
		bio1 <- mean(tmean)
		names(bio1) <- "bio1"
		out <- c(out, bio1)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	### BIOCLIM 2
	if (any(c(2, 3) %in% bios)) {

		delta <- tmax - tmin
		bio2 <- mean(delta)
		names(bio2) <- "bio2"
		if (2 %in% bios) out <- c(out, bio2)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	### BIOCLIM 5
	if (any(c(3, 5, 7) %in% bios)) {
		
		bio5 <- max(tmax)
		names(bio5) <- "bio5"
		if (5 %in% bios) out <- c(out, bio5)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	### BIOCLIM 6
	if (any(c(3, 6, 7) %in% bios)) {
		
		bio6 <- min(tmin)
		names(bio6) <- "bio6"
		if (6 %in% bios) out <- c(out, bio6)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### BIOCLIM 7 (annual temp range)
	if (any(c(3, 7) %in% bios)) {
		
		bio7 <- bio5 - bio6
		names(bio7) <- "bio7"
		if (7 %in% bios) out <- c(out, bio7)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	### BIOCLIM 3 (100 * bio2 / bio7)
	if (3 %in% bios) {
		
		bio3 <- 100 * bio2 / bio7
		names(bio3) <- "bio3"
		out <- c(out, bio3)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	### BIOCLIM 4 (100 * sd of temp)
	if (4 %in% bios) {
		
		bio4 <- 100 * terra::stdev(tmean, pop = !sample)
		names(bio4) <- "bio4"
		out <- c(out, bio4)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	# BIOCLIM 8 (mean temp of wettest quarter)
	if (8 %in% bios) {
		
		mm <- terra::minmax(tmeanByQuarter)
		minVal <- -10 * abs(min(mm[1L, , drop = TRUE]))
		bio8 <- tmean[[1L]] * 0 + minVal
		for (i in wettestQuarters) {
			bio8 <- max(bio8, (bio55 == i) * tmeanByQuarter[[i]])
		}

		names(bio8) <- "bio8"
		out <- c(out, bio8)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIOCLIM 9 (mean temp of driest quarter)
	if (9 %in% bios) {
		
		mm <- terra::minmax(tmeanByQuarter)
		minVal <- -10 * abs(min(mm[1L, , drop = TRUE]))
		bio9 <- tmean[[1L]] * 0 + minVal
		for (i in driestQuarters) {
			bio9 <- max(bio9, (bio56 == i) * tmeanByQuarter[[i]])
		}

		names(bio9) <- "bio9"
		out <- c(out, bio9)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIOCLIM 10 (mean temp of warmest quarter)
	if (10 %in% bios) {

		mm <- terra::minmax(tmeanByQuarter)
		minVal <- -10 * abs(min(mm[1L, , drop = TRUE]))
		bio10 <- tmean[[1L]] * 0 + minVal
		for (i in warmestQuarters) {
			bio10 <- max(bio10, (bio53 == i) * tmeanByQuarter[[i]])
		}

		names(bio10) <- "bio10"
		out <- c(out, bio10)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIOCLIM 11 (mean temp of coldest quarter)
	if (11 %in% bios) {

		mm <- terra::minmax(tmeanByQuarter)
		minVal <- -10 * abs(min(mm[1L, , drop = TRUE]))
		bio11 <- tmean[[1L]] * 0 + minVal
		for (i in coldestQuarters) {
			bio11 <- max(bio11, (bio54 == i) * tmeanByQuarter[[i]])
		}

		bio11 <- max(bio11)
		names(bio11) <- "bio11"
		out <- c(out, bio11)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIOCLIM 12 (total precipitation)
	if (12 %in% bios) {

		bio12 <- sum(ppt)
		names(bio12) <- "bio12"
		if (12 %in% bios) out <- c(out, bio12)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIOCLIM 13 (wettest month precipitation)
	if (13 %in% bios) {

		bio13 <- max(ppt)
		names(bio13) <- "bio13"
		out <- c(out, bio13)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIOCLIM 14 (driest month precipitation)
	if (14 %in% bios) {

		bio14 <- min(ppt)
		names(bio14) <- "bio14"
		out <- c(out, bio14)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### BIOCLIM 15 (CV of precipitation)
	if (15 %in% bios) {
		
		sd <- terra::stdev(ppt + pptDelta, pop = !sample)
		bio15 <- 100 * sd / mean(ppt + pptDelta)
		names(bio15) <- "bio15"
		out <- c(out, bio15)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	# BIOCLIM 16 (precipitation of the wettest quarter)
	if (16 %in% bios) {

		bio16 <- ppt[[1L]] * 0
		for (i in wettestQuarters) {
			bio16 <- c(bio16, (bio55 == i) * pptByQuarter[[i]])
		}

		bio16 <- sum(bio16)
		names(bio16) <- "bio16"
		out <- c(out, bio16)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO17: Precipitation of the driest quarter
	if (17 %in% bios) {

		bio17 <- ppt[[1L]] * 0
		for (i in driestQuarters) {
			bio17 <- c(bio17, (bio56 == i) * pptByQuarter[[i]])
		}

		bio17 <- sum(bio17)
		names(bio17) <- "bio17"
		out <- c(out, bio17)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO18: Precipitation of the warmest quarter (based on mean temperature)
	if (18 %in% bios) {

		bio18 <- ppt[[1L]] * 0
		for (i in warmestQuarters) {
			bio18 <- c(bio18, (bio53 == i) * pptByQuarter[[i]])
		}

		bio18 <- sum(bio18)
		names(bio18) <- "bio18"
		out <- c(out, bio18)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO19: Precipitation of the coldest quarter (based on mean temperature)
	if (19 %in% bios) {

		bio19 <- ppt[[1L]] * 0
		for (i in coldestQuarters) {
			bio19 <- c(bio19, (bio54 == i) * pptByQuarter[[i]])
		}

		bio19 <- sum(bio19)
		names(bio19) <- "bio19"
		out <- c(out, bio19)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO41: Temperature of the quarter following the coldest quarter (based on mean temperature)
	if (41 %in% bios) {

		mm <- terra::minmax(tmeanByQuarter)
		minVal <- -10 * abs(min(mm[1L, , drop = TRUE]))
		bio41 <- tmean[[1L]] * 0 + minVal
		for (i in coldestQuarters) {
			if (i + quarter <= nLayers) {
				j <- i + quarter
			} else {
				j <- i + quarter - nLayers
			}
			bio41 <- max(bio41, (bio54 == i) * tmeanByQuarter[[j]])
		}

		names(bio41) <- "bio41"
		out <- c(out, bio41)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO42: Temperature of the quarter following the warmest quarter (based on mean temperature)
	if (42 %in% bios) {

		mm <- terra::minmax(tmeanByQuarter)
		minVal <- -10 * abs(min(mm[1L, , drop = TRUE]))
		bio42 <- tmean[[1L]] * 0 + minVal
		for (i in warmestQuarters) {
			if (i + quarter <= nLayers) {
				j <- i + quarter
			} else {
				j <- i + quarter - nLayers
			}
			bio42 <- max(bio42, (bio53 == i) * tmeanByQuarter[[j]])
		}

		names(bio42) <- "bio42"
		out <- c(out, bio42)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO43: Precipitation of the quarter following the coldest quarter (based on mean temperature; mm)
	if (43 %in% bios) {

		bio43 <- ppt[[1L]] * 0
		for (i in coldestQuarters) {
			if (i + quarter <= nLayers) {
				j <- i + quarter
			} else {
				j <- i + quarter - nLayers
			}
			bio43 <- max(bio43, (bio54 == i) * pptByQuarter[[j]])
		}

		names(bio43) <- "bio43"
		out <- c(out, bio43)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO44: Precipitation of the quarter following the warmest quarter (based on mean temperature; mm)
	if (44 %in% bios) {

		bio44 <- ppt[[1L]] * 0
		for (i in warmestQuarters) {
			if (i + quarter <= nLayers) {
				j <- i + quarter
			} else {
				j <- i + quarter - nLayers
			}
			bio44 <- max(bio44, (bio53 == i) * pptByQuarter[[j]])
		}

		names(bio44) <- "bio44"
		out <- c(out, bio44)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO45: Temperature of the quarter following the driest quarter (based on mean temperature; deg C)
	if (45 %in% bios) {

		mm <- terra::minmax(tmeanByQuarter)
		minVal <- -10 * abs(min(mm[2L, , drop = TRUE]))
		bio45 <- tmean[[1L]] * 0 + minVal
		for (i in driestQuarters) {
			if (i + quarter <= nLayers) {
				j <- i + quarter
			} else {
				j <- i + quarter - nLayers
			}
			bio45 <- max(bio45, (bio56 == i) * tmeanByQuarter[[j]])
		}

		names(bio45) <- "bio45"
		out <- c(out, bio45)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO46: Temperature of the quarter following the wettest quarter (based on mean temperature; deg C)
	if (46 %in% bios) {

		mm <- terra::minmax(tmeanByQuarter)
		minVal <- -10 * abs(min(mm[2L, , drop = TRUE]))
		bio46 <- tmean[[1L]] * 0 + minVal
		for (i in wettestQuarters) {
			if (i + quarter <= nLayers) {
				j <- i + quarter
			} else {
				j <- i + quarter - nLayers
			}
			bio46 <- max(bio46, (bio55 == i) * tmeanByQuarter[[j]])
		}

		names(bio46) <- "bio46"
		out <- c(out, bio46)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO47: Precipitation of the quarter following the driest quarter (based on mean temperature; mm)
	if (47 %in% bios) {

		bio47 <- ppt[[1L]] * 0
		for (i in driestQuarters) {
			if (i + quarter <= nLayers) {
				j <- i + quarter
			} else {
				j <- i + quarter - nLayers
			}
			bio47 <- max(bio47, (bio56 == i) * pptByQuarter[[j]])
		}

		names(bio47) <- "bio47"
		out <- c(out, bio47)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO48: Precipitation of the quarter following the wettest quarter (based on mean temperature; mm)
	if (48 %in% bios) {

		bio48 <- ppt[[1L]] * 0
		for (i in wettestQuarters) {
			if (i + quarter <= nLayers) {
				j <- i + quarter
			} else {
				j <- i + quarter - nLayers
			}
			bio48 <- max(bio48, (bio55 == i) * pptByQuarter[[j]])
		}

		names(bio48) <- "bio48"
		out <- c(out, bio48)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO49: Hottest month
	if (49 %in% bios) {

		bio49 <- terra::app(tmax, which.max)
		names(bio49) <- "bio49"
		out <- c(out, bio49)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO50: Coldest month
	if (50 %in% bios) {

		bio50 <- terra::app(tmin, which.min)
		names(bio50) <- "bio50"
		out <- c(out, bio50)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}
	
	# BIO51: Wettest month
	if (51 %in% bios) {

		bio51 <- terra::app(ppt, which.max)
		names(bio51) <- "bio51"
		out <- c(out, bio51)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO52: Driest month
	if (52 %in% bios) {

		bio52 <- terra::app(ppt, which.min)
		names(bio52) <- "bio52"
		out <- c(out, bio52)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO57: The largest decrease in temperature from one month to the next (deg C).
	# BIO58: The largest increase in temperature from one month to the next (deg C).
	if (any(c(57, 58) %in% bios)) {

		deltas <- tmean
		for (i in 1:nLayers) {
			
			if (i + quarter <= nLayers) {
				target <- i + 1
			} else {
				target <- 1
			}

			deltas[[i]] <- tmean[[i]] - tmean[[target]]

		}

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
		
		if (57 %in% bios) {

			bio57 <- max(deltas)
			names(bio57) <- "bio57"
			out <- c(out, bio57)

			tasks <- tasks + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		}

		if (58 %in% bios) {

			bio58 <- abs(min(deltas))
			names(bio58) <- "bio58"
			out <- c(out, bio58)

			tasks <- tasks + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		}

	}
	
	# BIO59: The largest decrease in precipitation from one month to the next.
	# BIO60: The largest increase in precipitation from one month to the next.
	if (any(c(59, 60) %in% bios)) {

		deltas <- ppt
		for (i in 1:nLayers) {
			
			if (i + quarter <= nLayers) {
				target <- i + 1
			} else {
				target <- 1
			}

			deltas[[i]] <- ppt[[i]] - ppt[[target]]

		}

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
		
		if (59 %in% bios) {

			bio59 <- max(deltas)
			names(bio59) <- "bio59"
			out <- c(out, bio59)

			tasks <- tasks + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		}

		if (60 %in% bios) {

			bio60 <- abs(min(deltas))
			names(bio60) <- "bio60"
			out <- c(out, bio60)

			tasks <- tasks + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		}

	}

	out <- do.call(c, out)

	outNames <- names(out)
	nc <- nchar(outNames)
	if (any(nc == 4L)) {
		shorts <- out[[nc == 4L]]
		shorts <- shorts[[order(names(shorts))]]
	} else { shorts <- NULL }

	if (any(nc == 5L)) {
		longs <- out[[nc == 5L]]
		longs <- longs[[order(names(longs))]]
	} else { longs <- NULL }

	if (is.null(shorts)) {
		out <- longs
	} else if (is.null(longs)) {
		out <- shorts
	} else {
		out <- c(shorts, longs)
	}

	if (verbose | faster("verbose")) close(pb)
	out

	} # EOF
)


#' Calculate a series of "quarter" rasters and return their [sources()] names. A quarter can be summarized using 
#'
#' x A "stack" of `SpatRaster`s
#' fun Character: Name of function to summarize across a quarter ("sum", "min", "max", or "mean").
#' quarter Numeric: Length of a quarter
#'
#' @noRd
.calcQuarterSR <- function(x, fun, quarter) {

	out <- x
	qs <- c(x, x[[1L:(quarter - 1L)]])

	n <- nlyr(x)
	for (i in seq_len(n)) {

		select <- i:(i + quarter - 1L)
		quart <- qs[[select]]

		args <- list(x = quart)
		out[[i]] <- do.call(fun, args)

	} # next quarter
	names(out) <- paste0("q", 1L:n)
	out

}
