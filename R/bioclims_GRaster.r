#' BIOCLIM rasters
#'
#' @description The BIOCLIM set of bioclimatic variables were created for modeling species' geographic distributions (Booth et al. 2014). This function can create the "standard" 19 set of variables, plus several more from an "extended" set.
#'
#' The units reported below assume that input rasters are in mm (precipitation) and deg C (temperature), and that each raster represents a month (but other time units are allowed, with corresponding changes to the temporal units assumed below).
#'
#' * BIO1: Mean annual temperature, calculated using monthly means (deg C)
#' * BIO2: Mean diurnal range across months (average of monthly difference between maximum and minimum temperature) (deg C)
#' * BIO3: Isothermality (100 * BIO02 / BIO07; unit-less)
#' * BIO4: Temperature seasonality (standard deviation across months of average monthly temperature * 100; deg C)
#' * BIO5: Maximum temperature of the warmest month (based on maximum temperature; deg C)
#' * BIO6: Minimum temperature of the coldest month (based on minimum temperature; deg C)
#' * BIO7: Range of annual temperature (BIO05 - BIO06; deg C)
#' * BIO8: Temperature of the wettest quarter (based on mean temperature; deg C)
#' * BIO9: Temperature of the driest quarter (based on mean temperature; deg C)
#' * BIO10: Temperature of the warmest quarter (based on mean temperature; deg C)
#' * BIO11: Temperature of the coldest quarter (based on mean temperature; deg C)
#'
#' * BIO12: Total annual precipitation (mm)
#' * BIO13: Precipitation of the wettest month (mm)
#' * BIO14: Precipitation of the driest month (mm)
#' * BIO15: Precipitation seasonality (100 * coefficient of variation; unit-less)
#' * BIO16: Precipitation of the wettest quarter (mm)
#' * BIO17: Precipitation of the driest quarter (mm)
#' * BIO18: Precipitation of the warmest quarter (based on mean temperature; mm)
#' * BIO19: Precipitation of the coldest quarter (based on mean temperature; mm)
#'
#' "Extended" set of BIOCLIM variables (starts at 41 to avoid conflicts with Kriticos et al. 2014):
#' * BIO41: Temperature of the quarter following the coldest quarter (based on mean temperature; deg C)
#' * BIO42: Temperature of the quarter following the warmest quarter (based on mean temperature; deg C)
#' * BIO43: Precipitation of the quarter following the coldest quarter (based on mean temperature; mm)
#' * BIO44: Precipitation of the quarter following the warmest quarter (based on mean temperature; mm)
#'
#' * BIO45: Temperature of the quarter following the driest quarter (based on mean temperature; deg C)
#' * BIO46: Temperature of the quarter following the wettest quarter (based on mean temperature; deg C)
#' * BIO47: Precipitation of the quarter following the driest quarter (based on mean temperature; mm)
#' * BIO48: Precipitation of the quarter following the wettest quarter (based on mean temperature; mm)
#'
#' * BIO49: Hottest month (based on maximum temperature)
#' * BIO50: Coldest month (based on minimum temperature)
#' * BIO51: Wettest month
#' * BIO52: Driest month
#'
#' * BIO53: First month of the warmest quarter (based on mean temperature)
#' * BIO54: First month of the coldest quarter (based on mean temperature)
#' * BIO55: First month of the wettest quarter
#' * BIO56: First month of the driest quarter
#'
#' * BIO57: The greatest decrease in temperature from one month to the next (deg C; always >= 0)
#' * BIO58: The greatest increase in temperature from one month to the next (deg C; always >= 0)
#' * BIO59: The greatest decrease in precipitation from one month to the next (mm; always >= 0)
#' * BIO60: The greatest increase in precipitation from one month to the next (mm; always >= 0)
#' 
#' By default, "quarter" refers to any consecutive run of three months, not a financial quarter. A quarter can thus include November-December-January, or December-January-February, for example. However, the length of a quarter can be changed using the argument `quarter`.
#'
#' The variables are defined assuming that the input rasters represent monthly values (12 rasters for min/max temperature and precipitation), but you can also use sets of 52 rasters, representing one per week, in which case "quarter" would be a successive run of 3 weeks. You could also attempt 365 rasters, in which case a "quarter" would be a run of 3 successive days.
#'
#' BIOCLIMs 41 through 44 are added here to capture the "shoulder" seasons (spring and autumn) important in temperature regions. BIOCLIMs 45 through 48 are also included for consistency.
#'
#' BIOCLIMs 49 through 60 are not bioclimatic variables per se, but useful for assessing the properties of the variables that are defined based on the "-est" month or quarter.
#'
#' The numbering of the new BIOCLIMs was begun at 41 because BIOCLIMs 20 through 40 are taken (Kriticos et al. 2014).
#'
#' @param ppt A multi-layered `GRaster` or `SpatRaster`, representing monthly/weekly/daily precipitation.
#'
#' @param tmin,tmax A multi-layered `GRaster` or `SpatRaster`, representing monthly/weekly/daily minimum and maximum temperature.
#'
#' @param tmean Either `NULL` (default), or a multi-layered `GRaster` or `SpatRaster`, representing monthly/weekly/daily average temperature. If `NULL`, `tmean` will be calculated internally from `tmin` and `tmax`. Providing these rasters thus saves time if you already have them on hand.
#'
#' @param bios Any of:
#' * Numeric values: Calculate these BIOCLIM variables. For example, `bios = c(1, 12)` calculates BIOCLIMs 1 and 12.
#' * `NULL` (default): Calculate BIOCLIMs 1 through 19
#' * `"*"`: Calculate all BIOCLIMs this function can calculate.
#' * `"+"`: Calculate BIOCLIMs 41 onward.
#' * Any combination of the above (e.g., `c(1, 12, "+")`).
#'
#' @param sample Logical: If `TRUE` (default), BIO4 and 15 are calculated with the sample standard deviation. If `FALSE`, then the population standard deviation is used.
#'
#' @param quarter Numeric: Length of a "quarter". BIOCLIM variables are typically calculated using monthly-averaged rasters (e.g., precipitation and temperature of January, February, etc.), in which case a "quarter" is 3 months (so the default for `quarter` is 3). However, this function can accommodate any set of rasters representing a time series (e.g., 365 for daily rasters), in which case the user can decide what constitutes a "quarter" for calculation of the any BIOCLIMs that use "quarters" in their definitions.
#'
#' @param pptDelta Numeric: Value to add to precipitation for calculation of BIO15 (coefficient of variation of precipitation, times 100). Adding a small value avoids division by 0. The default is 1.
#'
#' @param verbose Logical: If `TRUE` (default), display progress.
#'
#' @returns A `GRaster` with one or more layers.
#'
#' @references Booth, T.H., Nix, H.A., Busby, J.R., and Hutchinson, M.F.  2014.  BIOCLIM: The first species distribution modeling package, its early applications and relevance to most current MaxEnt studies. *Diversity and Distributions* 20:1-9 \doi{10.1111/ddi.12144}.
#'
#' Kriticos, D.J., Jarošik, V., and Otam N.  2014.  Extending the suite of BIOCLIM variables: A proposed registry system and case study using principal components analysis. *Methods in Ecology and Evolution* 5:956-960 \doi{10.1111/2041-210X.12244}.
#'
#' @example man/examples/ex_bioclims.r
#'
#' @aliases bioclims
#' @rdname bioclims
#' @exportMethod bioclims
methods::setMethod(
	f = "bioclims",
	signature = c(ppt = "GRaster"),
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
	
	### number of tasks (used for progress bar
	nTasks <- 2 + length(bios) +
		(any(meanTempBios %in% bios) & !is.null(tmean)) +
		any(quarterTempBios %in% bios) +
		any(quarterPrecipBios %in% bios) +
		any(wettestQuarterBios %in% bios) +
		any(driestQuarterBios %in% bios) +
		any(warmestQuarterBios %in% bios) +
		any(coldestQuarterBios %in% bios) +
		any(c(57, 58) %in% bios) +
		any(c(59, 60) %in% bios) +
		(sample & 4 %in% bios) +
		(sample & 15 %in% bios)

	if (verbose | faster("verbose")) {
		pb <- utils::txtProgressBar(min = 0, max = nTasks, initial = 0, style = 3, width = 30)
	}
	tasks <- 0

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

	nLayers <- nlyr(ppt)

	if (any(omnibus::notIn(bios, allBios))) stop("Argument `bios` has at least one invalid value. Valid values include:\n  ", paste(allBios, collapse = " "))

	if (any(omnibus::notIn(bios, allBios))) stop("Argument `bios` has at least one invalid value. Valid values include:\n  ", paste(allBios, collapse = ", "))

	if (quarter > nLayers) stop("Argument `quarter` cannot be more than the number of raster layers in any of the input GRasters.")

	compareGeom(ppt, tmin, lyrs = TRUE)
	compareGeom(ppt, tmax, lyrs = TRUE)
	if (!is.null(tmean)) compareGeom(ppt, tmean, lyrs = TRUE)
	
	.locationRestore(ppt)
	.region(ppt)

	ppt <- sources(ppt)
	tmin <- sources(tmin)
	tmax <- sources(tmax)
	if (!is.null(tmean)) tmean <- sources(tmean)

	### calculate tmean if needed
	if (is.null(tmean) & any(bios %in% meanTempBios)) {

		tmean <- .makeSourceName("r_mapcalc", "raster", nLayers)
		for (i in seq_len(nLayers)) {

			ex <- paste0(tmean[i], " = (", tmin[i], " + ", tmax[i], ") / 2")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		}
		if (faster("clean")) on.exit(.rm(tmean, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### quarter temperature rasters
	if (any(bios %in% quarterTempBios)) {


		tmeanByQuarter <- .calcQuarterGR(tmean, fun = "mean", quarter = quarter)
		if (faster("clean")) on.exit(.rm(tmeanByQuarter, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### quarter precipitation rasters
	if (any(bios %in% quarterPrecipBios)) {

		pptByQuarter <- .calcQuarterGR(ppt, fun = "sum", quarter = quarter)
		if (faster("clean")) on.exit(.rm(pptByQuarter, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### MAIN
	########

	out <- NULL

	### BIO53 (warmest quarter)
	if (any(bios %in% warmestQuarterBios)) {

		bio53Minus1 <- .makeSourceName("r_mapcalc", "raster")
		bio53 <- .makeSourceName("r_mapcalc", "raster", name = "bio53")
		
		rgrass::execGRASS(
			cmd = "r.series",
			input = tmeanByQuarter,
			output = bio53Minus1,
			method = "max_raster",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		if (faster("clean")) on.exit(.rm(bio53Minus1, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		ex <- paste0(bio53, " = ", bio53Minus1, " + 1")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# vector of warmest quarters
		warmestQuarters <- .freq(bio53, dtype = "CELL", digits = NA, bins = NA)
		warmestQuarters <- warmestQuarters[["value"]]

		if (53 %in% bios) {
			out <- c(out, bio53)
			names(out)[length(out)] <- "bio53"
		}

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### BIOCLIM 54 (coldest quarter)
	if (any(bios %in% coldestQuarterBios)) {

		bio54Plus1 <- .makeSourceName("r_mapcalc", "raster")
		bio54 <- .makeSourceName("r_mapcalc", "raster", name = "bio54")
		
		rgrass::execGRASS(
			cmd = "r.series",
			input = tmeanByQuarter,
			output = bio54Plus1,
			method = "min_raster",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		if (faster("clean")) on.exit(.rm(bio54Plus1, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		ex <- paste0(bio54, " = ", bio54Plus1, " + 1")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# get vector of all coldest quarters
		coldestQuarters <- .freq(bio54, dtype = "CELL", digits = NA, bins = NA)
		coldestQuarters <- coldestQuarters[["value"]]

		if (54 %in% bios) {
			out <- c(out, bio54)
			names(out)[length(out)] <- "bio54"
		}

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### BIOCLIM 55 (wettest quarter)
	if (any(bios %in% wettestQuarterBios)) {
		
		bio55Plus1 <- .makeSourceName("r_mapcalc", "raster")
		bio55 <- .makeSourceName("r_mapcalc", "raster", name = "bio55")

		rgrass::execGRASS(
			cmd = "r.series",
			input = pptByQuarter,
			output = bio55Plus1,
			method = "max_raster",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		if (faster("clean")) on.exit(.rm(bio55Plus1, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		# increment index (first is 0)
		ex <- paste0(bio55, " = ", bio55Plus1, " + 1")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# get vector of all wettest quarters
		wettestQuarters <- .freq(bio55, dtype = "CELL", digits = NA, bins = NA)
		wettestQuarters <- wettestQuarters[["value"]]

		if (55 %in% bios) {
			out <- c(out, bio55)
			names(out)[length(out)] <- "bio55"
		}

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### BIOCLIM 56 (driest quarter)
	if (any(bios %in% driestQuarterBios)) {

		bio56Plus1 <- .makeSourceName("r_mapcalc", "raster")
		bio56 <- .makeSourceName("r_mapcalc", "raster", name = "bio56")
		
		rgrass::execGRASS(
			cmd = "r.series",
			input = pptByQuarter,
			output = bio56Plus1,
			method = "min_raster",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		if (faster("clean")) on.exit(.rm(bio56Plus1, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		ex <- paste0(bio56, " = ", bio56Plus1, " + 1")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		# get vector of all driest quarters
		driestQuarters <- .freq(bio56, dtype = "CELL", digits = NA, bins = NA)
		driestQuarters <- driestQuarters[["value"]]

		if (56 %in% bios) {
			out <- c(out, bio56)
			names(out)[length(out)] <- "bio56"
		}

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### BIOCLIM 1
	if (1 %in% bios) {

		bio1 <- .makeSourceName("r_series", "raster", n = 1L, name = "bio1")
		rgrass::execGRASS(
			"r.series",
			input = paste(tmean, collapse = ","),
			output = bio1,
			method = "average",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		out <- c(out, bio1)
		names(out)[length(out)] <- "bio1"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	### BIOCLIM 2
	if (any(c(2, 3) %in% bios)) {
		
		meanMaxSrc <- .makeSourceName("r_series", "raster", n = 1L)
		meanMinSrc <- .makeSourceName("r_series", "raster", n = 1L)
		bio2 <- .makeSourceName("r_series", "raster", n = 1L, name = "bio2")

		# mean of maximums
		rgrass::execGRASS(
			"r.series",
			input = paste(tmax, collapse = ","),
			output = meanMaxSrc,
			method = "average",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		# mean of minimums
		rgrass::execGRASS(
			"r.series",
			input = paste(tmin, collapse = ","),
			output = meanMinSrc,
			method = "average",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		# bio2
		ex <- paste0(bio2, " = ", meanMaxSrc, " - ", meanMinSrc)

		if (faster("clean")) on.exit(.rm(c(meanMaxSrc, meanMinSrc), type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		if (2 %in% bios) {
			out <- c(out, bio2)
			names(out)[length(out)] <- "bio2"
		}
	
		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### BIOCLIM 5
	if (any(c(3, 5, 7) %in% bios)) {
		
		bio5 <- .makeSourceName("r_series", "raster", n = 1L, name = "bio5")

		rgrass::execGRASS(
			"r.series",
			input = paste(tmax, collapse = ","),
			output = bio5,
			method = "maximum",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		if (5 %in% bios) {
			out <- c(out, bio5)
			names(out)[length(out)] <- "bio5"
		}

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	### BIOCLIM 6
	if (any(c(3, 6, 7) %in% bios)) {
		
		bio6 <- .makeSourceName("r_series", "raster", n = 1L, name = "bio6")

		rgrass::execGRASS(
			"r.series",
			input = paste(tmin, collapse = ","),
			output = bio6,
			method = "minimum",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		if (6 %in% bios) {
			out <- c(out, bio6)
			names(out)[length(out)] <- "bio6"
		}

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### BIOCLIM 7 (annual temp range)
	if (any(c(3, 7) %in% bios)) {
		
		bio7 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio7")
		ex <- paste0(bio7, " = ", bio5, " - ", bio6)
		
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		if (7 %in% bios) {
			out <- c(out, bio7)
			names(out)[length(out)] <- "bio7"
		}

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	### BIOCLIM 3 (100 * bio2 / bio7)
	if (3 %in% bios) {
		
		bio3 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio3")

		bio2 <- out[names(out) == "bio2"]
		bio7 <- out[names(out) == "bio7"]
		
		ex <- paste0(bio3, " = 100 * ", bio2, " / ", bio7)
		
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio3)
		names(out)[length(out)] <- "bio3"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	### BIOCLIM 4 (100 * sd of temp)
	if (4 %in% bios) {
		
		bio4 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio4")

		rgrass::execGRASS(
			"r.series",
			input = paste(tmean, collapse = ","),
			output = bio4,
			method = "stddev",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		# convert to sample SD as per R
		if (sample) {
		
			tasks <- tasks + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

			srcIn <- bio4
			bio4 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio4")

			ex <- paste0(bio4, " = 100 * sqrt((", nLayers, " * (", srcIn, "^2)) / (", nLayers, " - 1))")
			
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
			if (faster("clean")) on.exit(.rm(c(srcIn), type = "raster", warn = FALSE, verify = FALSE), add = TRUE)
		
		}

		out <- c(out, bio4)
		names(out)[length(out)] <- "bio4"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	# BIOCLIM 8 (mean temp of wettest quarter)
	if (8 %in% bios) {
		
		# burn temperature of wettest quarter to cells
		bio8 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio8")

		ex <- paste0(bio8, " = ")
		for (i in wettestQuarters) {
			ex <- paste0(ex, "if(", bio55, "==", i, ",", tmeanByQuarter[i], ",")
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(wettestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio8)
		names(out)[length(out)] <- "bio8"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIOCLIM 9 (mean temp of driest quarter)
	if (9 %in% bios) {

		# burn temperature of driest quarter to cells
		bio9 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio9")

		ex <- paste0(bio9, " = ")
		for (i in driestQuarters) {
			ex <- paste0(ex, "if(", bio56, "==", i, ",", tmeanByQuarter[i], ",")
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(driestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio9)
		names(out)[length(out)] <- "bio9"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIOCLIM 10 (mean temp of warmest quarter)
	if (10 %in% bios) {

		# burn temperature of warmest quarter to cells
		bio10 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio10")

		ex <- paste0(bio10, " = ")
		for (i in warmestQuarters) {
			ex <- paste0(ex, "if(", bio53, "==", i, ",", tmeanByQuarter[i], ",")
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(warmestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio10)
		names(out)[length(out)] <- "bio10"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIOCLIM 11 (mean temp of coldest quarter)
	if (11 %in% bios) {

		# burn temperature of coldest quarter to cells
		bio11 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio11")

		ex <- paste0(bio11, " = ")
		for (i in coldestQuarters) {
			ex <- paste0(ex, "if(", bio54, "==", i, ",", tmeanByQuarter[i], ",")
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(coldestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio11)
		names(out)[length(out)] <- "bio11"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIOCLIM 12 (total precipitation)
	if (12 %in% bios) {

		bio12 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio12")

		input <- paste0(ppt, collapse = ",")

		rgrass::execGRASS(
			cmd = "r.series",
			input = input,
			output = bio12,
			method = "sum",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		out <- c(out, bio12)
		names(out)[length(out)] <- "bio12"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIOCLIM 13 (wettest month precipitation)
	if (13 %in% bios) {

		bio13 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio13")

		input <- paste0(ppt, collapse = ",")

		rgrass::execGRASS(
			cmd = "r.series",
			input = input,
			output = bio13,
			method = "maximum",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		out <- c(out, bio13)
		names(out)[length(out)] <- "bio13"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIOCLIM 14 (driest month precipitation)
	if (14 %in% bios) {

		bio14 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio14")

		input <- paste0(ppt, collapse = ",")

		rgrass::execGRASS(
			cmd = "r.series",
			input = input,
			output = bio14,
			method = "minimum",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		out <- c(out, bio14)
		names(out)[length(out)] <- "bio14"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	### BIOCLIM 15 (CV of precipitation)
	if (15 %in% bios) {
		
		# as per dismo::biovars and WorldClim, add 1 to precipitation to avoid division by 0
		if (pptDelta != 0) {

			pptDeltaSrc <- .makeSourceName("bioclims", "raster", nLayers)
			for (i in seq_along(ppt)) {
				ex <- paste0(pptDeltaSrc[i], " = ", ppt[i], " + ", pptDelta)
				rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
			}

			tasks <- tasks + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		} else {
			pptDeltaSrc <- ppt
		}

		sdSrc <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio15")

		rgrass::execGRASS(
			"r.series",
			input = paste(pptDeltaSrc, collapse = ","),
			output = sdSrc,
			method = "stddev",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		# convert to sample SD as per R
		if (sample) {
		
			sdSrcIn <- sdSrc
			sdSrc <- .makeSourceName("r_mapcalc", "raster", n = 1L)

			ex <- paste0(sdSrc, " = sqrt((", nLayers, " * (", sdSrcIn, "^2)) / (", nLayers, " - 1))")
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
		
			if (faster("clean")) on.exit(.rm(c(sdSrcIn), type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

			tasks <- tasks + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		}

		meanSrc <- .makeSourceName("bioclims_r_series", "ppt_mean")
		rgrass::execGRASS(
			"r.series",
			input = paste(pptDeltaSrc, collapse = ","),
			output = meanSrc,
			method = "average",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		bio15 <- .makeSourceName("r_mapcalc", "raster", name = "bio15")
		
		ex <- paste0(bio15, " = 100 * ", sdSrc, " / ", meanSrc)
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio15)
		names(out)[length(out)] <- "bio15"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)
	
	}

	# BIOCLIM 16 (precipitation of the wettest quarter)
	if (16 %in% bios) {

		# burn precipitation of wettest quarter to cells
		bio16 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio16")

		ex <- paste0(bio16, " = ")
		for (i in wettestQuarters) {
			ex <- paste0(ex, "if(", bio55, "==", i, ",", pptByQuarter[i], ",")
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(wettestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio16)
		names(out)[length(out)] <- "bio16"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO17: Precipitation of the driest quarter
	if (17 %in% bios) {

		# burn precipitation of driest quarter to cells
		bio17 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio17")

		ex <- paste0(bio17, " = ")
		for (i in driestQuarters) {
			ex <- paste0(ex, "if(", bio56, "==", i, ",", pptByQuarter[i], ",")
		}
		ex <- paste0(ex, "null()", paste0(rep(")", length(driestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio17)
		names(out)[length(out)] <- "bio17"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO18: Precipitation of the warmest quarter (based on mean temperature)
	if (18 %in% bios) {

		# burn precipitation of warmest quarter to cells
		bio18 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio18")

		ex <- paste0(bio18, " = ")
		for (i in warmestQuarters) {
			ex <- paste0(ex, "if(", bio53, "==", i, ",", pptByQuarter[i], ",")
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(warmestQuarters) - 1L), collapse = ""), ")")

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio18)
		names(out)[length(out)] <- "bio18"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO19: Precipitation of the coldest quarter (based on mean temperature)
	if (19 %in% bios) {

		# burn precipitation of warmest quarter to cells
		bio19 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio19")

		ex <- paste0(bio19, " = ")
		for (i in coldestQuarters) {
			ex <- paste0(ex, "if(", bio54, "==", i, ",", pptByQuarter[i], ",")
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(coldestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio19)
		names(out)[length(out)] <- "bio19"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO41: Temperature of the quarter following the coldest quarter (based on mean temperature)
	if (41 %in% bios) {

		# burn precipitation of warmest quarter to cells
		bio41 <- .makeSourceName("r_mapcalc", "raster", n = 1L, name = "bio41")
		n <- length(coldestQuarters)

		ex <- paste0(bio41, " = ")
		for (i in coldestQuarters) {

			if (i + quarter <= nLayers) {
				target <- i + quarter
			} else {
				target <- i + quarter - n
			}
			ex <- paste0(bio41, " = if(", bio54, "==", i, ",", tmeanByQuarter[target], ",")
	
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(coldestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio41)
		names(out)[length(out)] <- "bio41"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO42: Temperature of the quarter following the warmest quarter (based on mean temperature; deg C)
	if (42 %in% bios) {

		# burn precipitation of warmest quarter to cells
		bio42 <- .makeSourceName("bioclims_bio42", "raster")

		ex <- paste0(bio42, " = ")
		for (i in warmestQuarters) {

			if (i + quarter <= nLayers) {
				target <- i + quarter
			} else {
				target <- i + quarter - nLayers
			}
			ex <- paste0(bio42, " = if(", bio53, "==", i, ",", tmeanByQuarter[target], ",")
	
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(warmestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio42)
		names(out)[length(out)] <- "bio42"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO43: Precipitation of the quarter following the coldest quarter (based on mean temperature; mm)
	if (43 %in% bios) {

		# burn precipitation of warmest quarter to cells
		bio43 <- .makeSourceName("bioclims_bio43", "raster")

		ex <- paste0(bio43, " = ")
		for (i in coldestQuarters) {

			if (i + quarter <= nLayers) {
				target <- i + quarter
			} else {
				target <- i + quarter - nLayers
			}
			ex <- paste0(bio43, " = if(", bio54, "==", i, ",", pptByQuarter[target], ",")
	
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(coldestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio43)
		names(out)[length(out)] <- "bio43"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO44: Precipitation of the quarter following the warmest quarter (based on mean temperature; mm)
	if (44 %in% bios) {

		bio44 <- .makeSourceName("bioclims_bio44", "raster")

		ex <- paste0(bio44, " = ")
		for (i in warmestQuarters) {

			if (i + quarter <= nLayers) {
				target <- i + quarter
			} else {
				target <- i + quarter - nLayers
			}
			ex <- paste0(bio44, " = if(", bio53, "==", i, ",", pptByQuarter[target], ",")
	
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(warmestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio44)
		names(out)[length(out)] <- "bio44"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO45: Temperature of the quarter following the driest quarter (based on mean temperature; deg C)
	if (45 %in% bios) {

		bio45 <- .makeSourceName("bioclims_bio45", "raster")

		ex <- paste0(bio45, " = ")
		for (i in driestQuarters) {

			if (i + quarter <= nLayers) {
				target <- i + quarter
			} else {
				target <- i + quarter - nLayers
			}
			ex <- paste0(bio45, " = if(", bio56, "==", i, ",", tmeanByQuarter[target], ",")
	
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(warmestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio45)
		names(out)[length(out)] <- "bio45"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO46: Temperature of the quarter following the wettest quarter (based on mean temperature; deg C)
	if (46 %in% bios) {

		bio46 <- .makeSourceName("bioclims_bio46", "raster")

		ex <- paste0(bio46, " = ")
		for (i in wettestQuarters) {

			if (i + quarter <= nLayers) {
				target <- i + quarter
			} else {
				target <- i + quarter - nLayers
			}
			ex <- paste0(bio46, " = if(", bio55, "==", i, ",", tmeanByQuarter[target], ",")
	
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(warmestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio46)
		names(out)[length(out)] <- "bio46"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO47: Precipitation of the quarter following the driest quarter (based on mean temperature; mm)
	if (47 %in% bios) {

		bio47 <- .makeSourceName("bioclims_bio47", "raster")

		ex <- paste0(bio47, " = ")
		for (i in driestQuarters) {

			if (i + quarter <= nLayers) {
				target <- i + quarter
			} else {
				target <- i + quarter - nLayers
			}
			ex <- paste0(bio47, " = if(", bio56, "==", i, ",", pptByQuarter[target], ",")
	
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(warmestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio47)
		names(out)[length(out)] <- "bio47"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO48: Precipitation of the quarter following the wettest quarter (based on mean temperature; mm)
	if (48 %in% bios) {

		bio48 <- .makeSourceName("bioclims_bio48", "raster")

		ex <- paste0(bio48, " = ")
		for (i in wettestQuarters) {

			if (i + quarter <= nLayers) {
				target <- i + quarter
			} else {
				target <- i + quarter - nLayers
			}
			ex <- paste0(bio48, " = if(", bio55, "==", i, ",", pptByQuarter[target], ",")
	
		}
		ex <- paste0(ex, "null()", paste(rep(")", length(warmestQuarters)), collapse = ""))

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio48)
		names(out)[length(out)] <- "bio48"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO49: Hottest month (based on maximum temperature)
	if (49 %in% bios) {

		bio49 <- .makeSourceName("bioclims_bio49", "raster")
		bio49Minus1 <- .makeSourceName("bioclims_bio49Minus1", "raster")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		rgrass::execGRASS(
			"r.series",
			input = tmax,
			output = bio49Minus1,
			method = "max_raster",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		if (faster("clean")) on.exit(.rm(bio49Minus1, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		ex <- paste0(bio49, " = ", bio49Minus1, " + 1")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio49)
		names(out)[length(out)] <- "bio49"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO50: Coldest month (based on minimum temperature)
	if (50 %in% bios) {

		bio50 <- .makeSourceName("bioclims_bio50", "raster")
		bio50Minus1 <- .makeSourceName("bioclims_bio50Minus1", "raster")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		rgrass::execGRASS(
			"r.series",
			input = tmin,
			output = bio50Minus1,
			method = "min_raster",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		if (faster("clean")) on.exit(.rm(bio50Minus1, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		ex <- paste0(bio50, " = ", bio50Minus1, " + 1")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio50)
		names(out)[length(out)] <- "bio50"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO51: Wettest month.
	if (51 %in% bios) {

		bio51 <- .makeSourceName("bioclims_bio51", "raster")
		bio51Minus1 <- .makeSourceName("bioclims_bio51Minus1", "raster")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		rgrass::execGRASS(
			"r.series",
			input = ppt,
			output = bio51Minus1,
			method = "max_raster",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		if (faster("clean")) on.exit(.rm(bio51Minus1, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		ex <- paste0(bio51, " = ", bio51Minus1, " + 1")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio51)
		names(out)[length(out)] <- "bio51"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO52: Driest month.
	if (52 %in% bios) {

		bio52 <- .makeSourceName("bioclims_bio52", "raster")
		bio52Minus1 <- .makeSourceName("bioclims_bio52Minus1", "raster")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		rgrass::execGRASS(
			"r.series",
			input = ppt,
			output = bio52Minus1,
			method = "min_raster",
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

		if (faster("clean")) on.exit(.rm(bio52Minus1, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		ex <- paste0(bio52, " = ", bio52Minus1, " + 1")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		out <- c(out, bio52)
		names(out)[length(out)] <- "bio52"

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

	}

	# BIO57 and BIO58
	if (any(c(57, 58) %in% bios)) {

		# difference between temperature of each month and the next
		deltaTmeanSrcs <- .makeSourceName("bioclims_delta_temp", "raster", nLayers)
		for (i in seq_len(nLayers)) {

			iNext <- if (i == nLayers) { 1L } else { i + 1L }
			ex <- paste0(deltaTmeanSrcs[i], " = ", tmean[i], " - ", tmean[iNext])
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		}
		if (faster("clean")) on.exit(.rm(deltaTmeanSrcs, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		# BIO57: The largest *decrease* in temperature from one month to the next
		if (57 %in% bios) {
			
			bio57 <- .makeSourceName("bioclims_bio57", "raster")
			rgrass::execGRASS(
				"r.series",
				input = deltaTmeanSrcs,
				output = bio57,
				method = "maximum",
				nprocs = faster("cores"),
				memory = faster("memory"),
				flags = c(.quiet(), "overwrite", "n")
			)

			out <- c(out, bio57)
			names(out)[length(out)] <- "bio57"

			tasks <- tasks + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		}

		# BIO58: The largest *increase* in temperature from one month to the next
		if (58 %in% bios) {
			
			src <- .makeSourceName("bioclims_bio58_delta", "raster")
			rgrass::execGRASS(
				"r.series",
				input = deltaTmeanSrcs,
				output = src,
				method = "minimum",
				nprocs = faster("cores"),
				memory = faster("memory"),
				flags = c(.quiet(), "overwrite", "n")
			)

			bio58 <- .makeSourceName("bioclims_bio58", "raster")
			ex <- paste0(bio58, " = -1 * ", src)
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

			out <- c(out, bio58)
			names(out)[length(out)] <- "bio58"

			tasks <- tasks + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		}

	}

	# BIO59 and BIO60: The largest decrease/increase in precipitation from one month to the next
	if (any(c(59, 60) %in% bios)) {

		# difference between precip of each month and the next
		deltaPptSrcs <- .makeSourceName("bioclims_delta_ppt", "raster", nLayers)
		for (i in seq_len(nLayers)) {

			iNext <- if (i == nLayers) { 1L } else { i + 1L }
			ex <- paste0(deltaPptSrcs[i], " = ", ppt[i], " - ", ppt[iNext])
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
		}
		if (faster("clean")) on.exit(.rm(deltaPptSrcs, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

		tasks <- tasks + 1
		if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		# BIO59: The largest decrease in precipitation from one month to the next
		if (59 %in% bios) {
			
			bio59 <- .makeSourceName("bioclims_bio59", "raster")
			rgrass::execGRASS(
				"r.series",
				input = deltaPptSrcs,
				output = bio59,
				method = "maximum",
				nprocs = faster("cores"),
				memory = faster("memory"),
				flags = c(.quiet(), "overwrite", "n")
			)

			out <- c(out, bio59)
			names(out)[length(out)] <- "bio59"

			tasks <- tasks + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		}

		# BIO60: The largest increase in precipitation from one month to the next
		if (60 %in% bios) {
			
			src <- .makeSourceName("bioclims_bio60_delta", "raster")
			rgrass::execGRASS(
				"r.series",
				input = deltaPptSrcs,
				output = src,
				method = "minimum",
				nprocs = faster("cores"),
				memory = faster("memory"),
				flags = c(.quiet(), "overwrite", "n")
			)

			bio60 <- .makeSourceName("bioclims_bio60", "raster")
			ex <- paste0(bio60, " = -1 * ", src)
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

			out <- c(out, bio60)
			names(out)[length(out)] <- "bio60"

			tasks <- tasks + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

		}

	}

	# sort BIOCLIMs
	outNames <- names(out)
	nc <- nchar(outNames)
	shorts <- out[nc == 4L]
	longs <- out[nc == 5L]

	shorts <- shorts[order(names(shorts))]
	longs <- longs[order(names(longs))]

	out <- c(shorts, longs)
	
	# return
	names <- names(out)
	out <- .makeGRaster(out, names = names)

	if (verbose | faster("verbose")) {
		tasks <- tasks + 1
		utils::setTxtProgressBar(pb, tasks)
		close(pb)
	}
	out


	} # EOF
)


#' Calculate a series of "quarter" rasters and return their [sources()] names. A quarter can be summarized using 
#'
#' x Character vector of raster [sources()] names (usually 12 or 52)
#' fun Character: Name of function to summarize across a quarter. Allowed values are "sum", "min", "max", or "mean".
#' quarter Numeric: Length of a quarter
#'
#' @noRd
.calcQuarterGR <- function(x, fun, quarter) {

	qs <- c(x, x[1L:(quarter - 1L)])
	n <- length(x)
	srcs <- .makeSourceName("r_mapcalc", "raster", n = n)
	for (i in seq_len(n)) {

		select <- qs[i:(i + quarter - 1L)]

		method <- if (fun == "sum") {
			"sum"
		} else if (fun == "min") {
			"minimum"
		} else if (fun == "max") {
			"maximum"
		} else if (fun == "mean") {
			"average"
		}

		rgrass::execGRASS(
			"r.series",
			input = select,
			output = srcs[i],
			method = method,
			nprocs = faster("cores"),
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite", "n")
		)

	} # next quarter
	srcs

}
