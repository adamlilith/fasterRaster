#' Correlation between GRasters
#'
#' @description Calculate the correlation, covariance, or chi-squared, Cramer's *V*, or Kruskal-Wallis's *H* between two or more rasters. Note that cells that are `NA` for any raster are ignored across all rasters.
#' 
#' @param x A `GRaster` with two or more layers. Partial matching is used and capitalization ignored.
#' @param fun Character: Name of the statistic(s) to calculate:
#' * `"cor"` (default): Pearson sample correlation (i.e., the denominator is `n - 1`). Appropriate for numeric-numeric raster comparisons.
#' * `"cov"`: Covariance.
#' * `"chisq"`: Chi-squared test and Cramer's *V*. Default for integer-integer, factor-factor, or integer-factor raster comparisons.
#' * `"kw"`: Kruskal-Wallis *H* statistic. Appropriate for integer-numeric or factor-numeric raster comparisons.
#' * `"auto"`: Automatically select the appropriate statistic based on the data types of the rasters and calculates the (approximate) effect size and (where necessary) rescale to the range from 0 to 1. If any two rasters are numeric, then the absolute value of the Pearson or Spearman correlation will be calculated (depending on argument `cor`). If any two rasters are integer or factor, then Cramer's *V* will be returned. If any two are numeric and factor/integer, then the square root of the proportion of variance explained by the Kruskal-Wallis test will be returned. This is `sqrt(H / (N - 1)`, where `H` is the KW test statistic and `N` the number of non-`NA` cells.
#' 
#' @param cor Either 'pearson' (default) or 'spearman'. Only used if `fun = "cor"` or `fun = "auto"` and any two rasters are numeric. Indicates the type of correlation statistic to calculate. Capitalization is ignored and partial matching is used.
#' 
#' @param integerAsNumeric Logical: If `TRUE` (default), then treat integer rasters as numeric. This is useful for rasters that are stored as integers but are actually continuous variables. If `FALSE`, then treat integer rasters as categorical variables. Only applicable for Kruskal-Wallis test or when `cor = 'auto'`.
#' 
#' @param correct Logical (only used if `fun = "chisq"`): If `TRUE` (default), then apply continuity correction when computing the test statistic for 2 by 2 tables: one half is subtracted from all |*O - E*| differences; however, the correction will not be bigger than the differences themselves. No correction is performed if `simulate = TRUE`.
#' 
#' @param simulate Logical (only used if `fun = "chisq"`): If `TRUE`, then the *p*-value will be estimated by Monte Carlo simulation, using [stats::chisq.test()]]. This is recommended when there are many categories, because the chi-squared distribution is not a good approximation of the distribution of the test statistic in this case. The default is `FALSE` because simulating *p*-values can be time-consuming.
#'
#' @param nSim Numeric or integer (only used if `fun = "chisq"`): Number of replicates for Monte Carlo simulation when `simulate` is `TRUE`. The default is 2000.
#' 
#' @param na.rm Logical: If `TRUE` (default), then remove cells with `NA` values in any raster. If `FALSE`, then pairwise comparisons of rasters will use all pairs of non-`NA` cells, even if they are `NA` in other `GRaster`s.
#' 
#' @param verbose Logical: If `TRUE`, then display progress. Default is `FALSE`. Ignored for some values of `fun`.
#' 
#' @returns The output depends on the selected statistic:
#' * `"cor"`: A correlation `matrix`. An attribute "`n`" gives the number of cells used in each pairwise correlation.
#' * `"cov"`: A covariance `matrix`.
#' * `"chisq"`: A `list` with five or six elements:
#'    * `chisq`: A `matrix` of chi-squared values for each pairwise comparison;
#'    * `df`: A `matrix` of degrees of freedom for each pairwise comparison (only included if `simulate = FALSE`);
#'    * `p.value`: A `matrix` of *p*-values for each pairwise comparison. Note that most rasters have so many cells that even very small differences create very small *p*-values, so do not get too excited;
#'    * `nCats1`: A `matrix` with the number of categories/integer values for each raster in each pairwise comparison;
#'    * `nCats2`: A `matrix` with the number of categories/integer values for each raster in each pairwise comparison;
#'    * `nCells`: A `matrix` with the number of cells used in each pairwise comparison.
#' * `"kw"`: A matrix with *H* values for each comparison.
#' * `"auto"`: A `list` with these elements:
#'   * `stat`: A character vector with the name of the statistic calculated for each pairwise comparison.
#'   * `relative.effect.size`: A numeric matrix with the relative effect size of each comparison. Values are in the range of 0 to 1.
#' @example man/examples/ex_layerCor.r
#'
#' @seealso [terra::layerCor()], [stats::cor()], [stats::cov()]
#' 
#' @aliases layerCor
#' @rdname layerCor
#' @exportMethod layerCor
methods::setMethod(
	f = "layerCor",
	signature = c(x = "GRaster"),
	function(
		x,
		fun = "cor",
		cor = "Pearson",
		correct = TRUE,
		simulate = FALSE,
		nSim = 2000,
		integerAsNumeric = TRUE,
		na.rm = TRUE,
		verbose = FALSE
	) {

	# for debugging
	if (FALSE) {

		fun <- "auto"
		cor <- "Pearson"
		correct <- TRUE
		simulate <- FALSE
		nSim <- 2000
		integerAsNumeric <- TRUE
		verbose <- TRUE
		na.rm <- TRUE

	}

	if (nlyr(x) == 1L) stop("The raster must have >= 2 layers.")
	fun <- omnibus::pmatchSafe(fun, c("cor", "cov", "chisq", "kw", "auto"))
	
	.locationRestore(x)
	.region(x)

	nLayers <- nlyr(x)

	### mask NAs
	############

	# mask to non-NA cells
	if (na.rm) {
	
		if (verbose | faster("verbose")) omnibus::say("Forcing all cells to NA where any raster has an NA in that cell...")

		x <- na.omit(x)
		on.exit(.rm(x, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)
	
	}

	### correlation/covariance
	##########################
	if (fun %in% c("cor", "cov")) {

		cor <- omnibus::pmatchSafe(tolower(cor), c("pearson", "spearman"), error = FALSE)

		# covariance
		if (fun == "cov") {

			args <- list(
				cmd = "r.covar",
				map = paste(sources(x), collapse = ","),
				flags = c(.quiet()),
				intern = TRUE
			)

			info <- do.call(rgrass::execGRASS, args = args)

			n <- substr(info[1L], 5L, nchar(info[1L]))
			n <- as.numeric(n)

			out <- matrix(NA_real_, ncol = nLayers, nrow = nLayers, dimnames = list(names(x), names(x)))

			for (i in seq_len(nLayers)) {

				this <- info[i + 1L]
				this <- strsplit(this, " ")[[1]]
				this <- as.numeric(this)
				out[i, ] <- this

			}
			attr(out, "n") <- n
		
		} else if (cor == "pearson") { # Pearson correlation

			args <- list(
				cmd = "r.covar",
				map = paste(sources(x), collapse = ","),
				flags = c(.quiet(), "r"),
				intern = TRUE
			)

			if (fun == "cor") args$flags <- c(args$flags, "r")
			info <- do.call(rgrass::execGRASS, args = args)

			n <- substr(info[1L], 5L, nchar(info[1L]))
			n <- as.numeric(n)

			out <- matrix(NA_real_, ncol = nLayers, nrow = nLayers, dimnames = list(names(x), names(x)))
			diag(out) <- 1.0

			for (i in seq_len(nLayers)) {

				this <- info[i + 1L]
				this <- strsplit(this, " ")[[1]]
				this <- as.numeric(this)
				out[i, ] <- this

			}
			attr(out, "n") <- n
		
		} else if (cor == "spearman") { # Spearman correlation

			out <- matrix(NA_real_, nLayers, nLayers, dimnames = list(names(x), names(x)))
			diag(out) <- 1.0

			if (verbose | faster("verbose")) {
				nTasks <- 0.5 * nLayers * (nLayers - 1L)
				tasks <- 0
				pb <- utils::txtProgressBar(min = 0, max = nTasks, initial = 0, style = 3, width = 30)
			}

			# for each primary raster
			for (i in seq_len(nLayers - 1L)) {

				if (verbose | faster("verbose")) {
					tasks <- tasks + 1
					utils::setTxtProgressBar(pb, tasks)
				}

				# for each secondary raster
				for (j in seq(i + 1L, nLayers)) {

					vals <- as.data.table(x[[c(i, j)]], xy = FALSE, na.rm = TRUE)
					out[i, j] <- out[j, i] <- stats::cor(vals[[1L]], vals[[2L]], method = "spearman")

				} # next 2nd layer

			} # next 1st layer

			if (verbose | faster("verbose")) {
				tasks <- tasks + 1
				utils::setTxtProgressBar(pb, tasks)
				close(pb)
			}

		} # if Spearman correlation

	} # if correlation/covariance
	
	### chi-squared
	###############
	if (fun == "chisq") {

		isFactor <- datatype(x) == "factor"
		isInteger <- datatype(x) == "integer"
		isFactorOrInteger <- all(isFactor | isInteger)

		if (!isFactorOrInteger) stop("Layers should be of type factor and/or integer.")

		# tabulate cross frequencies
		xFreqs <- crossFreq(x, na.rm = FALSE, cats = TRUE, verbose = FALSE)

		### for each raster pair, calculate chi2 and other statistics
		if (!inherits(xFreqs, "list")) xFreqs <- list(xFreqs)

		chisq <- df <- p.value <- cramer.v <- matrix(NA_real_, nLayers, nLayers, dimnames = list(names(x), names(x)))

		# for each table in cross frequencies (each pair of rasters), calculate statistics
		for (i in seq_along(xFreqs)) {

			xFreq <- xFreqs[[i]]

			# names/indices of these rasters in outputs
			rast1 <- names(xFreq)[1L]
			rast2 <- names(xFreq)[2L]

			index1 <- which(colnames(chisq) == rast1)
			index2 <- which(colnames(chisq) == rast2)

			# contingency table
			uniques1 <- unique(xFreq[[1L]])
			uniques2 <- unique(xFreq[[2L]])
			contig <- expand.grid(one = uniques1, two = uniques2)
			contig <- data.table::as.data.table(contig)
			contig[ , c("observed", "expected") := NA_real_]
			
			# populate contingency table
			nTotal <- sum(xFreq[["count"]])
			for (combo in 1L:nrow(contig)) {
				
				thisOne <- contig$one[combo]
				thisTwo <- contig$two[combo]
				nObs <- xFreq$count[xFreq[[1L]] == contig$one[combo] & xFreq[[2L]] == contig$two[combo]]
				if (length(nObs) == 0L) nObs <- 0
				contig$observed[combo] <- nObs

				nOne <- sum(xFreq[["count"]][xFreq[[1L]] == thisOne])
				nTwo <- sum(xFreq[["count"]][xFreq[[2L]] == thisTwo])

				contig$expected[combo] <- round((nOne / nTotal) * (nTwo / nTotal) * nTotal)

			} # next set of pairwise values inn each raster

			# chi^2
			chi2 <- stats::chisq.test(
				contig$observed,
				p = contig$expected,
				correct = correct,
				rescale.p = TRUE,
				simulate = simulate,
				B = nSim
			)

			c2 <- chi2$statistic
			chisq[index1, index2] <- chisq[index2, index1] <- c2
			if (!simulate) df[index1, index2] <- df[index2, index1] <- unname(chi2$parameter)
			p.value[index1, index2] <- p.value[index2, index1] <- chi2$p.value

			# Cramer's V
			nCats <- sum(xFreq[["count"]])
			minDims <- min(length(unique(xFreq[[1L]])), length(unique(xFreq[[2L]])))
			v <- sqrt(c2 / (nCats * (minDims - 1)))
			cramer.v[index1, index2] <- cramer.v[index2, index1] <- v

		} # next pairwise raster combination

		if (!simulate) {
			out <- list(chisq = chisq, df = df, p.value = p.value, cramer.v = cramer.v)
		} else {
			out <- list(chisq = chisq, p.value = p.value, cramer.v = cramer.v)
		}

	} # if Chi-squared/Cramer's V

	### Kruskal-Wallis
	##################
	if (fun == "kw") {

		dt <- datatype(x)

		okSet <- c("float", "double")
		if (integerAsNumeric) okSet <- c(okSet, "integer")
		isNumeric <- any(datatype(x) %in% okSet)
		isFactor <- any(is.factor(x))
		ok <- all(isNumeric | isFactor)

		if (nlyr(x) > 2L | !ok) stop("The Kruskal-Wallis test is only calculated for two layers at a time. Valid combinations of rasters include:\n     * factor versus float or double;\n     * factor versus integer (`integerAsNumeric` set to `TRUE`); or\n     * integer versus float or double (`integerAsNumeric` set to `FALSE`).")
		
		kw <- df <- p.value <- effectSize <- nCells <-
			matrix(NA_real_, nLayers, nLayers, dimnames = list(names(x), names(x)))

		if (verbose | faster("verbose")) {
			nTasks <- 0.5 * nLayers * (nLayers - 1L)
			tasks<- 0
			pb <- utils::txtProgressBar(min = 0, max = nTasks, initial = 0, style = 3, width = 30)
		}

		for (i in seq_len(nLayers - 1L)) {
			for (j in seq(i + 1L, nLayers)) {
				
				if (verbose | faster("verbose")) {
					tasks <- tasks + 1
					utils::setTxtProgressBar(pb, tasks)
				}

				# sample size
				srcOne <- sources(x[[i]])
				srcTwo <- sources(x[[j]])

				# flags: -1 ==> one line per cell, n = ignore NULLs, -N = do not return cells where all rasters have NAs
				outFile <- paste0(.workDir(), '/cell_values_', omnibus::rstring(1L), '.csv')
				args <- list(
					cmd = "r.stats",
					input = c(srcOne, srcTwo),
					output = outFile,
					separator = "comma",
					flags = c(.quiet(), "overwrite", "n", "N", "1")
				)
				do.call(rgrass::execGRASS, args = args)

				columnNames <- if (datatype(x[[i]]) %in% c("factor", "integer")) c("discrete", "continuous") else c("continuous", "discrete")
				vals <- data.table::fread(outFile, col.names = columnNames)

				vals[ , "discrete" := as.factor(discrete)]

				# KW test
				kw_result <- stats::kruskal.test(continuous ~ discrete, data = vals)

				kw[i, j] <- kw[j, i] <- kw_result$statistic
				df[i, j] <- df[j, i] <- kw_result$parameter
				p.value[i, j] <- p.value[j, i] <- kw_result$p.value
				nCells[i, j] <- nCells[j, i] <- nrow(vals)
				effectSize[i, j] <- effectSize[j, i] <- kw_result$statistic / (nrow(vals) - 1)
			
			}
		}

		out <- list(kw = kw, df = df, p.value = p.value, effectSize = effectSize, nCells = nCells)
		
		if (verbose | faster("verbose")) {
			tasks <- tasks + 1
			utils::setTxtProgressBar(pb, tasks)
			close(pb)
		}

	} # if Kruskal-Wallis

	### if automatic
	################

	if (fun == "auto") {

		stat <- matrix(NA_character_, nLayers, nLayers, dimnames = list(names(x), names(x)))
		values <- matrix(NA_real_, nLayers, nLayers, dimnames = list(names(x), names(x)))

		if (verbose | faster("verbose")) {

			nTasks <- 0.5 * nLayers * (nLayers - 1L)
			tasks <- 0
			pb <- utils::txtProgressBar(min = 0, max = nTasks, initial = 0, style = 3, width = 30)

			if (faster("verbose")) {
				fasterVerbose <- TRUE
				faster(verbose = FALSE)
			} else {
				fasterVerbose <- FALSE
			}

		} else { 
			fasterVerbose <- FALSE
		}

		numericSet <- c("float", "double")
		factorSet <- c("integer", "factor")
		if (integerAsNumeric) {
			numericSet <- c(numericSet, "integer")
		} else {
			factorSet <- c(factorSet, "integer")
		}

		for (i in seq_len(nLayers - 1L)) {
			for (j in seq(i + 1L, nLayers)) {

				if (verbose | faster("verbose")) {
					tasks <- tasks + 1
					utils::setTxtProgressBar(pb, tasks)
				}

				dt <- c(datatype(x[[i]]), datatype(x[[j]]))

				if (all(dt %in% numericSet)) {

					stat[i, j] <- stat[j, i] <- "cor"
					val <- layerCor(x[[c(i, j)]], fun = "cor", verbose = FALSE)[1L, 2L]
					val <- abs(val)

				} else if (all(dt %in% factorSet)) {

					stat[i, j] <- stat[j, i] <- "chisq"
					val <- layerCor(x[[c(i, j)]], fun = "chisq", correct = correct, simulate = simulate, nSim = nSim, verbose = FALSE)$cramer.v[1L, 2L]

				} else if (any(dt %in% numericSet) & any(dt %in% factorSet)) {

					stat[i, j] <- stat[j, i] <- "kw"
					val <- layerCor(x[[c(i, j)]], fun = "kw", integerAsNumeric = integerAsNumeric, verbose = FALSE)$effectSize[1L, 2L]
					val <- sqrt(val)

				}

				values[i, j] <- values[j, i] <- val

			}
		}

		if (verbose | faster("verbose")) {
			tasks <- tasks + 1
			utils::setTxtProgressBar(pb, tasks)
			close(pb)
			if (fasterVerbose) faster(verbose = TRUE)
		}

		out <- list(stat = stat, relative.effect.size = values)

	} # if automatic

	out	
	
	} # EOF
)
