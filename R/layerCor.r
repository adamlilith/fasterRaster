#' Correlation between GRasters
#'
#' @description Calculate the correlation, covariance, or chi-squared, Cramer's *V*, or Kruskal-Wallis's *H* between two or more rasters.
#' 
#' @param x A `GRaster` with two or more layers. Partial matching is used and capitalization ignored.
#' @param fun Character: Name of the statistic(s) to calculate:args
#' * `"cor"` (default): Pearson sample correlation (i.e., the denominator is `n - 1`). Appropriate for numeric-numeric raster comparisons.
#' * `"cov"`: Covariance.
#' * `"chisq"`: Chi-squared test and Cramer's *V*. Default for integer-integer, factor-factor, or integer-factor raster comparisons.
#' * `"kw"`: Kruskal-Wallis *H* statistic. Appropriate for integer-numeric or factor-numeric raster comparisons.
#' * `"auto"`: Automatically select the appropriate statistic based on the data types of the rasters and calculates the (approximate) effect size and (where necessary) rescale to the range from 0 to 1. If any two rasters are numeric, then the absolute value of the Pearson or Spearman correlation will be calculated (depending on argument `cor`). If any two rasters are integer or factor, then Cramer's *V* will be returned. If any two are numeric and factor/integer, then the proportion of variance explained by the Kruskal-Wallis test will be returned. This is estimated as `sqrt(H / (N - 1)`, where `H` is the KW test statistic and `N` the number of non-`NA` cells.
#' 
#' @param cor Either 'pearson' (default) or 'spearman'. Only used if `fun = "cor"` or `fun = "auto"` and any two rasters are numeric. Indicates the type of correlation statistic to calculate. Capitalization is ignored and partial matching is used.
#' 
#' @param correct Logical (only used if `fun = "chisq"` or `"cramer.v"`): If `TRUE` (default), then apply continuity correction when computing the test statistic for 2 by 2 tables: one half is subtracted from all |*O - E*| differences; however, the correction will not be bigger than the differences themselves. No correction is performed if `simulate = TRUE`.
#' 
#' @param simulate Logical (only used if `fun = "chisq"` or `"cramer.v"`): If `TRUE`, then the *p*-value will be estimated by Monte Carlo simulation, using [stats::chisq.test()]]. This is recommended when there are many categories, because the chi-squared distribution is not a good approximation of the distribution of the test statistic in this case. The default is `FALSE` because simulating *p*-values can be time-consuming.
#'
#' @param nSim Numeric or integerLogical (only used if `fun = "chisq"` or `"cramer.v"`): Number of replicates for Monte Carlo simulation when `simulate` is `TRUE`. The default is 2000.
#'
#' @param verbose Logical: If `TRUE`, then display progress. Default is `FALSE`. Ignored for some values of `fun`.
#' 
#' @returns The output depends on the selected statistic:
#' * `"cor"`: A correlation `matrix`. An attribute "`n`" gives the number of cells used in each pairwise correlation.
#' * `"cov"`: A covariance `matrix`.
#' * `"chisq"`: A `list` with five or six elements:
#'    * `chisq`: A `matrix` of chi-squared values for each pairwise comparison;
#'    * `df`: A `matrix` of degrees of freedom for each pairwise comparison (only included if `simulate = FALSE`);
#'    * `p.value`: A `matrix` of *p*-values for each pairwise comparison;
#'    * `nCats1`: A `matrix` with the number of categories/integer values for each raster in each pairwise comparison;
#'    * `nCats2`: A `matrix` with the number of categories/integer values for each raster in each pairwise comparison;
#'    * `nCells`: A `matrix` with the number of cells used in each pairwise comparison.
#'    * `"cramer.v"`: A `matrix` with Cramer's *V* values for each pairwise comparison. *V* ranges from 0 to 1 and indicates the degree of association ("correlation") between categorical rasters. Values of ~0.1 indicate a weak association, values of ~0.3 indicate moderate association, and values of ~0.5 or higher indicate strong association.
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
	function(x, fun = "cor", cor = "Pearson", correct = TRUE, simulate = FALSE, nSim = 2000, verbose = FALSE) {
	
	if (nlyr(x) == 1L) stop("The raster must have >= 2 layers.")
	fun <- omnibus::pmatchSafe(fun, c("cor", "cov", "chisq", "cramer.v", "kw", "auto"))
	
	.locationRestore(x)
	.region(x)

	nLayers <- nlyr(x)

	### correlation/covariance
	##########################
	if (fun %in% c("cor", "cov")) {

		cor <- match.arg(tolower(cor), c("pearson", "spearman"))

		if (cor == "pearson") {

			args <- list(
				cmd = "r.covar",
				map = paste(sources(x), collapse = ","),
				flags = c(.quiet()),
				intern = TRUE
			)

			if (fun == "cor") args$flags <- c(args$flags, "r")
			info <- do.call(rgrass::execGRASS, args = args)

			n <- substr(info[1L], 5L, nchar(info[1L]))
			n <- as.integer(n)

			out <- matrix(NA_real_, ncol = nLayers, nrow = nLayers, dimnames = list(names(x), names(x)))

			for (i in seq_len(nLayers)) {

				this <- info[i + 1L]
				this <- strsplit(this, " ")[[1]]
				this <- as.numeric(this)
				out[i, ] <- this

			}
			attr(out, "n") <- n
		
		} else if (cor == "spearman") {

			out <- matrix(NA_real_, nLayers, nLayers, dimnames = list(names(x), names(x)))
			diag(out) <- 1.00

			if (verbose | faster("verbose")) {
				nTasks <- 0.5 * nLayers * (nLayers - 1L)
				tasks <- 0
				pb <- utils::txtProgressBar(min = 0, max = nTasks, initial = 0, style = 3, width = 30)
			}

			for (i in seq_len(nLayers - 1L)) {

				if (verbose | faster("verbose")) {
					tasks <- tasks + 1
					utils::setTxtProgressBar(pb, tasks)
				}

				srcOne <- sources(x[[i]])
				
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

		if (!all(datatype(x) %in% c("factor", "integer"))) warning("Layers should be of type factor or integer.")

		chisq <- df <- p.value <- nCats1 <- nCats2 <- nCells <- matrix(NA_real_, nLayers, nLayers, dimnames = list(names(x), names(x)))

		if (verbose | faster("verbose")) {
			nTasks <- 0.5 * nLayers * (nLayers - 1L)
			tasks <- 0
			pb <- utils::txtProgressBar(min = 0, max = nTasks, initial = 0, style = 3, width = 30)
		}

		for (i in seq_len(nLayers - 1L)) {

			srcOne <- sources(x[[i]])
			
			for (j in seq(i + 1L, nLayers)) {
				
				if (verbose | faster("verbose")) {
					tasks <- tasks + 1
					utils::setTxtProgressBar(pb, tasks)
				}

				srcTwo <- sources(x[[j]])
				### mask rasters so that if either has an NA in a cell, both have an NA
				srcOneMask <- .mask(srcOne, mask = srcTwo, maskType = "raster")
				srcTwoMask <- .mask(srcTwo, mask = srcOneMask, maskType = "raster")

				### calculate frequencies of values across all pairs of cells
				
				outFile <- paste0(.workDir(), "/contingency_", omnibus::rstring(1L), ".csv")
				args <- list(
					cmd = "r.stats",
					input = c(srcOneMask, srcTwoMask),
					output = outFile,
					separator = "comma",
					flags = c(.quiet(), "overwrite", "c", "n", "N") # c = counts, n = ignore NULLs
				)
				do.call(rgrass::execGRASS, args = args)

				combos <- data.table::fread(outFile, header = FALSE, col.names = c("one", "two", "observed"))

				### frequencies of values in each raster
				freqsOne <- .freq(x = srcOneMask, dtype = "CELL")
				freqsTwo <- .freq(x = srcTwoMask, dtype = "CELL")

				nCats1[i, j] <- nCats1[j, i] <- nrow(freqsOne)
				nCats2[i, j] <- nCats2[j, i] <- nrow(freqsTwo)

				# create contingency table
				nc <- sum(combos$observed)
				combos[ , "expected" := NA_real_]

				for (k in seq_len(nrow(combos))) {

					thisOne <- combos$one[k]
					thisTwo <- combos$two[k]

					combos$expected[k] <- (freqsOne$count[freqsOne$value == thisOne] / nc) * (freqsTwo$count[freqsTwo$value == thisTwo] / nc)

				}

				chi2 <- stats::chisq.test(
					combos$observed,
					p = combos$expected,
					correct = correct,
					rescale.p = TRUE,
					simulate = simulate,
					B = nSim
				)

				chisq[i, j] <- chisq[j, i] <- chi2$statistic
				if (!simulate) df[i, j] <- df[j, i] <- unname(chi2$parameter)
				p.value[i, j] <- p.value[j, i] <- chi2$p.value
				nCells[i, j] <- nCells[j, i] <- nc

			} # next second raster

		} # next first raster

		### Cramer's V
		cramer.v <- matrix(NA_real_, nLayers, nLayers, dimnames = list(names(x), names(x)))
		for (i in seq_len(nLayers - 1L)) {
			for (j in seq(i + 1L, nLayers)) {
				
				# sample size
				srcOne <- sources(x[[i]])
				srcTwo <- sources(x[[j]])

				n <- nCells[i, j]

				nCats <- min(nCats1[i, j], nCats2[i, j])
				cramer.v[i, j] <- cramer.v[j, i] <- sqrt(chisq[i, j] / (n * (nCats - 1L)))
			
			}
		}

		if (verbose | faster("verbose")) {
			tasks <- tasks + 1
			utils::setTxtProgressBar(pb, tasks)
			close(pb)
		}

		if (!simulate) {
			out <- list(chisq = chisq, df = df, p.value = p.value, cramer.v = cramer.v, nCats1 = nCats1, nCats2 = nCats2, nCells = nCells)
		} else {
			out <- list(chisq = chisq, p.value = p.value, cramer.v = cramer.v, nCats1 = nCats1, nCats2 = nCats2, nCells = nCells)
		}

	} # if Chi-squared/Cramer's V

	### Kruskal-Wallis
	##################
	if (fun == "kw") {

		dt <- datatype(x)
		if (!any(dt %in% c("factor", "integer")) | !any(dt %in% c("double", "float"))) warning("One layer should be of type factor or integer, and the other should be of type float or double.")
		
		kw <- df <- p.value <- effectSize <- nCells <- matrix(NA_real_, nLayers, nLayers, dimnames = list(names(x), names(x)))

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

		for (i in seq_len(nLayers - 1L)) {
			for (j in seq(i + 1L, nLayers)) {

				if (verbose | faster("verbose")) {
					tasks <- tasks + 1
					utils::setTxtProgressBar(pb, tasks)
				}

				dt <- c(datatype(x[[i]]), datatype(x[[j]]))

				if (all(dt %in% c("float", "double"))) {

					stat[i, j] <- stat[j, i] <- "cor"
					val <- layerCor(x[[c(i, j)]], fun = "cor", verbose = FALSE)[1L, 2L]
					val <- abs(val)

				} else if (all(dt %in% c("integer", "factor"))) {

					stat[i, j] <- stat[j, i] <- "chisq"
					val <- layerCor(x[[c(i, j)]], fun = "chisq", correct = correct, simulate = simulate, nSim = nSim, verbose = FALSE)$cramer.v[1L, 2L]

				} else if (any(dt %in% c("float", "double")) & any(dt %in% c("integer", "factor"))) {

					stat[i, j] <- stat[j, i] <- "kw"
					val <- layerCor(x[[c(i, j)]], fun = "kw", verbose = FALSE)$effectSize[1L, 2L]
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
