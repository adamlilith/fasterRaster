#' Pearson's chi-squared for two or more integer of categorical rasters
#'
#' @description This function takes two or more integer or categorical (factor) `GRaster`s and compares them using a Chi-squared test using [stats::chisq.test()].
#'
#' @param x A `GRaster` with one or more layers, each of which must be have cells that represent integers or categories (factors). If at least one cell in a pair of rasters has `NA`, then the cell will be ignored.
#'
#' @param ... Either missing or integer/categorical (factor) `GRaster`s.
#' 
#' @param correct Logical: If `TRUE` (default), then apply continuity correction when computing the test statistic for 2 by 2 tables: one half is subtracted from all |*O - E*| differences; however, the correction will not be bigger than the differences themselves. No correction is performed if `simulate.p.value = TRUE`.
#' 
#' @param simulate.p.value Logical: If `TRUE`, then the *p*-value will be estimated by Monte Carlo simulation, using [stats::chisq.test()]]. This is recommended when there are many categories, because the chi-squared distribution is not a good approximation of the distribution of the test statistic in this case. The default is `FALSE` because simulating *p*-values can be time-consuming.
#'
#' @param B Numeric or integer: Number of replicates for Monte Carlo simulation when `simulate.p.value` is `TRUE`. The default is 2000.
#'
#' @param verbose Logical: If `TRUE`, then display progress. Default is `FALSE`.
#'
#' @returns A list with two or three elements: `chisq` is a matrix of chi-squared values for each pairwise comparison; `df` is a matrix of degrees of freedom for each pairwise comparison (only included if `simulate.p.values = FALSE`); and `p.value` is a matrix of *p*-values for each pairwise comparison.
#'
#' @example man/examples/ex_layerChiSq.r
#'
#' @seealso [layerCor()]
#'
#' @aliases layerChiSq
#' @rdname layerChiSq
#' @exportMethod layerChiSq
methods::setMethod(
	f = "layerChiSq",
	signature = c(x = "GRaster"),
	function(x, ..., correct = TRUE, simulate.p.value = FALSE, B = 2000, verbose = FALSE) {

	.locationRestore(x)
	.region(x)

	dots <- list(...)
	if (length(dots) > 0L) {
		dots <- omnibus::unlistRecursive(dots)
		for (i in seq_along(dots)) {
			x <- c(x, dots[[i]])
		}
	}

	nLayers <- nlyr(x)
	if (!all(datatype(x) %in% c("factor", "integer"))) stop("All layers must be of type factor or integer.")

	chisq <- df <- p.value <- matrix(NA_real_, nLayers, nLayers)
	rownames(chisq) <- colnames(chisq) <- colnames(p.value) <- rownames(p.value) <- names(x)
	
	nTasks <- 0.5 * nLayers * (nLayers - 1L)
	if (verbose | faster("verbose")) {
		tasks<- 0
		pb <- utils::txtProgressBar(min = 0, max = nTasks, initial = 0, style = 3, width = 30)
	}

	for (i in seq_len(nLayers - 1L)) {

		srcOne <- sources(x[[i]])
		
		for (j in seq(i + 1L, nLayers)) {
			
			tasks <- tasks + 1
			if (verbose | faster("verbose")) utils::setTxtProgressBar(pb, tasks)

			### calculate frequencies of values across all pairs of cells
			srcTwo <- sources(x[[j]])
			
			outFile <- paste0(.workDir(), "/contingency_", omnibus::rstring(1L), ".csv")
			args <- list(
				cmd = "r.stats",
				input = c(srcOne, srcTwo),
				output = outFile,
				separator = "comma",
				flags = c(.quiet(), "overwrite", "c", "n") # c = counts, n = ignore NULLs
			)
			do.call(rgrass::execGRASS, args = args)

			combos <- data.table::fread(outFile, header = FALSE, col.names = c("one", "two", "observed"))

			### frequencies of values in each raster
			freqsOne <- .freq(x = srcOne, dtype = "CELL")
			freqsTwo <- .freq(x = srcTwo, dtype = "CELL")

			# create contingency table
			combos[ , "expected" := NA_real_]

			nCells <- sum(combos$observed)

			for (k in seq_len(nrow(combos))) {

				thisOne <- combos$one[k]
				thisTwo <- combos$two[k]

				combos$expected[k] <- (freqsOne$count[freqsOne$value == thisOne] * freqsTwo$count[freqsTwo$value == thisTwo]) / nCells

			}

			chi2 <- stats::chisq.test(
				combos[["observed"]],
				p = combos[["expected"]],
				correct = correct,
				rescale.p = TRUE,
				simulate.p.value = simulate.p.value,
				B = B
			)

			chisq[i, j] <- chisq[j, i] <- chi2$statistic
			if (!simulate.p.value) df[i, j] <- df[j, i] <- unname(chi2$parameter)
			p.value[i, j] <- p.value[j, i] <- chi2$p.value

		} # next second raster

	} # next first raster

	if (verbose | faster("verbose")) {
		tasks <- tasks + 1
		utils::setTxtProgressBar(pb, tasks)
		close(pb)
	}

	if (!simulate.p.value) {
		list(chisq = chisq, df = df, p.value = p.value)
	} else {
		list(chisq = chisq, p.value = p.value)
	}

	} # EOF
)
