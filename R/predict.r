#' Make predictions from a linear or generalized linear model to a GRaster
#'
#' @description This version of the `predict()` function make predictions to a set of `GRaster`s from a model object.
#'
#' The model must be either a linear model, which is of class `lm` and typically created using the [stats::lm()] function or a generalized linear model (GLM), which is class `glm` and typically created using [stats::glm()]. Other packages can also create `lm` or `glm` objects, but they will probably not work in this function. For example, generalized additive models, which can be created using the `gam()` function in the **mgcv** package, inherit the `glm` class, but cannot be used in this function.
#'
#' @param object A `GRaster` with one or more layers.
#'
#' @param model An `lm` or `glm` model object.
#'
#' @param type Character: Type of prediction to make. This can be either `link` (default; predictions are made on the scale of the link function) or `response` (predictions are made on the scale of the response variable). This function can only make predictions on the scale of the response for the identity, logit, log, or cloglog (complementary log-log) link functions.
#'
#' @returns A `GRaster`.
#'
#' @seealso [terra::predict()]; [stats::predict()]
#'
#' @example man/examples/ex_predict.r
#'
#' @aliases predict
#' @rdname predict
#' @exportMethod predict
methods::setMethod(
	f = "predict",
	signature = c(object = "GRaster"),
	function(object, model, type = "response") {
	
	type <- omnibus::pmatchSafe(type, c("link", "response"), nmax = 1L)

	.locationRestore(object)
	.region(object)

	### parse model to GRASS r.mapcalc format
	coeffs <- stats::coef(model)

	terms <- data.frame(
		term = names(coeffs),
		value = coeffs,
		termClean = NA_character_,
		row.names = seq_along(coeffs)
	)

	terms$term <- gsub(terms$term, pattern = "I\\(", replacement = "")
	terms$term <- gsub(terms$term, pattern = "\\(", replacement = "")
	terms$term <- gsub(terms$term, pattern = "\\)", replacement = "")
		

	i <- which(terms$term == "Intercept")
	if (length(i) > 0L) {
		terms$termClean[i] <- as.character(terms$value[i])
	}

	# interaction terms
	i <- which(grepl(terms$term, pattern = ":"))
	if (length(i) > 0L) {
	
		for (j in i) {
		
			term <- terms$term[j]
			term <- strsplit(term, split = ":")[[1L]]
			term <- .matchRasterNamesToFormula(object, term)
			term <- paste(term, collapse = " * ")
			term <- paste(terms$value[j], ' * ', term)
			terms$termClean[j] <- term
		
		}
	
	}

	i <- which(is.na(terms$termClean))
	if (length(i) > 0L) {
	
		for (j in i) {
		
			term <- .matchRasterNamesToFormula(object, terms$term[j])

			term <- paste(terms$value[j], ' * ', term)

			terms$termClean[j] <- term

		}
	
	}
	
	ex <- paste(terms$termClean, collapse = " + ")
	src <- .makeSourceName("r_mapcalc", "raster")
	ex <- paste0(src, " = ", ex)

	rgrass::execGRASS(
		cmd = "r.mapcalc",
		expression = ex,
		flags = c(.quiet(), "overwrite")
	)

	# return prediction on response scale
	fam <- stats::family(model)
	link <- fam$link

	if (type == "response" & link != "identity") {
	
		srcIn <- src
		src <- .makeSourceName("r_mapcalc", "raster")

		if (link == "log") {
			ex <- paste0(src, " = exp(", srcIn, ")")
		} else if (link == "logit") {
			ex <- paste0(src, " = exp(", srcIn, ") / (1 + exp(", srcIn, "))")
		} else if (link == "cloglog") {
			ex <- paste0(src, " = 1 - exp(-1 * exp(", srcIn, "))")
		} else {
			stop("The ", link, " link function is not supported.")
		}
	
		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)

	}

	.makeGRaster(src, "prediction")

	} # EOF
)

#' @param object `GRaster` stack.
#' @param term Character vector to which to match names.
#'
#' @returns A character vector same length as `x` with [sources()] names.
#'
#' @noRd
.matchRasterNamesToFormula <- function(object, term) {

	names <- names(object)
	srcs <- sources(object)
	nc <- nchar(names)
	names <- names[order(nc)]
	srcs <- srcs[order(nc)]

	matched <- rep(FALSE, length(term))
	for (i in seq_along(names)) {
		term <- gsub(term, pattern = names[i], replacement = srcs[i])
	}
	term

}
