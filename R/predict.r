#' Make predictions from a linear or generalized linear model to a GRaster
#'
#' @description This version of the `predict()` function make predictions to a set of `GRaster`s from a model object.
#'
#' The model must be either a linear model, which is of class `lm` and typically created using the [stats::lm()] function or a generalized linear model (GLM), which is class `glm` and typically created using [stats::glm()]. Other packages can also create `lm` or `glm` objects, but they may not work in this function. For example, generalized additive models, which can be created using the `gam()` function in the **mgcv** package, inherit the `glm` class, but cannot be used in this function. However, `glm` objects created with the **speedglm** package should work with this function.
#'
#' This `predict()` function can handle:
#' * Linear predictors and intercepts like `1 + x`;
#' * Quadratic terms like `x^2` (or, in **R** formula notation, `I(x^2)`);
#' * Two-way interaction terms between scalars like `x1:x2` and `x1 * x2`;
#' * Categorical predictors (i.e., categorical `GRaster`s; see `vignette("GRasters", package = "fasterRaster")`);
#' * Two-way interactions between a categorical predictor and a scalar predictor; and
#' * Two-way interactions between categorical predictors.
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
	#########################################

	coeffs <- stats::coef(model)

	terms <- data.table(
		term = names(coeffs),
		value = coeffs,
		termClean = NA_character_
	)

	terms <- terms[!is.na(terms$value)]

	terms$term <- gsub(terms$term, pattern = "I\\(", replacement = "")
	terms$term <- gsub(terms$term, pattern = "\\(", replacement = "")
	terms$term <- gsub(terms$term, pattern = "\\)", replacement = "")

	# intercept
	i <- which(terms$term == "Intercept")
	if (length(i) > 0L) {
		terms$termClean[i] <- as.character(terms$value[i])
	}

	# single factors... assuming we just use levels of factors as intercepts, not in interaction terms
	if (any(is.factor(object))) {

		facts <- which(is.factor(object))

		# use if() statement to obtain offsets due to each level of each factor
		for (fact in facts)	{
		
			levs <- levels(object)[[fact]]
			levs$TEMPTEMP__ <- paste0(names(object)[fact], levs[[2L]])
			levs <- levs[levs[[3L]] %in% names(coeffs), ]
			nLevs <- nrow(levs)

			if (nLevs > 0L) {

				for (j in seq_len(nLevs)) {

					thisTerm <- which(terms$term == levs$TEMPTEMP__[j])
					coeff <- coeffs[names(coeffs) == levs$TEMPTEMP__[j]]

					terms$termClean[thisTerm] <- paste0("if(", sources(object)[fact], "==", levs[[1L]][j], ",", coeff, ",0)")

				}

			}
		
		}
	
	}

	# interactions between factors
	i <- which(is.na(terms$termClean) & grepl(terms$term, pattern = ":"))
	if (sum(is.factor(object)) >= 2L & length(i) > 0L) {

		# list contains three items of information:
		# each element named after the layer's name
		# each element contains factor values
		# names of values are the names as they'd appear in coefficients
		facts <- which(is.factor(object))
		possibleNames <- list()
		for (fact in facts) {
			
			levs <- levels(object)[[fact]]
			index <- length(possibleNames) + 1L
			possibleNames[[index]] <- levs[[1L]]
			names(possibleNames[[index]]) <- paste0(names(object)[fact], levs[[2L]])
			names(possibleNames)[index] <- sources(object)[fact]

		}

		# replace "nice" formula term with if() statement testing equality for each factor level combination in model
		for (ii in i) { # each row in terms table
			for (jj in 1L:(length(possibleNames) - 1L)) { # each element in factor list
				for (jCount in seq_along(possibleNames[[jj]])) { # each item in the element
					jItem <- possibleNames[[jj]][jCount]
					jName <- names(jItem)
					for (kk in (jj + 1L):length(possibleNames)) { # remaining elements in factor list
						for (kCount in seq_along(possibleNames[[kk]])) { # each item in element
							kItem <- possibleNames[[kk]][kCount]
							kName <- names(kItem)
							if (terms$term[ii] == paste0(jName, ":", kName) | terms$term[ii] == paste0(kName, ":", jName)) {
								
								jSrc <- names(possibleNames)[jj]
								kSrc <- names(possibleNames)[kk]

								# value of coefficient... at least one of these will be empty
								# both will be empty if there were no cases of crosses between the two factor levels
								thisCoeff1 <- coeffs[paste0(jName, ":", kName)]
								thisCoeff2 <- coeffs[paste0(kName, ":", jName)]
								thisCoeff <- c(thisCoeff1, thisCoeff2)
								thisCoeff <- thisCoeff[!is.na(thisCoeff)]

								if (length(thisCoeff) > 0L) {

									terms$termClean[[ii]] <- paste0("if(", jSrc, "==", jItem, " & ", kSrc, "==", kItem, ",", thisCoeff, ",0)")
								} else {
									terms$termClean[[ii]] <- NA
								}
								
							}
						}
					}
				}
			}
		}

	}

	# interactions between factors and scalars
	i <- which(is.na(terms$termClean) & grepl(terms$term, pattern = ":"))
	if (any(is.factor(object)) & any(!is.factor(object)) & length(i) > 0L) {
		
		# list contains three items of information:
		# each element named after the layer's name
		# each element contains factor values
		# names of values are the names as they'd appear in coefficients
		facts <- which(is.factor(object))
		possibleNames <- list()
		for (fact in facts) {
			
			levs <- levels(object)[[fact]]
			index <- length(possibleNames) + 1L
			possibleNames[[index]] <- levs[[1L]]
			names(possibleNames[[index]]) <- paste0(names(object)[fact], levs[[2L]])
			names(possibleNames)[index] <- sources(object)[fact]

		}

		scalarNames <- names(object)[!is.factor(object)]
		scalarNames <- scalarNames[order(nchar(scalarNames), decreasing = TRUE)]
		for (ii in i) {
		
			term <- terms$term[ii]
			matchScalar <- FALSE
			scalar <- 1L
			while (!matchScalar & scalar <= length(scalarNames)) {

				scalarName <- scalarNames[scalar]
				if (grepl(term, pattern = scalarName)) {

					matchScalar <- TRUE
					matchFact <- FALSE
					fact <- 1L
					while (!matchFact & fact <= length(possibleNames)) {

						for (j in seq_along(possibleNames[[fact]])) {
							
							factNameLevel <- names(possibleNames[[fact]][j])
							if (grepl(term, pattern = factNameLevel)) {
								
								matchFact <- TRUE

								thisCoeff1 <- coeffs[paste0(factNameLevel, ":", scalarName)]
								thisCoeff2 <- coeffs[paste0(scalarName, ":", factNameLevel)]
								thisCoeff <- c(thisCoeff1, thisCoeff2)
								thisCoeff <- thisCoeff[!is.na(thisCoeff)]

								scalarSrc <- sources(object)[[which(names(object) == scalarName)]]
								factSrc <- names(possibleNames)[fact]
								levelValue <- possibleNames[[fact]][j]

								terms$termClean[ii] <- paste0(thisCoeff, " * if(", factSrc, "==", levelValue, ",1,0) * ", scalarSrc)

							} # if factor is in term
						
						} # next level of factor

						fact <- fact + 1L

					} # next factor
				
				} # if scalar is in this term
			
				scalar <- scalar + 1L

			} # next scalar

		} # next term

	} # scalar * factor

	# scalar linear terms
	i <- which(is.na(terms$termClean) & !grepl(terms$term, pattern = ":"))
	if (length(i) > 0L) {
	
		for (j in i) {
		
			term <- .matchRasterNamesToFormula(object, terms$term[j])
			term <- paste(terms$value[j], "*", term)
			terms$termClean[j] <- term

		}
	
	}
	
	# numeric interaction terms
	i <- which(is.na(terms$termClean) & grepl(terms$term, pattern = ":"))
	if (length(i) > 0L) {
	
		for (j in i) {
		
			term <- terms$term[j]
			term <- strsplit(term, split = ":")[[1L]]
			term <- .matchRasterNamesToFormula(object, term)
			term <- paste(term, collapse = "*")
			term <- paste(terms$value[j], "*", term)
			terms$termClean[j] <- term
		
		}
	
	}

	# there is some suggestion that long "expression" terms won't work, so we do it in chunks
	nTermsAtATime <- 20L
	sets <- ceiling(nrow(terms) / nTermsAtATime)
	for (set in seq_len(sets)) {

		index <- (nTermsAtATime * (set - 1L) + 1L):(min(nTermsAtATime * set, nrow(terms)))
		# ex <- paste(terms$termClean[index], collapse = " + ")
		ex <- paste(terms$termClean, collapse = " + ")
		if (set == 1L) {
			src <- .makeSourceName(paste0("predict_r_mapcalc_set", set), "raster")
			ex <- paste0(src, " = ", ex)
		} else {
			srcIn <- src
			src <- .makeSourceName(paste0("predict_r_mapcalc_set", set), "raster")
			ex <- paste0(src, " = ", srcIn, " + ", ex)
		}

		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)

	}

	# return prediction on response scale
	fam <- stats::family(model)
	link <- fam$link

	if (type == "response" & link != "identity") {
	
		srcIn <- src
		src <- .makeSourceName("predict_r_mapcalc", "raster")

		if (link == "log") {
			ex <- paste0(src, " = exp(", srcIn, ")")
		} else if (link == "logit") {
			ex <- paste0(src, " = exp(", srcIn, ") / (1 + exp(", srcIn, "))")
		} else if (link == "cloglog") {
			ex <- paste0(src, " = 1 - exp(-1 * exp(", srcIn, "))")
		} else {
			stop("The ", link, " link function is not supported.")
		}
	
		rgrass::execGRASS(cmd = "r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

	}

	.makeGRaster(src, names = "prediction")

	} # EOF
)

#' @param object `GRaster` stack.
#' @param term Character vector to which to match names.
#'
#' @returns A character vector same length as `object` with [sources()] names.
#'
#' @noRd
.matchRasterNamesToFormula <- function(object, term) {

	# find names of rasters and replace from longest to shortest in length to help obviate duplicate names
	names <- names(object)
	srcs <- sources(object)
	nc <- nchar(names)
	ncOrder <- order(nc)
	names <- names[ncOrder]
	srcs <- srcs[ncOrder]

	for (i in seq_along(names)) {
		term <- gsub(term, pattern = names[i], replacement = srcs[i], fixed = TRUE)
	}
	term

}
