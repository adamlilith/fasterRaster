#' Replace a specific value(s) in a GRaster
#'
#' @description This function replaces one or more user-specified values in a raster with other values. See [classify()] for replacing ranges of values.
#'
#' @param x A `GRaster`.
#'
#' @param from,to Vectors of numeric or character values. The value(s) in `from` will be replaced with the value(s) in `to`. They must be the same length, or, if you supply a single value for `to`, then all values in `from` will be converted to the same value of `to`. Numeric/integer or character vectors can be used:
#' * `from` and `to` are numeric or integer vectors: Values in `from` will be replaced by their corresponding value in `to`.
#' * `from` and `to` are character vectors: You can use a character vector for `from` and `to`. In this case, the input raster must be a factor (categorical) raster or an integer raster. If `from` is a character vector, these levels(categories) will be replaced by the levels in `to.` This can add levels to a `GRaster` `to` has labels that do not match any existing labels in `from`.
#' * `from` is a character vector and `to` is an integer vector: Cells in `x` that correspond to the given label will have their values replaced by the corresponding value in `to`, and be matched the the corresponding label. If no label corresponds to the new value, a new level will be created. The input must be a categorical raster.
#' * `from` is an integer vector and `to` is a character vector: Cells in `x` that have a value in `from` will be replaced by values that match the labels in `to`. If the input raster does not have a value that corresponds to the given label in `y`, then a new value will be created.
# * You can include `NA` in either `from` or `to` to replace `NA` values or to assign `NA` values.
#' 
#' @param others `NULL` (default), `NA`, numeric, or a character:
#' * `NULL` (default): Values that do not appear in `from` will be unchanged.
#' * `NA`: Values that do not appear in `from` will be set to `NA`.
#' * Character: Cells in `x` that do not appear in `to` will be assigned to this level. In this case, `x` must be a categorical (factor) raster.
#'
#' @param warn Logical: If `TRUE` (default), display a warning when new levels are created.
#'
#' @returns A `GRaster`.
#'
#' @seealso [terra::subst()], [classify()]
#'
#' @example man/examples/ex_subst.r
#'
#' @aliases subst
#' @rdname subst
#' @exportMethod subst
methods::setMethod(
	f = "subst",
	signature = c(x = "GRaster"),
	function(x, from, to, others = NULL, warn = TRUE) {
	
	if (length(from) != length(to) & length(to) != 1L) stop("Arguments `from` and `to` must have the same length.")
	if (is.character(from) & !all(is.factor(x))) stop("You can only use a character vector for `from` if all\n  layers in `x` are categorical rasters.")
	if (is.character(to) & any(datatype(x, "GRASS") != "CELL")) stop("You can only use a character vector for `to` if all\n  layers in `x` are categorical or integer.")
	if (is.character(others) & !all(is.factor(x))) stop("You can only use a character for `others` if all\n  layers in `x` are categorical.")

	if (length(from) > 1L & length(to) == 1L) to <- rep(to, length(from))

	.locationRestore(x)
	.region(x)

	levs <- cats(x)
	nLayers <- nlyr(x)
	srcs <- .makeSourceName("subst_r_mapcalc", "raster", n = nLayers)
	for (i in seq_len(nLayers)) {
	
		if (is.factor(x)[i]) {
			
			thisLev <- levs[[i]]
			ac <- activeCat(x, layer = i)

			# `from` is a character vector
			if (is.character(from)) {

				if (faster("useDataTable")) {
					thisFrom <- thisLev[[1L]][match(from, thisLev[[ac + 1L]])]
				} else {
					thisFrom <- thisLev[ , 1L][match(from, thisLev[  , ac + 1L, drop = TRUE])]
				}
				if (any(is.na(thisFrom) & !is.na(from))) {
					stop("Argument `from` contains one or more values that do not appear in the GRaster's levels table.\n  These will be added ")
				}

			# `from` is numeric/logical
			} else {
				thisFrom <- from
				thisFrom <- as.integer(thisFrom)
			}
		
			if (any(is.character(to))) {

				if (faster("useDataTable")) {
					thisTo <- thisLev[[1L]][match(to, thisLev[[ac + 1L]])]
				} else {
					thisTo <- thisLev[ , 1L][match(to, thisLev[  , ac + 1L, drop = TRUE])]
				}

				# create new levels
				if (any(is.na(thisTo) & !is.na(to))) {
				
					if (warn) warning("Not all labels in `to` are found in `x`. New levels will be created.")
					
					topVal <- max(.maxVal(x)[i], thisLev[[ac]])
					nNew <- sum(is.na(unique(thisTo)) & !is.na(unique(to)))
					newVals <- 1L:nNew + topVal
					thisTo[is.na(thisTo) & !is.na(to)] <- newVals
					thisTo <- as.integer(thisTo)
					
					# create table to `rbind` to levels table
					newLev <- .addLevels(ac = ac, thisLev = thisLev, thisTo = thisTo, labels = to)

					# add new levels to table
					thisLev <- rbind(thisLev, newLev)
			
				}

			# `to` is integer/numeric
			} else if (!is.character(to)) {

				thisTo <- to
				if (any(omnibus::countDecDigits(thisTo) > 0L)) stop("When using `subst()` on a categorical rasters, the\n  values in `to` must be integers or character values.")
				thisTo <- as.integer(thisTo)

				# create table to `rbind` to levels table
				newLev <- .addLevels(ac = ac, thisLev = thisLev, thisTo = thisTo, labels = NULL)

				# add new levels to table
				thisLev <- rbind(thisLev, newLev)
			
			}
	
			# # remove levels that were re-assigned
			# removes <- thisFrom[!(thisFrom %in% thisTo)]
			# removes <- removes[!is.na(removes)]
			# if (length(removes) > 0L) levs[[i]] <- levs[[i]][!(levs[[i]][[1L]] %in% removes)]

			# remove duplicates
			if (faster("useDataTable")) {
				levs[[i]] <- thisLev[!duplicated(thisLev[ , c(1L, ac + 1L), with = FALSE])]
			} else {
				levs[[i]] <- thisLev[!duplicated(thisLev[ , c(1L, ac + 1L), drop = FALSE]), , drop = FALSE]
			}

		} else {

			# raster is not a factor raster
			thisFrom <- from
			thisTo <- to

		}

		ex <- paste0(srcs[i], " = ")
		for (j in seq_along(thisFrom)) {
			if (is.na(thisFrom[j]) & is.na(thisTo[j])) {
				ex <- paste0(ex, "if(isnull(", sources(x)[i], "),null(),")
			} else if (is.na(thisFrom[j]) & !is.na(thisTo[j])) {
				ex <- paste0(ex, "if(isnull(", sources(x)[i], "),", thisTo[j], ",")
			} else if (!is.na(thisFrom[j]) & is.na(thisTo[j])) {
				ex <- paste0(ex, "if(", sources(x)[i], "==", thisFrom[j], ",null(),")
			} else {
				ex <- paste0(ex, "if(", sources(x)[i], "==", thisFrom[j], ",", thisTo[j], ",")
			}
		}

		if (is.null(others)) {
			ex <- paste0(ex, sources(x)[i])
		} else if (is.na(others)) {
			ex <- "null()"
		} else if (is.character(others) & is.factor(x)[i]) {

			# get new value for `others`
			thisOthers <- levs[[i]][levs[[i]][[ac + 1]] == others, 1L]
			thisOthers <- thisOthers[[1L]]
	
			if (length(thisOthers) == 0L) {
				
				# add new level
				if (warn) warning("the value in `others` is not found among existing level labels. A new level will be created.")
				newVal <- max(0L, .maxVal(x)[i], max(thisTo, na.rm = TRUE)) + 1L
				newVal <- as.integer(newVal)
				newLabel <- others
				newLev <- .addLevels(ac = ac, thisLev = thisLev, thisTo = newVal, labels = others)
				levs[[i]] <- rbind(levs[[i]], newLev)
				ex <- paste0(ex, newVal)

			} else {
				ex <- paste0(ex, thisOthers)
			}

		
		} else if (is.character(others) & !is.factor(x)[i]) {
			stop("Cannot add a level to a non-factor GRaster. To make a GRaster categorical, use `levels<-` or `categories`.")
		} else {
			ex <- paste0(ex, others)
		}

		ex <- paste0(ex, paste(rep(")", length(thisFrom)), collapse = ""))
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
	
	}
	.makeGRaster(srcs, names = names(x), levels = levs)
	
	}
)


#' Creates a new data.table with same structure as the existing levels table
#'
#' @param ac Integer: Active category index (increment by 1 to get the actual column).
#' @param thisLev `data.table`: The existing levels table, to be used as a template.
#' @param thisTo Integer(s): The new values to add (integers).
#' @param `NULL` or character: The new label(s) to add (`NULL` if they already exist in `thisLev`, or character if not).
#'
#' @returns A `data.table`.
#'
#' @noRd
.addLevels <- function(ac, thisLev, thisTo, labels = NULL) {

	# create table to `rbind` to levels table
	for (j in 1L:ncol(thisLev)) {
		if (j == 1L) {
			newLev <- data.table::data.table(DUMMY__ = thisTo)
		} else {
			if (j == ac + 1L) {

				if (!is.null(labels)) {
					thisLabels <- labels
				} else {
					thisLabels <- thisLev[[j]][match(thisTo, thisLev[[1L]])]
				}
				add <- data.table::data.table(DUMMY__ = thisLabels)
			
			} else {
				add <- data.table::data.table(DUMMY__ = NA)
			}
			newLev <- cbind(newLev, add)
		}
		names(newLev)[j] <- names(thisLev)[j]
	}
	newLev <- newLev[!duplicated(newLev)]
	newLev <- newLev[!is.na(newLev[[1L]])]

	newLev

}




