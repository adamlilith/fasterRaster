#' Display a fasterRaster object
#'
#' @description Display a `GLocation`, `GSpatial`, `GRegion`, `GRaster`, or `GVector` object.
#'
#' @param object,x An object of class `GLocation`, `GSpatial`, `GRegion`, `GRaster`, or `GVector`.
#'
#' @returns Nothing (side effect is to display metadata on the given object).
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases show
#' @rdname show
#' @exportMethod show
methods::setMethod(
	f = "show", 
	signature = "GLocation",
	definition = function(object) {
	
	verbose <- faster("verbose")
	
	cat("class       :", paste(class(object), collapse=", "), "\n")
	cat("location    :", object@location, "\n")
	cat("mapset      :", object@mapset, "\n")
	cat("coord ref.  :", .showCRS(object), "\n")
	cat("workDir     :", object@workDir, "\n")

	}
)

#' @aliases print
#' @rdname show
#' @export
print.GLocation <- function(x, ...) show(x)

#' @aliases summary
#' @rdname show
#' @exportMethod summary
methods::setMethod(
	f = "summary",
	signature = "GLocation",
	definition = function(object) show(object)
)


#' @aliases show
#' @rdname show
#' @exportMethod show
methods::setMethod(
	f = "show",
	signature = "GSpatial",
	definition = function(object) {

	verbose <- faster("verbose")

	digs <- min(3, getOption("digits"))
	extent <- round(object@extent, digs)
	
	cat("class       :", paste(class(object), collapse=", "), "\n")
	if (verbose) {
		cat("location    :", object@location, "\n")
		cat("mapset      :", object@mapset, "\n")
		cat("workDir     :", object@workDir, "\n")
	}
	cat("topology    :", object@topology, "\n")
	cat("extent      :", paste(extent, collapse=", "), "(xmin, xmax, ymin, ymax)\n")
	if (verbose & object@topology == "3D") cat("z extent    :", paste(object@zextent, collapse=", "), " (bottom, top)\n")
	cat("coord ref.  :", format(st_crs(object)), "\n")
	
	}
)

#' @aliases print
#' @rdname show
#' @export
print.GSpatial <- function(x, ...) show(x)

#' @aliases summary
#' @rdname show
#' @exportMethod summary
methods::setMethod(
	f = "summary",
	signature = "GSpatial",
	definition = function(object) show(object)
)

#' @aliases show
#' @rdname show
#' @exportMethod show
methods::setMethod(
	f = "show",
	signature = "GRegion",
	definition = function(object) {

	verbose <- faster("verbose")

	digs <- min(5, getOption("digits"))
	resol <- round(object@resolution, digs)

	extent <- round(object@extent, max(round(digs / 2), 2))
	zextent <- round(object@zextent, max(round(digs / 2), 2))
	
	cat("class       :", paste(class(object), collapse=", "), "\n")
	if (verbose) {
		cat("location    :", object@location, "\n")
		cat("mapset      :", object@mapset, "\n")
		cat("workDir     :", object@workDir, "\n")
	}
	cat("topology    :", object@topology, "\n")
	cat("coord ref.  :", format(st_crs(object)), "\n")
	cat("dimensions  :", paste(object@dimensions, collapse=", "), "(nrow, ncol, ndepth)\n")
	cat("resolution  :", paste(resol, collapse=", "), "(x, y, z)\n")
	cat("extent      :", paste(extent, collapse=", "), "(xmin, xmax, ymin, ymax)\n")
	cat("z extent    :", paste(object@zextent, collapse=", "), " (bottom, top)\n")
	
	}
)

#' @aliases print
#' @rdname show
#' @export
print.GRegion <- function(x, ...) show(x)

#' @aliases summary
#' @rdname show
#' @exportMethod summary
methods::setMethod(
	f = "summary",
	signature = "GRegion",
	definition = function(object) show(object)
)

#' @aliases show
#' @rdname show
#' @exportMethod show
methods::setMethod(
	f = "show",
	signature = "GRaster",
	definition = function(object) {

	numLevels <- nlevels(object)
	if (any(numLevels > 0L)) {
  		minColWidth <- 14L
	} else {
  		minColWidth <- 4L
	}
	maxColWidth <- 30L
	verbose <- faster("verbose")

	digs <- min(5, getOption("digits"))
	resol <- round(object@resolution, digs)
	if (length(resol) == 2L) resol <- c(resol, NA_real_)
	
	extent <- round(object@extent, min(digs, 3))

	### pad everything by same amount so display of data for each later appears in neat columns
	minVal <- object@minVal
	maxVal <- object@maxVal
	sigs <- omnibus::roundedSigDigits(c(minVal, maxVal))
	sigs <- -1 * min(sigs) + 3
	minVal <- round(minVal, sigs)
	maxVal <- round(maxVal, sigs)
	
	minValLength <- nchar(minVal)
	maxValLength <- nchar(maxVal)
	
	if (anyNA(minValLength)) minValLength[is.na(minValLength)] <- 2L
	if (anyNA(maxValLength)) maxValLength[is.na(maxValLength)] <- 2L
	
	# minimum and maximum category values
	mmCats <- minmax(object, levels = TRUE)
	minCat <- mmCats["min", , drop = TRUE]
	maxCat <- mmCats["max", , drop = TRUE]
	minCat[is.na(minCat)] <- "NA"
	maxCat[is.na(maxCat)] <- "NA"
	nCats <- nlevels(object)
	
	if (verbose) {
		activeCat <- object@activeCat - 1L
		if (anyNA(activeCat)) activeCat[is.na(activeCat)] <- "NA"
	}

	# truncate long names
	namesNice <- object@names
	ncharNames <- nchar(namesNice)
	if (any(ncharNames > maxColWidth)) {
    	namesNice[ncharNames > maxColWidth] <-
   			paste0(substr(namesNice[ncharNames > maxColWidth], 1L, maxColWidth - 1L), "~")
	}

	# truncate long sources
	if (verbose) {
	
		sources <- object@sources
		ncharNames <- nchar(sources)
		if (any(ncharNames > maxColWidth)) {
			sources[ncharNames > maxColWidth] <-
				paste0(substr(sources[ncharNames > maxColWidth], 1L, maxColWidth - 1L), "~")
		}
		
	}

	# datatype
	datatypeFR <- datatype(object, "fasterRaster")
	if (verbose) datatypeGRASS <- datatype(object, "GRASS")

	# number of characters of longest display item
	nc <- pmax(
		rep(3, object@nLayers),
		nchar(namesNice),
		nchar(datatypeFR),
		nchar(nCats),
		minValLength, maxValLength
	)
 	if (verbose) {
		nc <- pmax(
			nc,
			nchar(activeCat),
			nchar(object@datatypeGRASS),
			nchar(sources)
		)
	}
	
	if (any(nc < minColWidth)) nc[nc < minColWidth] <- minColWidth

	# truncate category names
	if (any(nchar(minCat) > nc)) {
  		minCat[nchar(minCat) > nc] <- paste0(substr(minCat[nchar(minCat) > nc], 1L, nc - 1L), "~")
	}
	if (any(nchar(maxCat) > nc)) {
  		maxCat[nchar(maxCat) > nc] <- paste0(substr(maxCat[nchar(maxCat) > nc], 1L, nc - 1L), "~")
	}

 	# pad with extra spaces
 	sourcesNice <- names <- datatypeNiceFR <- datatypeNiceGRASS <- nCatsNice <- minValNice <- maxValNice <- minCatNice <- maxCatNice <- rep(NA, object@nLayers)

	for (i in seq_len(object@nLayers)) {

		fmt <- paste0("%", nc[i], "s")
		if (verbose) {
			sourcesNice[i] <- sprintf(fmt, sources[i])
    		activeCat[i] <- sprintf(fmt, activeCat[i])
   			datatypeNiceGRASS[i] <- sprintf(fmt, datatypeGRASS[i])
		}
		names[i] <- sprintf(fmt, object@names[i])
		datatypeNiceFR[i] <- sprintf(fmt, datatypeFR[i])
		nCatsNice[i] <- sprintf(fmt, nCats[i])
		minCatNice[i] <- sprintf(fmt, minCat[i])
		maxCatNice[i] <- sprintf(fmt, maxCat[i])
		minValNice[i] <- sprintf(fmt, minVal[i])
		maxValNice[i] <- sprintf(fmt, maxVal[i])

	}
	
	# concatenate each line into single strings
	names <- paste(names, collapse=" ")
	datatypeNiceFR <- paste(datatypeNiceFR, collapse=" ")
	minValNice <- paste(minValNice, collapse=" ")
	maxValNice <- paste(maxValNice, collapse=" ")
 	nCatsNice <- paste(nCatsNice, collapse = " ")
	minCatNice <- paste(minCatNice, collapse=" ")
	maxCatNice <- paste(maxCatNice, collapse=" ")

	if (verbose) {
		sources <- paste(sourcesNice, collapse=" ")
  		datatypeNiceGRASS <- paste(datatypeNiceGRASS, collapse = " ")
		activeCat <- paste(activeCat, collapse = "")
	}
	cat("class       : GRaster\n")
	if (verbose) {
		cat("location    :", object@location, "\n")
		cat("mapset      :", object@mapset, "\n")
		cat("workDir     :", object@workDir, "\n")
	}
	cat("topology    :", object@topology, "\n")
	cat("dimensions  :", paste(c(object@dimensions, object@nLayers), collapse=", "), "(nrow, ncol, ndepth, nlyr)\n")
	cat("resolution  :", paste(resol, collapse=", "), "(x, y, z)\n")
	cat("extent      :", paste(extent, collapse=", "), "(xmin, xmax, ymin, ymax)\n")
	if (verbose | object@topology == "3D") {
		cat("z extent    :", paste(object@zextent, collapse=", "), "(bottom, top)\n")
	}
	cat("coord ref.  :", format(st_crs(object)), "\n")
	if (verbose) cat("projection  :", object@projection, "\n")
	if (verbose) cat("sources     :", sources, "\n")
	cat("name(s)     :", names, "\n")
	if (!verbose) {
		cat("datatype    :", datatypeNiceFR, "\n")
	} else {
		cat("type (fR)   :", datatypeNiceFR, "\n")
		cat("type (GR)   :", datatypeNiceGRASS, "\n")
	}
	cat("min. value  :", minValNice, "\n")
	cat("max. value  :", maxValNice, "\n")
	if (any(numLevels > 0L)) {
  		cat("categories  :", nCatsNice, "\n")
  		if (verbose) cat("active cat. :", activeCat, "\n")
		cat("min. categ. :", minCatNice, "\n")
		cat("max. categ. :", maxCatNice, "\n")
	}

	} # EOF
)

#' @aliases print
#' @rdname show
#' @export
print.GRaster <- function(x, ...) show(x)

#' @aliases summary
#' @rdname show
#' @exportMethod summary
methods::setMethod(
	f = "summary",
	signature = "GRaster",
	definition = function(object) show(object)
)

#' @aliases show
#' @rdname show
#' @exportMethod show
methods::setMethod(
	f = "show",
	signature = "GVector",
	definition = function(object) {

	# maximum number of fields to show
	maxFieldsToShow <- 8L
  	maxColWidth <- 15L
	maxDigits <- 5L

	verbose <- faster("verbose")

	digs <- min(maxDigits, getOption("digits"))
	extent <- round(object@extent, digs)
	zextent <- round(object@zextent, digs)

	# concatenate fields across vectors for display
	# making list, each element becomes a line in the display
	# each element contains field n from first vector, field n from second vector, etc.
	# will then print each element on one line
	nFields <- ncol(object@table)
	nRows <- nrow(object@table)
	table <- head(object@table, 3)
	if (nRows > 0L) {

		fields <- colnames(table)
		classes <- sapply(table, class)
		maxFieldsToShow <- min(nFields, maxFieldsToShow)
		
  		row1 <- table[1L, 1L:maxFieldsToShow]
		row1 <- as.data.frame(row1)
		row1 <- unlist(row1)
		row1 <- as.character(row1)
		if (anyNA(row1)) row1[is.na(row1)] <- "NA"
		
		if (nRows >= 2L) {
			row2 <- table[2L, 1L:maxFieldsToShow]
			row2 <- as.data.frame(row2)
			row2 <- unlist(row2)
			row2 <- as.character(row2)
			if (anyNA(row2)) row2[is.na(row2)] <- "NA"
		} else {
			row2 <- NULL
		}
		
		if (nRows >= 3L) {
			row3 <- table[3L, 1L:maxFieldsToShow]
   			row3 <- as.data.frame(row3)
			row3 <- unlist(row3)
			row3 <- as.character(row3)
			if (anyNA(row3)) row3[is.na(row3)] <- "NA"
		} else {
			row3 <- NULL
		}

		fields <- fields[1L:maxFieldsToShow]
		classes <- classes[1L:maxFieldsToShow]
		
		classes <- gsub(classes, pattern = "integer", replacement = "<int>")
		classes <- gsub(classes, pattern = "numeric", replacement = "<num>")
		classes <- gsub(classes, pattern = "complex", replacement = "<cpx>")
		classes <- gsub(classes, pattern = "character", replacement = "<chr>")
		classes <- gsub(classes, pattern = "logical", replacement = "<log>")
		classes <- gsub(classes, pattern = "Date", replacement = "<Dte>")

		# column widths
		ncFields <- nchar(fields)
		ncFieldClasses <- nchar(classes)
		ncRow1 <- nchar(row1)
		nc <- pmax(ncFields, ncFieldClasses, ncRow1)
		
		if (!is.null(row2)) ncTableCells <- nchar(row2)
		if (!is.null(row3)) ncTableCells <- pmax(ncTableCells, nchar(row3))
		if (!is.null(row2))	nc <- pmax(nc, ncTableCells)
		if (any(nc > maxColWidth)) nc[nc > maxColWidth] <- maxColWidth

		# format table values
		tooLongIndex <- which(nchar(row1) > maxColWidth)
		if (length(tooLongIndex) > 0L) {
			row1[tooLongIndex] <- paste0(substr(row1[tooLongIndex], 1L, nc[tooLongIndex] - 1L), "~")
		}

		if (!is.null(row2)) {		

			tooLongIndex <- which(nchar(row2) > nc)
			if (length(tooLongIndex) > 0L) {
				row2[tooLongIndex] <- paste0(substr(row2[tooLongIndex], 1L, nc[tooLongIndex] - 1L), "~")
			}
		
		}
		
		if (!is.null(row3)) {		

   			tooLongIndex <- which(nchar(row3) > nc)
			if (length(tooLongIndex) > 0L) {
				row3[tooLongIndex] <- paste0(substr(row3[tooLongIndex], 1L, nc[tooLongIndex] - 1L), "~")
			}
		
		}
		
		for (i in seq_along(fields)) {
		
			fmt	<- paste0("%", nc[i], "s")
			fields[i] <- sprintf(fmt, fields[i])
			classes[i] <- sprintf(fmt, classes[i])
   			row1[i] <- sprintf(fmt, row1[i])
   			if (!is.null(row2)) row2[i] <- sprintf(fmt, row2[i])
   			if (!is.null(row3)) row3[i] <- sprintf(fmt, row3[i])
		
		}
		
		if (nFields > maxFieldsToShow) {
			classes <- c(classes, paste("(and", nFields - maxFieldsToShow, "more column(s))"))
		}
		
	}
	
	cat("class       : GVector\n")
	if (verbose) {
		cat("location    :", object@location, "\n")
		cat("mapset      :", object@mapset, "\n")
		cat("source      :", object@sources, "\n")
		cat("workDir     :", object@workDir, "\n")
	}
 	cat("geometry    :", object@topology, object@geometry, "\n")
	cat("dimensions  :", paste0(object@nGeometries, ", ", object@nSubgeometries, ", ", nFields), "(geometries, sub-geometries, columns)\n")
	# cat("dimensions  :", paste0(object@nGeometries, ", ", nFields), "(geometries, columns)\n")
	cat("extent      :", paste(extent, collapse=", "), "(xmin, xmax, ymin, ymax)\n")
	if (verbose | object@topology == "3D") cat("z extent    :", paste(zextent, collapse=", "), "(bottom, top)\n")
	cat("coord ref.  :", format(st_crs(object)), "\n")
	if (verbose) cat("projection  :", object@projection, "\n")

	if (nFields > 0L) {
		cat("names       :", fields, "\n")
		cat("type        :", classes, "\n")
		cat("values      :", row1, "\n")
		if (!is.null(row2)) cat("             ", row2, "\n")
		if (!is.null(row3)) cat("             ", row3, "\n")
		if (nRows > 3L) cat(paste0("              (and ", nRows - 3L, " more rows)\n"))
	}
	
	} # EOF

)

#' @aliases print
#' @rdname show
#' @export
print.GVector <- function(x, ...) show(x)

#' @aliases summary
#' @rdname show
#' @exportMethod summary
methods::setMethod(
    f = "summary",
    signature = "GVector",
    definition = function(object) show(object)
)

#' Show the CRS for an object, and make it pretty, even if it doesn't want to
#' @noRd
.showCRS <- function(x) {

	# format(st_crs(x))

	x <- crs(x)
	x <- sf::st_crs(x)

	if (!is.na(x$input) && x$input != x$wkt) {
		out <- x$input # pre-made
	} else {
  		start <- regexec(x$wkt, pattern = "CONVERSION")[[1L]]
		if (start == -1L) {
			out <- NA_character_
		} else {
   			start <- start + nchar("CONVERSION") + 1L
			x <- substr(x$wkt, start = start, stop = nchar(x$wkt))

   			stop <- regexec(x, pattern = ",")[[1L]] - 2L
			x <- substr(x, start = 2L, stop)
			if (grepl(x, pattern = "METHOD")) {
				out <- NA_character_
			} else {
			   out <- x
			}
		}
	}
	out

}