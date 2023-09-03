#' Display a fasterRaster object
#'
#' @description Display a `GSession`, `GSpatial`, `GRegion`, `GRaster`, or `GVector` object.
#'
#' @param object,x An object of class `GSession`, `GSpatial`, `GRegion`, `GRaster`, or `GVector`.
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
	signature = "GSession",
	definition = function(object) {
	
	cat("class       :", paste(class(object), collapse=", "), "\n")
	cat("location    :", object@location, "\n")
	cat("mapset      :", object@mapset, "\n")
	cat("coord ref.  :", .showCRS(object), "\n")

	}
)

#' @aliases print
#' @rdname show
#' @exportMethod print
methods::setMethod(
	f = "print",
	signature = "GSession",
	definition = function(x) show(x)
)

#' @aliases show
#' @rdname show
#' @exportMethod show
methods::setMethod(
	f = "show",
	signature = "GSpatial",
	definition = function(object) {

	details <- getFastOptions("details")

	digs <- min(3, getOption("digits"))
	extent <- round(object@extent, digs)
	
	cat("class       :", paste(class(object), collapse=", "), "\n")
	if (getFastOptions("details")) {
		cat("sources(s)   :", object@sources, "\n")
		cat("location    :", object@location, "\n")
		cat("mapset      :", object@mapset, "\n")
	}
	cat("topology    :", object@topology, "\n")
	cat("extent      :", paste(extent, collapse=", "), "(xmin, xmax, ymin, ymax)\n")
	if (details & object@topology == "3D") cat("z extent    :", paste(object@zextent, collapse=", "), " (bottom, top)\n")
	cat("coord ref.  :", .showCRS(object), "\n")
	
	}
)

#' @aliases print
#' @rdname show
#' @exportMethod print
methods::setMethod(
	f = "print",
	signature = "GSpatial",
 	definition = function(x) show(x)
)

#' @aliases show
#' @rdname show
#' @exportMethod show
methods::setMethod(
	f = "show",
	signature = "GRegion",
	definition = function(object) {

	details <- getFastOptions("details")

	digs <- min(5, getOption("digits"))
	resol <- round(object@resolution, digs)

	extent <- round(object@extent, max(round(digs / 2), 2))
	zextent <- round(object@zextent, max(round(digs / 2), 2))
	
	cat("class       :", paste(class(object), collapse=", "), "\n")
	if (getFastOptions("details")) {
		# cat("sources(s)   :", object@sources, "\n")
		cat("location    :", object@location, "\n")
		cat("mapset      :", object@mapset, "\n")
	}
	cat("topology    :", object@topology, "\n")
	cat("coord ref.  :", .showCRS(object), "\n")
	cat("dimensions  :", paste(object@dimensions, collapse=", "), "(nrow, ncol, ndepth)\n")
	cat("resolution  :", paste(resol, collapse=", "), "(x, y, z)\n")
	cat("extent      :", paste(extent, collapse=", "), "(xmin, xmax, ymin, ymax)\n")
	cat("z extent    :", paste(object@zextent, collapse=", "), " (bottom, top)\n")
	
	}
)

#' @aliases print
#' @rdname show
#' @exportMethod print
methods::setMethod(
	f = "print",
	signature = "GRegion",
 	definition = function(x) show(x)
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
  		minColWidth <- 10L
	} else {
  		minColWidth <- 4L
	}
	maxColWidth <- 30L
	details <- getFastOptions("details")

	digs <- min(7, getOption("digits"))
	resol <- round(object@resolution, digs)
	if (length(resol) == 2L) resol <- c(resol, NA_real_)
	
	extent <- round(object@extent, min(digs, 3))

	### pad everything by same amount so display of data for each later appears in neat columns
	minVal <- round(object@minVal, digs)
	maxVal <- round(object@maxVal, digs)		
	
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
	if (details) activeCat <- object@activeCat - 1L

	# truncate long names
	namesNice <- object@names
	ncharNames <- nchar(namesNice)
	if (any(ncharNames > maxColWidth)) {
    	namesNice[ncharNames > maxColWidth] <-
   			paste0(substr(namesNice[ncharNames > maxColWidth], 1L, maxColWidth - 1L), "~")
	}

	# truncate long sources
	if (details) {
	
		sources <- object@sources
		ncharNames <- nchar(sources)
		if (any(ncharNames > maxColWidth)) {
			sources[ncharNames > maxColWidth] <-
				paste0(substr(sources[ncharNames > maxColWidth], 1L, maxColWidth - 1L), "~")
		}
	}

	# datatype
	datatypeFR <- datatype(object, "fasterRaster")
	if (details) datatypeGRASS <- datatype(object, "GRASS")

	# number of characters of longest display item
	nc <- pmax(
		rep(3, object@nLayers),
		nchar(namesNice),
		nchar(datatypeFR),
		nchar(nCats),
		minValLength, maxValLength
	)
 	if (details) {
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
		if (details) {
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

	if (details) {
		sources <- paste(sourcesNice, collapse=" ")
  		datatypeNiceGRASS <- paste(datatypeNiceGRASS, collapse = " ")
		activeCat <- paste(activeCat, collapse = "")
	}
	
	cat("class       : GRaster\n")
	if (details) {
		cat("location    :", object@location, "\n")
		cat("mapset      :", object@mapset, "\n")
	}
	cat("topology    :", object@topology, "\n")
	cat("dimensions  :", paste(c(object@dimensions, object@nLayers), collapse=", "), "(nrow, ncol, ndepth, nlyr)\n")
	cat("resolution  :", paste(resol, collapse=", "), "(x, y, z)\n")
	cat("extent      :", paste(extent, collapse=", "), "(xmin, xmax, ymin, ymax)\n")
	if (details | object@topology == "3D") {
		cat("z extent    :", paste(object@zextent, collapse=", "), "(bottom, top)\n")
	}
	cat("coord ref.  :", .showCRS(object), "\n")
	if (details) cat("projection  :", object@projection, "\n")
	if (details) cat("sources     :", sources, "\n")
	cat("name        :", names, "\n")
	if (!details) {
		cat("datatype    :", datatypeNiceFR, "\n")
	} else {
		cat("type (fR)   :", datatypeNiceFR, "\n")
		cat("type (GR)   :", datatypeNiceGRASS, "\n")
	}
	cat("min. value  :", minValNice, "\n")
	cat("max. value  :", maxValNice, "\n")
	if (any(numLevels > 0L)) {
  		cat("categories  :", nCatsNice, "\n")
  		if (details) cat("active cat. :", activeCat, "\n")
		cat("min. categ. :", minCatNice, "\n")
		cat("max. categ. :", maxCatNice, "\n")
	}

	} # EOF
)

#' @aliases print
#' @rdname show
#' @exportMethod print
methods::setMethod(
	f = "print",
	signature = "GRaster",
 	definition = function(x) show(x)
)

#' @aliases show
#' @rdname show
#' @exportMethod show
methods::setMethod(f="show", signature="GVector",
	definition = function(object) {

	details <- getFastOptions("details")

	digs <- min(3, getOption("digits"))
	extent <- round(object@extent, digs)
	zextent <- round(object@zextent, digs)

	# concatenate fields across vectors for display
	# making list, each element becomes a line in the display
	# each element contains field n from first vector, field n from second vector, etc.
	# will then print each element on one line
	if (object@nFields > 0L) {

		fields <- object@db@fields
		classes <- object@db@classes
		maxFieldsToShow <- min(object@nFields, 10L)
		
		if (object@nFields > 0L) {
		
			fields <- fields[1L:maxFieldsToShow]
			classes <- classes[1L:maxFieldsToShow]
			
			classes <- gsub(classes, pattern="integer", replacement="<int>")
			classes <- gsub(classes, pattern="numeric", replacement="<num>")
			classes <- gsub(classes, pattern="character", replacement="<chr>")
			
		}
		
		ncFields <- nchar(fields)
		ncFieldClasses <- nchar(classes)
		nc <- pmax(ncFields, ncFieldClasses)
		
		for (i in seq_along(fields)) {
		
			fmt	<- paste0("%", nc[i], "s")
			fields[i] <- sprintf(fmt, fields[i])
			classes[i] <- sprintf(fmt, classes[i])
		}
		
		if (object@nFields > maxFieldsToShow) {
			classes <- c(classes, paste("(and", object@nFields - maxFieldsToShow, "more)"))
		}
		
	}
	
	cat("class       : GVector\n")
	if (details) {
		cat("location    :", object@location, "\n")
		cat("mapset      :", object@mapset, "\n")
		cat("source       :", object@sources, "\n")
		if (details & object@nFields > 0) cat("db layer    :", object@db@dbLayer, "\n")
	}
	cat("geometry    :", object@geometry, "\n")
	cat("dimensions  :", paste0(object@nGeometries, ", ", object@nFields), "(geometries, columns)\n")
	cat("topology    :", object@topology, "\n")
	cat("extent      :", paste(extent, collapse=", "), "(xmin, xmax, ymin, ymax)\n")
	if (details | object@topology == "3D") cat("z extent    :", paste(zextent, collapse=", "), "(bottom, top)\n")
	cat("coord ref.  :", .showCRS(object), "\n")
	cat("projection  :", object@projection, "\n")

	if (object@nFields > 0L) {
		cat("names       :", fields, "\n")
		cat("type        :", classes, "\n")
	}
	
	} # EOF

)

#' @aliases print
#' @rdname show
#' @exportMethod print
methods::setMethod(
	f = "print",
	signature = "GVector",
 	definition = function(x) show(x)
)


#' Show the CRS for an object, and make it pretty, even if it doesn't want to
#' @noRd
.showCRS <- function(x) {

	if (inherits(x, c("SpatRaster", "SpatVector", "sf"))) {
		x <- sf::st_crs(x)
	} else if (inherits(x, "GSession")) {
		x <- st_crs(x)
	} else if (!inherits(x, "crs")) {
		x <- sf::st_crs(x)
	}

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
