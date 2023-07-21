#" Display a fasterRaster object
#"
#" @description Display a `GSession`, `GSpatial`, `GRegion`, `GRaster`, or `GVector` object.
#"
#" @param object An object of class `GSession`, `GSpatial`, `GRegion`, `GRaster`, or `GVector`.
#"
#" @returns Nothing (side effect is to display metadata on the given object).
#"
#" @example man/examples/ex_GRaster_GVector.r
#"
#" @aliases show
#" @rdname show
#" @exportMethod show
methods::setMethod(
	f = "show", 
	signature = "GSession",
	definition = function(object) {
	
		crs <- object@crs
		crs <- sf::st_crs(crs)
		crs <- crs$input
	
		cat("class       :", paste(class(object), collapse=", "), "\n")
		cat("location    :", object@location, "\n")
		cat("mapset      :", object@mapset, "\n")
		cat("coord ref.  :", crs, "\n")
	}
)

#" @aliases print
#" @rdname show
#" @exportMethod print
methods::setMethod(
	f = "print",
	signature = "GSession",
	definition = function(x) show(x)
)

#" @aliases show
#" @rdname show
#" @exportMethod show
methods::setMethod(
	f = "show",
	signature = "GSpatial",
	definition = function(object) {

		details <- getFastOptions("details")

		digs <- min(3, getOption("digits"))
		extent <- round(object@extent, digs)
		
		crs <- object@crs
		crs <- sf::st_crs(crs)
		crs <- crs$input

		cat("class       :", paste(class(object), collapse=", "), "\n")
		if (getFastOptions("details")) {
			cat("gnames(s)   :", object@gnames, "\n")
			cat("location    :", object@location, "\n")
			cat("mapset      :", object@mapset, "\n")
		}
		cat("topology    :", object@topology, "\n")
		cat("extent      :", paste(extent, collapse=", "), "(xmin, xmax, ymin, ymax)\n")
		if (details & object@topology == "3D") cat("z extent    :", paste(object@zextent, collapse=", "), " (bottom, top)\n")
		cat("coord ref.  :", crs, "\n")
	}
)

#" @aliases print
#" @rdname show
#" @exportMethod print
methods::setMethod(
	f = "print",
	signature = "GSpatial",
	definition = function(x) show(x)
)

#" @aliases show
#" @rdname show
#" @exportMethod show
methods::setMethod(
	f = "show",
	signature = "GRegion",
	definition = function(object) {

		details <- getFastOptions("details")

		digs <- min(5, getOption("digits"))
		resol <- round(object@resolution, digs)

		extent <- round(object@extent, max(round(digs / 2), 2))
		zextent <- round(object@zextent, max(round(digs / 2), 2))
		
		crs <- object@crs
		crs <- sf::st_crs(crs)
		crs <- crs$input

		cat("class       :", paste(class(object), collapse=", "), "\n")
		if (getFastOptions("details")) {
			# cat("gnames(s)   :", object@gnames, "\n")
			cat("location    :", object@location, "\n")
			cat("mapset      :", object@mapset, "\n")
		}
		cat("topology    :", object@topology, "\n")
		cat("coord ref.  :", crs, "\n")
		cat("dimensions  :", paste(object@dimensions, collapse=", "), "(nrow, ncol, ndepth)\n")
		cat("resolution  :", paste(resol, collapse=", "), "(x, y, z)\n")
		cat("extent      :", paste(extent, collapse=", "), "(xmin, xmax, ymin, ymax)\n")
		cat("z extent    :", paste(object@zextent, collapse=", "), " (bottom, top)\n")
	}
)

#" @aliases print
#" @rdname show
#" @exportMethod print
methods::setMethod(
	f = "print",
	signature = "GRegion",
	definition = function(x) show(x)
)

#" @aliases show
#" @rdname show
#" @exportMethod show
methods::setMethod(
	f="show",
	signature="GRaster",
	definition = function(object) {

		details <- getFastOptions("details")

		digs <- min(5, getOption("digits"))
		resol <- round(object@resolution, digs)
		if (length(resol) == 2L) resol <- c(resol, NA_real_)
		
		extent <- round(object@extent, min(digs, 2))

		crs <- object@crs
		crs <- sf::st_crs(crs)
		crs <- crs$input

		# pad everything by same amount so display of data for each later appears in neat columns
		minVal <- round(object@minVal, digs)
		maxVal <- round(object@maxVal, digs)		
		
		minValLength <- nchar(minVal)
		maxValLength <- nchar(maxVal)
		
		if (anyNA(minValLength)) minValLength[is.na(minValLength)] <- 2L
		if (anyNA(maxValLength)) maxValLength[is.na(maxValLength)] <- 2L
		
		nc <- pmax(
			rep(3, object@nLayers),
			nchar(object@names),
			nchar(object@datatypeGRASS),
			nchar(object@nCats),
			minValLength,
			maxValLength
		)
		if (details) nc <- pmax(nc, nchar(object@gnames))

		gnames <- names <- datatype <- nCats <- minValChar <- maxValChar <- rep(NA, object@nLayers)
		for (i in seq_len(object@nLayers)) {
			fmt <- paste0("%", nc[i], "s")
			gnames[i] <- sprintf(fmt, object@gnames[i])
			names[i] <- sprintf(fmt, object@names[i])
			datatype[i] <- sprintf(fmt, object@datatypeGRASS[i])
			nCats[i] <- sprintf(fmt, object@nCats[i])
			minValChar[i] <- sprintf(fmt, minVal[i])
			maxValChar[i] <- sprintf(fmt, maxVal[i])
		}
		
		gnames <- paste(gnames, collapse=" ")
		names <- paste(names, collapse=" ")
		datatype <- paste(datatype, collapse=" ")
		minValChar <- paste(minValChar, collapse=" ")
		maxValChar <- paste(maxValChar, collapse=" ")

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
		cat("coord ref.  :", crs, "\n")
		if (details) cat("gnames(s)   :", gnames, "\n")
		cat("name(s)     :", names, "\n")
		if (details) cat("datatype*   :", datatype, "\n")
		if (details | any(ncat(object) > 0L)) cat("num. categ. :", nCats, "\n")
		cat("min. value  :", minValChar, "\n")
		cat("max. value  :", maxValChar, "\n")
		cat("* GRASS datatype\n")
	}
)

#" @aliases print
#" @rdname show
#" @exportMethod print
methods::setMethod(
	f = "print",
	signature = "GRaster",
	definition = function(x) show(x)
)

#" @aliases show
#" @rdname show
#" @exportMethod show
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
	
	# CRS
	crs <- object@crs
	crs <- sf::st_crs(crs)
	crsSimple <- crs$input

	cat("class       : GVector\n")
	if (details) {
		cat("location    :", object@location, "\n")
		cat("mapset      :", object@mapset, "\n")
		cat("gname       :", object@gnames, "\n")
		if (details & object@nFields > 0) {
			cat("db layer    :", object@db@dbLayer, "\n")
			cat("db table    :", object@db@dbTable, "\n")
		}
	}
	cat("geometry    :", object@geometry, "\n")
	cat("dimensions  :", paste0(object@nGeometries, ", ", object@nFields), "(geometries, columns)\n")
	cat("topology    :", object@topology, "\n")
	cat("extent      :", paste(extent, collapse=", "), "(xmin, xmax, ymin, ymax)\n")
	if (details | object@topology == "3D") cat("z extent    :", paste(zextent, collapse=", "), "(bottom, top)\n")
	cat("coord ref.  :", crsSimple, "\n")

	if (object@nFields > 0L) {
		cat("names       :", fields, "\n")
		cat("type        :", classes, "\n")
	}
	
	} # EOF

)

#" @aliases print
#" @rdname show
#" @exportMethod print
methods::setMethod(f="print", signature="GVector",
	definition = function(x) show(x)
)
