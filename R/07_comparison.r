#' Comparison operations on GRasters
#'
#' @description You can do comparative operations on `GRaster`s using normal operators in **R**: `<`, `<=`, `==`, `!=`, `>=`, and `>`.
#' 
#' @param e1,e2 `GRaster`, logical, numeric, or character. Character is useful when using a [categorical raster][tutorials_raster_data_types], in which case you can use something like `raster1 == "Wetlands"` to force all "wetland" cells to be 1 (TRUE) and all others 0 (FALSE) or `NA` (if it was originally `NA`).
#'
#' @returns A `GRaster` of [type][tutorial_raster_data_types] `CELL`.
#'
#' @example man/examples/ex_GRaster_arithmetic.r
#'
#' @rdname Comparison
#' @noRd

# raster raster
methods::setMethod("Ops", signature(e1 = "GRaster", e2 = "GRaster"),
    function(e1, e2) {
	
		compareGeom(e1, e2)
		.restore(e1)

		if (nlyr(e1) > 1L & nlyr(e2) == 1L) {
			e2 <- e2[[rep(1L, nlyr(e1))]]
		} else if (nlyr(e1) == 1L & nlyr(e2) > 1L) {
			e1 <- e1[[rep(1L, nlyr(e2))]]
		} else if (nlyr(e1) != nlyr(e2)) {
			stop("Rasters must have the same number of layers, or at least one raster must have a single layer.")
		}

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e1)) {

			name <- paste0(names(e1)[i], "_", names(e2)[i])
			src <- .makeSourceName(name, "rast")

			ex <- paste(src, "= int(if(", sources(e1)[i], " ", oper, " ", sources(e2)[i], "))")
			this <- .genericArith(name = name, src = src, ex = ex)
			
			if (i == 1L) {
				out <- this
			} else {
				out <- c(out, this)
			}
			
		}
		out
		
	} # EOF
)

# logical raster
methods::setMethod("Ops", signature(e1 = "logical", e2 = "GRaster"),
    function(e1, e2) {
	
		.restore(e2)

		e1 <- as.integer(e1)
		if (is.na(e1)) e1 <- "null()"
		
		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e2)) {
			
			name <- names(e2)[i]
			src <- .makeSourceName(name, "rast")

			ex <- paste(src, "= int(if(", e1, " ", oper, " ", sources(e2)[i], "))")
			this <- .genericArith(name = name, src = src, ex = ex)
			
			if (i == 1L) {
				out <- this
			} else {
				out <- c(out, this)
			}
			
		}
		out
			
	} # EOF
)

# raster logical
methods::setMethod("Ops", signature(e1 = "GRaster", e2 = "logical"),
    function(e1, e2) {
	
		.restore(e1)

		e2 <- as.integer(e2)
		if (is.na(e2)) e2 <- "null()"

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e1)) {

			name <- names(e2)[i]
			src <- .makeSourceName(name, "rast")
			
			ex <- paste(src, "= int(if(", sources(e1)[i], " ", oper, " ", e2, "))")
			this <- .genericArith(name = name, src = src, ex = ex)
			
			if (i == 1L) {
				out <- this
			} else {
				out <- c(out, this)
			}
			
		}
		out
			
	} # EOF
)

# numeric raster
methods::setMethod("Ops", signature(e1 = "numeric", e2 = "GRaster"),
    function(e1, e2) {
	
		.restore(e2)

		if (is.na(e1)) e1 <- "null()"

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e2)) {
		
			name <- names(e2)[i]
			src <- .makeSourceName(name, "rast")

			ex <- paste(src, "= int(if(", e1, " ", oper, " ", sources(e2)[i], "))")
			this <- .genericArith(name = name, src = src, ex = ex)
			
			if (i == 1L) {
				out <- this
			} else {
				out <- c(out, this)
			}

		}
		out
		
	} # EOF
)

# raster numeric
methods::setMethod("Ops", signature(e1 = "GRaster", e2 = "numeric"),
    function(e1, e2) {
	
		.restore(e1)

		if (is.na(e2)) e2 <- "null()"
		oper <- as.vector(.Generic)[1L]

		for (i in 1L:nlyr(e1)) {
		
			name <- names(e1)[i]
			src <- .makeSourceName(name, "rast")

			ex <- paste(src, "= int(if(", sources(e1)[i], " ", oper, " ", e2, "))")
			this <- .genericArith(name = name, src = src, ex = ex)
			
			if (i == 1L) {
				out <- this
			} else {
				out <- c(out, this)
			}
			
		}
		out
			
	} # EOF
)

# raster character
methods::setMethod("Ops", signature(e1 = "GRaster", e2 = "character"),
    function(e1, e2) {
	
		if (!all(is.factor(e1))) stop("Raster must be categorical for this type of comparison.")
		.restore(e1)

		oper <- as.vector(.Generic)[1L]
		if (!(oper %in% c("==", "!="))) stop("Can only use the ", sQuote("=="), " or ", sQuote("!="), " logical operators when comparing a raster to a character string.")

		levs <- levels(e1)

		for (i in seq_len(nlyr(e1))) {

			# get value of this category
			thisValue <- levs[[i]]$Value[levs[[i]]$Label == e2]
			thisValue <- as.numeric(thisValue)
			if (length(thisValue) == 0L) {
				this <- 0L * not.na(e1)
			} else {

				if (oper == "==") {
					this <- e1 == thisValue
				} else if (oper == "!=") {
     				this <- e1 != thisValue
				}

			}

			if (i == 1L) {
				out <- this
			} else {
				out <- c(out, this)
			}
			
		} # next layer
		out
			
	} # EOF
)

# character raster
methods::setMethod("Ops", signature(e1 = "character", e2 = "GRaster"),
    function(e1, e2) {
	
		if (!all(is.factor(e2))) stop("Raster must be categorical for this type of comparison.")
		.restore(e2)

		oper <- as.vector(.Generic)[1L]
		if (!(oper %in% c("==", "!="))) stop("Can only use the ", sQuote("=="), " or ", sQuote("!="), " logical operators when comparing a raster to a character string.")

		levs <- levels(e2)

		for (i in seq_len(nlyr(e2))) {

			# get value of this category
			thisValue <- levs[[i]]$Value[levs[[i]]$Label == e1]
			if (length(thisValue) == 0L) {
				this <- 0L * not.na(e2)
			} else {

				if (oper == "==") {
					this <- e2 == thisValue
				} else if (oper == "!=") {
     				this <- e2 != thisValue
				}

			}

			if (i == 1L) {
				out <- this
			} else {
				out <- c(out, this)
			}
			
		} # next layer
		out
			
	} # EOF
)
