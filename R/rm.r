#' Remove rasters/vectors from a GRASS session
#'
#' Remove rasters or vectors from a **GRASS** session. This will remove them from **GRASS**, but not from R if they exists in R.
#'
#' @param ... Any of:
#' * One or more `GRaster`s or `GVector`s
#' * Character: `gnames` of of `GRaster`s or `GVector`s to remove.
#'* `*`: *Everything* will be removed.
#' `*rasters*`: *All* rasters.
#' `*vectors*`: *All* vectors.
#'
#' @return Invisibly returns `NULL`.
#'
#' @seealso [rm()]; **GRASS** module [https://grass.osgeo.org/grass82/manuals/g.remove.html](g.remove)
#'
#' @example man/examples/ex_fasterOperations.r
#'
#' @keywords internal

if (!isGeneric('.rm')) { rm.GRaster <- setGeneric('.rm', function(...) standardGeneric('.rm')) }
if (!isGeneric('.rm')) { rm.GVector <- setGeneric('.rm', function(...) standardGeneric('.rm')) }
if (!isGeneric('.rm')) { rm.character <- setGeneric('.rm', function(...) standardGeneric('.rm')) }

# generic rm() function
..rm <- function(...) {

	x <- list(...)
	if (length(x) > 0L) {
	
		# rasters or vectors?
		rasts <- sapply(x, inherits, 'GRaster')
		vects <- sapply(x, inherits, 'GVector')
		
		types <- rep(NA, length(x))
		types[rasts] <- 'raster'
		types[vects] <- 'vector'
		
		gnames <- sapply(x, .gname)

		# names_from_dots <- function(...) sapply(substitute(list(...))[-3L], deparse)
		# rname <- names_from_dots(...)
		# rname <- rname[2L:length(rname)]

		# remove from GRASS and R
		for (i in seq_along(x)) {
			
			# # nullify object in R
			# if (type == 'raster') {

				# toNullify <- GRaster(
					# location = NA_character_,
					# mapset = NA_character_,
					# crs = NA_character_,
					# gname = NA_character_,
					# rname = NA_character_,
					# extent = c(NA_real_, NA_real_, NA_real_, NA_real_),
					# topology = NA_character_,
					# datatypeGRASS = NA_character_,
					# dimensions = c(NA_integer_, NA_integer_, NA_integer_),
					# resolution = c(NA_real_, NA_real_),
					# numCategories = NA_integer_,
					# minVal = NA_real_,
					# maxVal = NA_real_
				# )
			
			# } else {
			
				# toNullify <- GVector(
					# location = NA_character_,
					# mapset = NA_character_,
					# crs = NA_character_,
					# gname = NA_character_,
					# extent = c(NA_real_, NA_real_, NA_real_, NA_real_),
					# topology = NA_character_,
					# geometry = NA_character_,
					# bottom = NA_real_,
					# top = NA_real_,
					# fields = NA_real_,
					# fieldClasses = NA_real_
				# )
			
			# }
			
			# assign(rname[i], toNullify, pos=sys.frame())
# warning('ABS: Removing from sys.frame()... is this correct for all cases?')
			rgrass::execGRASS('g.remove', flags=c('quiet', 'f'), type=types[i], name=gnames[i])
			
		}
	
	}

	invisible(NULL)

} # EOF

setMethod(
	'.rm',
	signature('GRaster'),
	function(...) {
	..rm(...)
	invisible(NULL)
	}
)

setMethod(
	'.rm',
	signature('GVector'),
	function(...) {
	..rm(...)
	invisible(NULL)
	}
)

setMethod(
	'.rm',
	signature('character'),
	function(...) {
	
	x <- c(...)
	if (length(x) > 0L) {
	
		# generics
		if (length(x) == 1L) {
			x <- if (x[1L] == '*') {
				.ls()
			} else if (x[1L] == '*rasters*') {
				.ls('rasters')
			} else if (x[1L] == '*vectors*') {
				.ls('vectors')
			}
		}

		# types
		spatials <- .ls()
		spatialTypes <- names(spatials)
		types <- spatialTypes[match(x, spatials)]
		
		# remove from GRASS
		for (i in seq_along(x)) {
			rgrass::execGRASS('g.remove', flags=c('quiet', 'f'), type=types[i], name=x[i])
		}
	
	}
	
	invisible(NULL)
	
	} # EOF
)
