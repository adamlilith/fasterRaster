#' @title Classes for 'fasterRaster' locations, rasters, and vectors
#'
#' @describeIn GSession
#'
#' @importFrom methods new
#' @importFrom methods show
#' @exportClass GVector
GVector <- setClass(
	'GVector',
	contains = 'GSpatial',
	slots = list(
		ztop = 'numeric',
		zbottom = 'numeric',
		nGeometries = 'integer',
		geometry = 'character',
		nFields = 'integer',
		fields = 'list',
		fieldClasses = 'list'
	),
	prototype = prototype(
		ztop = NA_real_,
		zbottom = NA_real_,
		geometry = NA_character_,
		nGeometries = NA_integer_,
		nFields = NA_integer_,
		fields = list(),
		fieldClasses = list()
	)
)

setValidity('GVector',
	function(object) {
		if (!(object@geometry %in% c(NA_character_, 'points', 'lines', 'polygons'))) {
			paste0('@geometry can only be NA, ', sQuote('points'), ', ', sQuote('lines'), ', or ', sQuote('polygons'), '.')
		} else if (object@nLayers != length(object@ztop)) {
			'@nLayers is different from the number of @ztop values.'
		} else if (object@nLayers != length(object@zbottom)) {
			'@nLayers is different from the number of @zbottom values.'
		} else if ((!all(is.na(object@ztop)) & !all(is.na(object@zbottom))) && any(object@ztop[!is.na(object@ztop)] < object@zbottom[!is.na(object@zbottom)])) {
			'At least one @ztop values is lower than its corresponding @zbottom value.'
		} else if (!all(sapply(object@fields, length) == sapply(object@fieldClasses, length))) {
			'@fields and @fieldClasses must have the same lengths for each vector.'
		} else {
			TRUE
		}
	} # EOF
)

#' Create a GVector
#'
#' @description Create a `GVector` from a vector existing in the current **GRASS** session.
#'
#' @param gn Character: The name of the vector in **GRASS**.
#'
#' @returns A `GVector`.
#'
#' @seealso [makeGRaster()]
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @rdname makeGVector
#' @export
makeGVector <- function(gn) {

	info <- .vectInfo(gn)
	new(
		'GVector',
		location = getFastOptions('location'),
		mapset = getFastOptions('mapset'),
		crs = crs(),
		topology = info[['topology']][1L],
		gnames = gn,
		nLayers = 1L,
		geometry = info[['geometry']][1L],
		nGeometries = info[['numGeometries']],
		extent = c(info[['west']][1L], info[['east']][1L], info[['south']][1L], info[['north']][1L]),
		ztop = info[['ztop']],
		zbottom = info[['zbottom']],
		nFields = info[['numFields']],
		fields = list(info[['fields']]),
		fieldClasses = list(info[['fieldClasses']])
	)
}

# # show
# methods::setMethod(f='show', signature='GVector',
	# definition = function(object) {

		# details <- getFastOptions('details')

		# digs <- min(3, getOption('digits'))
		# extent <- round(object@extent, digs)

		# nLayers <- nlyr(object)

		# # concatenate fields across vectors for display
		# # making list, each element becomes a line in the display
		# # each element contains field n from first vector, field n from second vector, etc.
		# # will then print each element on one line
		# fields <- object@fields
		# if (length(fields) == 0L) {
			# fieldsDisplay <- '(none)'
		# } else {
			
			# maxDisplay <- min(5L, max(object@nFields))
			# fieldsDisplay <- list()
			# for (i in 1L:maxDisplay) {
				# for (j in seq_along(fields)) {
					# if (length(fields[[j]]) >= i) {

						# fc <- if (fieldClasses[[i]][j] == 'character') {
							# '<chr>'
						# } else if (fieldClasses[[i]][j] == 'integer') {
							# '<int>'
						# } else if (fieldClasses[[i]][j] == 'numeric') {
							# '<num>'
						# }
						
						# fieldsDisplay[[i]] <- c(fieldsDisplay[[i]], paste0(fc, fields[[j]][i]))

					# }
				# }
			# }
			
			# if (any(nFields) > 5L) {
				# for (i in seq_along(fields)) {
					# if (nFields[i] > 5L) {
						# fieldsDisplay[[6L]] <- c(fieldsDisplay[[i]], paste0('(and ', nFields[i] - 5L, ' more)'))
					# } else {
						# fieldsDisplay[[6L]] <- c(fieldsDisplay[[i]], '')
					# }
				# }
			# }
			
		# }
		
		# stringLengths <- pmax(nchar(object@gnames)))
		# for (i in seq_len(nlyr(object))) {
			# stringLengths[i] <- pmax(stringLengths[i], fieldsDisplay[1L:length(fieldsDisplay)][i])
		# }
		

		# crs <- object@crs
		# crs <- sf::st_crs(crs)
		# crs <- crs$input

		# cat('class       : GVector\n')
		# if (details) {
			# cat('location    :', object@location, '\n')
			# cat('mapset      :', object@mapset, '\n')
		# }
		# cat('topology    :', object@topology, '\n')
		# cat('coord ref.  :', crs, '\n')
		# cat('dimensions  :', paste0(object@nGeometries, ', ', object@nFields), '(geometries, fields)\n')
		# cat('extent      :', paste(extent, collapse=', '), '(xmin, xmax, ymin, ymax)\n')
		# if (details) cat('gnames      :', object@gnames, '\n')
		# cat('geometry    :', object@geometry, '\n')
		# cat('vert. ext.  :', paste(object@zbottom, object@ztop, collapse=', '), '(bottom, top)\n')
		# # cat('fields      :', paste(fields, collapse=', '), '\n')
		# # cat('type        :', paste(fieldClasses, collapse=', '), '\n')

		# if (any(nFields) > 0L) {
			# for (i in seq_along(fieldsDisplay)) {
				# string <- fieldsDisplay[[i]]
				# string <- sprintf('%10s')
				# cat('fields      : ', paste())
			# }
		
		# }

		# if (is.na(fields[1L])) {
			# cat('fields      : NA\n')
		# } else {

			# cat('fields      :', fieldClasses[1L], fields[1L], '\n')
			# if (nFields > 1L){
				# for (i in 2L:length(fields)) {
					# cat('             ', fieldClasses[i], fields[i], '\n')
				# }
			# }
		# }


	# }
# )

# # print
# methods::setMethod(f='print', signature='GVector',
	# definition = function(x) {
		# show(x)
	# }
# )
