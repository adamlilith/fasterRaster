#' Get topological type (point, line, polygon) for one or more spatial vectors
#'
#' This function attempts to ascertain the type of topology of a spatial vector. In \code{GRASS}, vectors can have a topology type of \code{point}, \code{line}, \code{area} (polygon), \code{centroid}, or \code{boundary}. This function is an imperfect way to try to ascertain whether the vector is a \code{point}, \code{line}, or \code{polygon} (the others are unsupported).
#'
#' @param vect The name of one or more spatial vectors in the active \code{GRASS} session.
#' @param as The format in which to return the results:
#' \itemize{
#'	\item \code{'GRASS'} (default): Returns \code{point}, \code{line}, \code{polygon}, or \code{NA} (indeterminate).
#'	\item \code{'terra'}: Returns \code{points}, \code{lines}, \code{polygons}, or \code{NA} (indeterminate).
#'	\item \code{'sf'}: Returns \code{POINTS}, \code{LINESTRING}, \code{POLYGONS}, or \code{NA} (indeterminate). Note that technically any of these could be the "\code{MULTI}" version, but this is left implied.
#' }
#' @param names If \code{TRUE} (default), the returned vector will have the vectors' names.
#'
#' @return A named character vector. If a type cannot be determined, it will be \code{NA}.
#'
#' @examples man/examples/ex_fasterInfo.r
#'
#' @export

vectTopo <- function(vect, as = 'GRASS', names = TRUE) {

	# type <- n <- rep(NA, length(vect))
	type <- rep(NA, length(vect))
	if (names) names(type) <- vect
	
	for (i in seq_along(vect)) {
	
		info <- execGRASS('v.info', map=vect[i], flags='t', intern=TRUE)
		
		points <- info[grepl(info, pattern='points=')]
		lines <- info[grepl(info, pattern='lines=')]
		boundaries <- info[grepl(info, pattern='boundaries=')]
		centroids <- info[grepl(info, pattern='centroids=')]
		areas <- info[grepl(info, pattern='areas=')]
	
		points <- sub(points, pattern='points=', replacement='')
		lines <- sub(lines, pattern='lines=', replacement='')
		boundaries <- sub(boundaries, pattern='boundaries=', replacement='')
		centroids <- sub(centroids, pattern='centroids=', replacement='')
		areas <- sub(areas, pattern='areas=', replacement='')
	
		points <- as.numeric(points)
		lines <- as.numeric(lines)
		boundaries <- as.numeric(boundaries)
		centroids <- as.numeric(centroids)
		areas <- as.numeric(areas)
	
		type[i] <- if (points > 0 & lines == 0 & boundaries == 0 & centroids == 0 & areas == 0) {
			'point'
		} else if (points == 0 & lines > 0 & boundaries == 0 & centroids == 0 & areas == 0) {
			'line'
		} else if (points == 0 & lines == 0 & boundaries > 0 & centroids > 0 & areas > 0) {
			'polygon'
		} else {
			NA
		}
		
		# if (!is.na(type)) n[i] <- get(type, inherits=FALSE)
		
	} # next vector
	
	if (as == 'terra') {
		if (any(type == 'point')) type[type == 'point'] <- 'points'
		if (any(type == 'point')) type[type == 'point'] <- 'lines'
		if (any(type == 'point')) type[type == 'point'] <- 'pologons'
	} else if (as == 'sf') {
		if (any(type == 'point')) type[type == 'point'] <- 'POINT'
		if (any(type == 'point')) type[type == 'point'] <- 'LINESTRING'
		if (any(type == 'point')) type[type == 'point'] <- 'POLYGON'
	}
	
	type

}
