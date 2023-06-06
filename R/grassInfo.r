#' GRASS citation, version, and copyright information
#'
#' Report the **GRASS** citation, version, or copyright information.
#'
#' @param x Character: What to return. Any of: `'citation'` (default), `'version'`, '`copyright`' Partial matching is used.
#' 
#' @return Character.
#'
#' @example man/examples/ex_grassInfo.r
#' 
#' @aliases grassInfo
#' @rdname grassInfo
#' @export grassInfo
grassInfo <- function(x = 'citation') {
	
	match <- pmatchSafe(x, table = c('citation', 'version', 'copyright'))

	if (match == 'citation') {
		rgrass::execGRASS('g.version', flags='x')
	} else if (match == 'copyright') {
		rgrass::execGRASS('g.version', flags='c')
	} else if (match == 'version') {
		rgrass::execGRASS('g.version')
	}

}
