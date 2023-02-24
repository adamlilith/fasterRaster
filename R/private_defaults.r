# DEFAULTS FOR ALL GLOBAL SETTINGS

# global PUBLIC options
.grassDirDefault <- function() NULL
.locationDefault <- function() 'default'
.rastClassDefault <- function() 'SpatRaster'
.vectClassDefault <- function() 'SpatVector'
.coresDefault <- function() 1
.detailsDefault <- function() TRUE
.memoryDefault <- function() 300 # in MB
.grassVerDefault <- function() '82'

# global HIDDEN options
.workDirDefault <- function() rightSlash(tempdir())
.mapsetDefault <- function() 'PERMANENT'
.locationDefault <- function() 'default'

# Names or table of PUBLIC options
# @param type NULL (show all names), OR 'numeric', 'logical', or 'character', OR 'table' (show table)
.namesOfOptions <- function(type= NULL) {
	
	opts <- data.frame(
		name = c(
			'grassDir',
			'rastClass',
			'vectClass',
			'details',
			'cores',
			'memory',
			'grassVer'
		),
		type = c(
			'character',
			'character',
			'character',
			'logical',
			'numeric',
			'numeric',
			'character'
		)
	)
		
	if (is.null(type)) {
		opts$name
	} else if (type == 'table') {
		opts
	} else {
		opts$name[opts$type == type]
	}
	
}

#' Names or table HIDDEN of options
#' @param type NULL (show all names), OR 'numeric', 'logical', or 'character', OR 'table' (show table)
.namesOfHiddenOptions <- function(type= NULL) {
	
	opts <- data.frame(
		name = c(
			'workDir',
			'mapset',
			'location'
		),
		type = c(
			'character',
			'character',
			'character'
		)
	)
		
	if (is.null(type)) {
		opts$name
	} else if (type == 'table') {
		opts
	} else {
		opts$name[opts$type == type]
	}
	
}
