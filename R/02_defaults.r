# DEFAULTS FOR ALL GLOBAL SETTINGS

# global PUBLIC options
.autoRegionDefault <- function() TRUE # only TRUE or FALSE, never NA
.coresDefault <- function() 1
.detailsDefault <- function() TRUE
.grassDirDefault <- function() NULL
.memoryDefault <- function() 300 # in MB
.useDataTableDefault <- function() FALSE # logical
# .grassVerDefault <- function() '82'

.workDirDefault <- function() file.path(gsub('\\\\', '/', tempdir()))
.locationDefault <- function() 'default'
.mapsetDefault <- function() 'PERMANENT'

# Names or table of PUBLIC options
# @param type NULL (show all names), OR 'numeric', 'logical', or 'character', OR 'table' (show table)
.namesOfOptions <- function(type= NULL) {
	
	opts <- data.frame(
		name = c(
			'grassDir',
			'details',
			'cores',
			'memory',
			# 'grassVer',
			'location',
			'mapset',
			'workDir',
			'autoRegion',
			'useDataTable'
		),
		type = c(
			'character',
			'logical',
			'numeric',
			'numeric',
			# 'character',
			'character',
			'character',
			'character',
			'logical',
			'logical'
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
