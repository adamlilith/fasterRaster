# DEFAULTS FOR ALL GLOBAL SETTINGS

# global PUBLIC options
.grassDirDefault <- function() NA_character_
.locationDefault <- function() 'default'
.rastClassDefault <- function() 'SpatRaster'
.vectClassDefault <- function() 'SpatVector'
.coresDefault <- function() 1
.detailsDefault <- function() TRUE
.memoryDefault <- function() 300 # in MB
.grassVerDefault <- function() '82'

.workDirDefault <- function() rightSlash(tempdir())
.mapsetDefault <- function() 'PERMANENT'
.crsDefault <- function() NA_character_
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
			'grassVer',
			'location',
			'mapset',
			'workDir',
			'mapset',
			'location'
		),
		type = c(
			'character',
			'character',
			'character',
			'logical',
			'numeric',
			'numeric',
			'character',
			'character',
			'character',
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

# #' Names or table HIDDEN of options
# #' @param type NULL (show all names), OR 'numeric', 'logical', or 'character', OR 'table' (show table)
# .namesOfHiddenOptions <- function(type= NULL) {
	
	# opts <- data.frame(
		# name = c(
			# # 'workDir',
			# 'mapset',
			# 'location',
			# 'crs'
		# ),
		# type = c(
			# # 'character',
			# 'character',
			# 'character',
			# 'character'
		# )
	# )
		
	# if (is.null(type)) {
		# opts$name
	# } else if (type == 'table') {
		# opts
	# } else {
		# opts$name[opts$type == type]
	# }
	
# }
