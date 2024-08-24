# DEFAULTS FOR ALL GLOBAL SETTINGS

# global PUBLIC options
.grassDirDefault <- function() NA_character_
.addonsDirDefault <- function() NA_character_
.workDirDefault <- function() file.path(omnibus::forwardSlash(tempdir()))

.locationDefault <- function() "default"
# .mapsetDefault <- function() "PERMANENT"

.coresDefault <- function() 2
.verboseDefault <- function() FALSE
.cleanDefault <- function() TRUE
.memoryDefault <- function() 2048 # in MB
# .nAtATimeDefault <- function() 1000000 # number of indices to select at a time
# .rasterPrecisionDefault <- function() "double" # "FCELL"/"float", or "DCELL"/"double"
.useDataTableDefault <- function() TRUE # logical

#' Names or table of PUBLIC options
#' @param type NULL (show all names), OR "numeric", "logical", or "character", OR "table" (show table)
#' @noRd
.namesOfOptions <- function(type = NULL) {
	
	opts <- data.frame(
		name = c(
			"grassDir",
			"addonsDir",
			"clean",
			"verbose",
			"cores",
			"memory",
			"location",
			# "mapset",
			"workDir",
			"useDataTable"
		),
		type = c(
			"character",
			"character",
			"logical",
			"logical",
			"numeric",
			"numeric",
			"character",
			# "character",
			"character",
			"logical"
		)
	)
		
	if (is.null(type)) {
		opts$name
	} else if (type == "table") {
		opts
	} else {
		opts$name[opts$type == type]
	}
	
}
