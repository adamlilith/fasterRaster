# DEFAULTS FOR ALL GLOBAL SETTINGS

# global PUBLIC options
.grassDirDefault <- function() NA_character_
.addonsDirDefault <- function() NA_character_
.workDirDefault <- function() file.path(forwardSlash(tempdir()))

.locationDefault <- function() "default"
# .mapsetDefault <- function() "PERMANENT"

.coresDefault <- function() 2
.correctDefault <- function() TRUE
.verboseDefault <- function() FALSE
.memoryDefault <- function() 1024 # in MB
.rasterPrecisionDefault <- function() "double" # "FCELL"/"float", or "DCELL"/"double"
.useDataTableDefault <- function() TRUE # logical

#' Names or table of PUBLIC options
#' @param type NULL (show all names), OR "numeric", "logical", or "character", OR "table" (show table)
#' @noRd
.namesOfOptions <- function(type = NULL) {
	
	opts <- data.frame(
		name = c(
			"grassDir",
			"addonsDir",
			"correct",
			"verbose",
			"cores",
			"memory",
			"location",
			# "mapset",
			"workDir",
			"useDataTable",
   			"rasterPrecision"
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
			"logical",
			"character"
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
