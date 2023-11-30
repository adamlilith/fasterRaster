# DEFAULTS FOR ALL GLOBAL SETTINGS

# global PUBLIC options
.coresDefault <- function() 2
.verboseDefault <- function() FALSE
.grassDirDefault <- function() NULL
.addonDirDefault <- function() NULL
.memoryDefault <- function() 300 # in MB
.rasterPrecisionDefault <- function() "double" # "FCELL"/"float", or "DCELL"/"double"
.useDataTableDefault <- function() TRUE # logical

.workDirDefault <- function() file.path(forwardSlash(tempdir()))
.locationDefault <- function() "default"
.mapsetDefault <- function() "PERMANENT"

# Names or table of PUBLIC options
# @param type NULL (show all names), OR "numeric", "logical", or "character", OR "table" (show table)
.namesOfOptions <- function(type = NULL) {
	
	opts <- data.frame(
		name = c(
			"grassDir",
			"addonDir",
			"verbose",
			"cores",
			"memory",
			"location",
			"mapset",
			"workDir",
			"useDataTable",
   			"rasterPrecision"
		),
		type = c(
			"character",
			"character",
			"logical",
			"numeric",
			"numeric",
			"character",
			"character",
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
