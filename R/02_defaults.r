# DEFAULTS FOR ALL GLOBAL SETTINGS

# global PUBLIC options
.coresDefault <- function() 1
.detailsDefault <- function() TRUE
.grassDirDefault <- function() NULL
.grassVerDefault <- function() 8.3
.addonDirDefault <- function() NULL
.memoryDefault <- function() 300 # in MB
.rasterPrecisionDefault <- function() "float" # "FCELL"/"float", or "DCELL"/"double"
.useDataTableDefault <- function() TRUE # logical
# .grassVerDefault <- function() "82"

.workDirDefault <- function() file.path(forwardSlash(tempdir()))
.locationDefault <- function() "default"
.mapsetDefault <- function() "PERMANENT"

# Names or table of PUBLIC options
# @param type NULL (show all names), OR "numeric", "logical", or "character", OR "table" (show table)
.namesOfOptions <- function(type= NULL) {
	
	opts <- data.frame(
		name = c(
			"grassDir",
			"addonDir",
			"grassVer",
			"details",
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
