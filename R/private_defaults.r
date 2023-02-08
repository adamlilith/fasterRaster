# default values for all global arguments and variables

# session settings
.dirDefault <- function() tempdir()
.mapSetDefault <- function() NA
.locationDefault <- function() 'default'

# global arguments
.grassDirDefault <- function() NULL
.coresDefault <- function() 1
.memoryDefault <- function() 300 # in MB
.replaceDefault <- function() FALSE
.grassToRDefault <- function() TRUE
.trimRastDefault <- function() TRUE
.outVectClassDefault <- function() 'SpatVector'
.autoRegionDefault <- function() TRUE
.grassVerDefault <- function() '82'
