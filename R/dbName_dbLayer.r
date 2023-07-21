#" GRASS name of data table file associated with a GVector
#"
#" @description These functions are mainly for internal use. The data frame associated with a `GVector` is stored in **GRASS** as a file. This function gets the @dbLayer slot of a `GRaster`, which contains the name of this file. Note that more than one table can be associated with a **GRASS** vector, but **fasterRaster** allows just one table per vector.
#"
#" @param x A `GVector`.
#"
#" @returns Character or `NULL`` if no data table is associated with the vector.
#"
#" @example man/examples/ex_GRaster_GVector.r
#"
#" @noRd
.dbLayer <- function(x) {
	if (inherits(x@db, "GFullMetaTable")) {
		x@db@dbLayer
	} else {
		NULL
	}
}

#" @noRd
.dbTable <- function(x) {
	if (inherits(x@db, "GFullMetaTable")) {
		x@db@dbTable
	} else {
		NULL
	}
}
