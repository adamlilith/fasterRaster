#' Retrieves lists of ellipse arguments and arguments for startFaster()
#'
#' Used internally. *MUST* change "callingFx" to the name of the function from which this function is called!!!
#'
#' @param ...		Dots from calling function (i.e., do not define anything here!).
#' @param callingFx Name of the calling function.
#'
#' @keywords internal

.getInitsDots <- function(..., callingFx) {

	# combine dots and formal arguments
	dots <- list(...)
	# args <- formals(envir = environment())
	# args <- formals(envir = parent.frame())
	args <- formals(callingFx)

	if (length(dots) > 0L) args <- c(args, dots)
	if (any(names(args) == '...')) args <- args[names(args) != '...']
	if (any(names(args) == '')) args <- args[names(args) != '']
	
	# get values of all arguments, including dots
	for (i in seq_along(args)) {
		arg <- names(args[i])
		if (exists(arg, where=parent.frame(n=1), inherits=FALSE)) {
			args[[arg]] <- get(arg, pos=parent.frame(n=1), inherits=FALSE)
		}
	}

	# make list of arguments for startFaster() called "inits"
	inits <- formals(startFaster)
	inits <- inits[names(inits) %in% names(args)]

	if (length(inits) > 0L) {

		for (init in names(inits)) {
		
			if (exists(init, where=parent.frame(n=1), inherits=FALSE)) {
				inits[[init]] <- get(init, pos=parent.frame(n=1), inherits=FALSE)
			}
			if (exists(init, where=dots, inherits=FALSE)) {
				inits[[init]] <- dots[[init]]
			}
		}
		dots <- args[!(names(args) %in% names(inits))]
		
	} else {
		inits <- list(rast = NULL, vect = NULL, inRastName = NULL, inVectName = NULL)
	}

	# args <- formals(envir = parent.frame(n=1))
	dots <- dots[!(names(dots) %in% names(args))]
	if (length(dots) == 0L) dots <- NULL

	# return
	list(
		inits = inits,
		dots = dots
	)

}

