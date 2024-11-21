if (grassStarted()) {

# See current values for options:
faster("grassDir")
faster("cores")
faster("memory")
faster("useDataTable")
faster() # all options

# See default values for options:
faster("cores", default = TRUE)
faster(default = TRUE) # all options

# Set options (change accordingly for your system!!!)
if (FALSE) {

   opts. <- faster() # remember starting values of options

   faster(grassDir = "C:/Program Files/GRASS GIS 8.4")
   faster(verbose = TRUE, memory = 1024, cores = 1)

   faster(c("grassDir", "verbose", "memory", "cores"))

   faster(opts.) # reset options to starting values

}

}
