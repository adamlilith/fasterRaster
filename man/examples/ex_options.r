\dontrun{
# NB This example is in a "dontrun{}" block because it requires users to have
# GRASS GIS Version 8+ installed on their system.

# IMPORTANT: Run the line below to store your current settings. To restore
# them, run the last line in this example.
opts. <- getFastOptions()

# See current values for options:
getFastOptions("cores")
getFastOptions("memory")
getFastOptions("useDataTable")
getFastOptions("rasterPrecision")
getFastOptions("verbose")
getFastOptions("grassDir")
getFastOptions("addonDir")
getFastOptions("workDir")
getFastOptions() # all options

# See default values for options:
getFastOptions("cores", default = TRUE)
getFastOptions(default = TRUE) # all options

# Set options (change accordingly for your system!!!)
if (FALSE) {

setFastOptions(grassDir = "C:/Program Files/GRASS GIS 8.4")
setFastOptions(verbose = TRUE, memory = 600, cores = 4)

getFastOptions(c("grassDir", "verbose", "memory", "cores"))

}

# IMPORTANT #2: Revert back to original GRASS session if needed.
restoreSession(opts.)

}
