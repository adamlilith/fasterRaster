\dontrun{
# NB This example is in a "dontrun{}" block because it requires users to have
# GRASS GIS Version 8+ installed on their system.

# IMPORTANT: Run the line below to store your current settings. To restore
# them, run the last line in this example.
opts. <- getFastOptions()

# see current values for options
getFastOptions('cores')
getFastOptions('memory')
getFastOptions('details')
getFastOptions('grassVer')
getFastOptions('grassDir')
getFastOptions('workDir')
getFastOptions() # all options

# see default values for options
getFastOptions('cores', default = TRUE)
getFastOptions(default = TRUE) # all options

# set options (change accordingly for your system!!!)
setFastOptions(grassDir = 'C:/Program Files/GRASS GIS 8.3')
setFastOptions(details = TRUE, memory = 600, cores = 4)

# IMPORTANT #2: Revert back to original GRASS session if needed.
fastRestore(opts.)

}
