\dontrun{
# NB This example is in a "dontrun{}" block because it requires users to have
# GRASS GIS Version 8+ installed on their system.

# IMPORTANT #1: If you already have a GRASS session started, you will need to
# run the line below and the last line in this example to work with it again.
# If you have not started a GRASS session, you can skip this step and go to
# step #2.
opts. <- getFastOptions()

# IMPORTANT #2: Select the appropriate line below and change as necessary to
# where GRASS is installed on your system.

grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

# setup
library(sf)
library(terra)

# example data
madElev <- fastData('madElev')
madRivers <- fastData('madRivers')

# start GRASS session for examples only
fastStart(grassDir = grassDir, crs = madElev,
workDir = tempdir(), location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster
elev <- fast(madElev)
elev

# do some math
elev + 100
elev - 100
elev * 100
elev / 100
elev ^ 2
elev %/% 100 # divide then round down
elev %% 100 # modulus

elev + TRUE
elev - TRUE
elev * TRUE
elev / TRUE
elev ^ TRUE
elev %/% TRUE # divide then round down
elev %% TRUE # modulus

elev + elev
elev - elev
elev * elev
elev / elev
elev ^ elev
elev %/% elev # divide then round down
elev %% elev # modulus

# operators
abs(-1 * elev)
sin(elev)
cos(elev)
tan(elev)

asin(elev)
acos(elev)
atan(elev)
atan2(elev, m2^1.2)

exp(elev)
log(elev)
log1p(elev)
log10(elev)
log(elev, 2)

round(elev ^ 1.2)
floor(elev ^ 1.2)
ceiling(elev ^ 1.2)
trunc(elev ^ 1.2)

# comparison
elev < 100
elev <= 100
elev == 100
elev != 100
elev > 100
elev >= 100

elev + 100 < 2 * elev
elev + 100 <= 2 * elev
elev == elev
elev != 100
elev + 100 > 2 * elev
elev + 100 >= 2 * elev

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
