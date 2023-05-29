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

# start GRASS session for examples only
faster(grassDir = grassDir, crs = madElev,
workDir = tempdir(), location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster
elev <- fast(madElev)
elevs <- c(elev, elev, log10(elev) - 1, sqrt(elev))
names(elevs) <- c('elev1', 'elev2', 'log_elev', 'sqrt_elev')

elev
elevs

# do some math
elev + 100
elev - 100
elev * 100
elev / 100
elev ^ 2
elev %/% 100 # divide then round down
elev %% 100 # modulus

100 + elev
100 %/% elev
100 %% elev

elevs + 100
100 + elevs

# math with logicals
elev + TRUE
elev - TRUE
elev * TRUE
elev / TRUE
elev ^ TRUE
elev %/% TRUE # divide then round down
elev %% TRUE # modulus

elevs + TRUE
TRUE + elevs

# raster interacting with raster
elev + elev
elev - elev
elev * elev
elev / elev
elev ^ log(elev)
elev %/% sqrt(elev) # divide then round down
elev %% sqrt(elev) # modulus

elevs + elev
elev * elevs

# sign
abs(-1 * elev)
abs(elevs)
sqrt(elevs)

# trigonometry
sin(elev)
cos(elev)
tan(elev)

asin(elev)
acos(elev)
atan(elev)

atan(elevs)
atan2(elev, elev^1.2)
atan2(elevs, elev^1.2)
atan2(elev, elevs^1.2)
atan2(elevs, elevs^1.2)

# logarithms
exp(elev)
log(elev)
log2(elev)
log1p(elev)
log10(elev)
log(elev, 3)

log(elevs)

# rounding
round(elev + 0.5)
floor(elev + 0.5)
ceiling(elev + 0.5)
trunc(elev + 0.5)

# comparison
elev < 100
elev <= 100
elev == 100
elev != 100
elev > 100
elev >= 100

elev + 100 < 2 * elev

elevs > 10
10 > elevs

# mathematical functions on two or more rasters
mean(elevs)
mmode(elevs)
median(elevs)
nunique(elevs)

sum(elevs)
count(elevs)
min(elevs)
max(elevs)
range(elevs)
skewness(elevs)
kurtosis(elevs)

which.min(elevs)
which.max(elevs)

slope(elevs)
intercept(elevs)
r2(elevs)
tvalue(elevs)

sd(elevs)
sdpop(elevs)
var(elevs)
varpop(elevs)
quantile(elevs, 0.1)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
