\dontrun{

library(terra)

# change this to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

madElev <- fasterData('madElev')
ex <- 'out = madElev^2'
madElev2 <- fasterMapcalc(madElev, expression=ex, grassDir=grassDir)
plot(c(madElev, madElev2))

ex <- 'madElev_17 = 17'
m_17 <- fasterMapcalc(madElev, expression=ex, grassDir=grassDir)
plot(c(madElev, m_17))

# low-pass filter
ex <- 'lowPass = (madElev[-1, -1] + madElev[-1, 0] + madElev[-1, 1] + madElev[0, -1] + madElev[0, 0] + madElev[0, 1] + madElev[1, -1] + madElev[1, 0] + madElev[1, 1]) / 9'
lp <- fasterMapcalc(madElev, expression=ex, grassDir=grassDir)
plot(c(madElev, lp))

# high-pass filter
ex <- 'highPass = -0.7 * madElev[-1, -1] -1 * madElev[-1, 0] -0.7 * madElev[-1, 1] -1 * madElev[0, -1] + 6.8 * madElev[0, 0] -1 * madElev[0, 1] -0.7 * madElev[1, -1] -1 * madElev[1, 0] -0.7 * madElev[1, 1]'
hp <- fasterMapcalc(madElev, expression=ex, grassDir=grassDir)
plot(c(madElev, hp))

# Using some rasters already in a GRASS session:
# Note we are passing raster "hp" as a raster, not as something already in
# GRASS (even though it is). Also, we need to refer to the "hp" raster by
# its raster name in the formula (ie, see "names(hp)") since it is passed in
# as a raster.

input <- initGrass(rast=madElev, rastName='madElev', grassDir=grassDir)
exportRastToGrass(lp, grassName='lp')

ex <- 'out = (lp * highPass) / madElev'
mapcalc <- fasterMapcalc('madElev', 'lp', hp, expression=ex,
grassDir=grassDir)
mapcalc

# note differences between r.mapcalc and terra package math:
mapcalc
rast <- (lp * hp) / madElev
rast

# but they largely look the same
plot(c(mapcalc, rast))
plot(mapcalc - rast)

}
