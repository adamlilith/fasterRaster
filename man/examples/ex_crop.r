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
grassDir <- "/Applications/GRASS-8.3.app/Contents/Resources" # Mac
grassDir <- "C:/Program Files/GRASS GIS 8.3" # Windows
grassDir <- "/usr/local/grass" # Linux

# Eetup
library(sf)
library(terra)

# Elevation raster, plant specimen collections, rivers vector,
# outline of area vector
madElev <- fastData("madElev")
madDypsis <- fastData("madDypsis")
madRivers <- fastData("madRivers")
madCoast4 <- fastData("madCoast4")
madAnt <- madCoast4[madCoast4$NAME_4 == "Antanambe", ]

# Start GRASS session for examples only:
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert to fasterRaster format:
elev <- fast(madElev)
dypsis <- fast(madDypsis)
rivers <- fast(madRivers)
coast <- fast(madCoast4)
ant <- fast(madAnt)

### Crop raster by vector:
rastByVect <- crop(elev, ant)
plot(elev, col = "gray")
plot(rastByVect, add = TRUE)
plot(ant, add = TRUE)

### Crop raster by raster:
# We'll cheat by making the raster smaller in R, then crop the
# raster by this.
templateRast <- crop(madElev, madAnt)

template <- fast(templateRast)
rastByRast <- crop(elev, template)

plot(elev, col = "gray")
plot(rastByRast, add = TRUE)

### Crop vector by raster:
# We'll cheat by making the raster smaller in R, then crop the
# raster by this.
templateRast <- crop(madElev, madAnt)

template <- fast(templateRast)
ptsByRast <- crop(dypsis, template)

plot(elev, col = "gray", legend = FALSE)
plot(template, add = TRUE)
plot(dypsis, add = TRUE)
plot(ptsByRast, pch = 21, bg = "red", add = TRUE)

### Crop vector by vector:
ptsSubset <- dypsis[1:10] # use first 10 points as cropping template

# Crop points vector by convex hull around points:
ptsByPts <- crop(dypsis, ptsSubset)
plot(dypsis)
plot(convHull(ptsSubset), lty = "dotted", col = "blue", add = TRUE)
plot(ptsByPts, col = "red", add = TRUE)
plot(ptsSubset, col = "blue", pch = 3, cex = 1.6, add = TRUE)
legend("topleft",
    legend = c("Dypsis", "Selected", "Crop template", "Convex hull"),
    pch = c(16, 16, 3, NA),
    lwd = c(NA, NA, NA, 1),
    col = c("black", "red", "blue", "blue"),
    lty = c(NA, NA, NA, "dotted"),
    xpd = NA,
    bg = "white"
)

# Crop points vector by extent of points:
ptsByPts <- crop(dypsis, ptsSubset, ext = TRUE)
plot(dypsis)
plot(ptsByPts, col = "red", add = TRUE)
plot(ptsSubset, col = "blue", pch = 3, cex = 1.6, add = TRUE)
legend("topleft",
    legend = c("Dypsis", "Selected", "Crop template"),
    pch = c(16, 16, 3),
    lwd = c(NA, NA, NA),
    col = c("black", "red", "blue"),
    lty = c(NA, NA, NA),
    xpd = NA,
    bg = "white"
)

# Crop points vector by convex hull around lines:
ptsByPts <- crop(dypsis, rivers)
plot(rivers, col = "blue", pch = 3, cex = 1.6)
plot(dypsis, add = TRUE)
plot(convHull(rivers), lty = "dotted", border = "blue", add = TRUE)
plot(ptsByPts, col = "red", add = TRUE)
legend("topleft",
    legend = c("Dypsis", "Selected", "Rivers", "Convex hull"),
    pch = c(16, 16, NA, NA),
    lwd = c(NA, NA, 1, 1),
    col = c("black", "red", "blue", "blue"),
    lty = c(NA, NA, "solid", "dotted"),
    xpd = NA,
    bg = "white"
)

# Crop points vector by extent of lines:
ptsByPts <- crop(dypsis, rivers, ext = TRUE)
plot(rivers, col = "blue", pch = 3, cex = 1.6)
plot(dypsis, add = TRUE)
plot(convHull(rivers), lty = "dotted", border = "blue", add = TRUE)
plot(ptsByPts, col = "red", add = TRUE)
legend("topleft",
    legend = c("Dypsis", "Selected", "Rivers"),
    pch = c(16, 16, NA),
    lwd = c(NA, NA, 1),
    col = c("black", "red", "blue"),
    lty = c(NA, NA, "solid"),
    xpd = NA,
    bg = "white"
)

# Crop points vector by polygon:
ptsByPts <- crop(dypsis, ant)
plot(dypsis)
plot(ant, border = "blue", pch = 3, cex = 1.6, add = TRUE)
plot(ptsByPts, col = "red", add = TRUE)
legend("topleft",
    legend = c("Dypsis", "Selected", "Antanambe"),
    pch = c(16, 16, NA),
    lwd = c(NA, NA, 1),
    col = c("black", "red", "blue"),
    lty = c(NA, NA, "solid"),
    xpd = NA,
    bg = "white"
)

# Crop lines vector by polygon:
linesByPoly <- crop(rivers, ant)
plot(rivers)
plot(ant, border = "blue", pch = 3, cex = 1.6, add = TRUE)
plot(linesByPoly, col = "red", add = TRUE)
legend("topleft",
    legend = c("Rivers", "Selected", "Antanambe"),
    col = c("black", "red", "blue"),
    lwd = 1,
    xpd = NA,
    bg = "white"
)

# Crop polygon vector by convex hull around points:
polyByPoints <- crop(ant, dypsis)
plot(dypsis, col = "red")
plot(ant, border = "blue", add = TRUE)
plot(polyByPoints, border = "red", add = TRUE)
legend("topleft",
    legend = c("Dypsis", "Antanambe", "Selected"),
    col = c("red", "blue", "red"),
    pch = c(16, NA, NA),
    lwd = c(NA, 1, 1),
    xpd = NA,
    bg = "white"
)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession("examples")

}
