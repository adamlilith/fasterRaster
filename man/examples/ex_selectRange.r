if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)

# Make a stack of various versions of "elev" from which to select from:
x <- c(elev, 10 * elev, ln(elev), -1 * elev)
x

# Make a layer with random numbers between 1 and 4:
fun <- "= round(rand(0.5, 4.5))"
y <- app(elev, fun = fun)

selected <- selectRange(x, y)

}
