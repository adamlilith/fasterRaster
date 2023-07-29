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

# setup
library(terra)

# elevation raster, rivers vector, locations of Dypsis plants
madElev <- fastData("madElev")

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert SpatRaster to a GRaster:
elev <- fast(madElev)

# Create a set of rasters for us to operate on:
x <- c(elev, elev^2, sqrt(elev))

# Demonstrate check for badly-named rasters:
names(x) <- c("cos", "asin", "exp")
fun <- "= cos / asin + exp"
appCheck(x, fun, failOnBad = FALSE)

# Rename rasters acceptable names and run the function:
names(x) <- c("x1", "x2", "x3")
fun <- "= (x1 / x2) + x3"
appCheck(x, fun, failOnBad = FALSE)
app(x, fun = fun)

# This is the same as:
(x[[1]] / x[[2]]) + x[[3]]

# We can view a Shiny table using appFuns()):
appFuns()

# This is the same as:
data(appFunsTable)

# Apply other functions:
fun <- "= median(x1 / x2, x3, x1 * 2, cos(x2))"
app(x, fun = fun)

fun <- "= round(x1) * tan(x2) + log(x3, 10)"
app(x, fun = fun)

# Demonstrate effects of data type. The "+" sign does not guarantee
# output is of a given type, and the rasters are coerced to integers before
# the operation is conducted in the second function.
fun <- "= x1 + x3"
app(x, fun = fun, ensure = "float") # output is floating-point
app(x, fun = fun, ensure = "integer") # output is integer

# Some functions override the "ensure" argument. In this case, the output will
# not be an integer because the sin() function returns a float value.
fun <- "= sin(x2)"
app(x, fun = fun, ensure = "integer")

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession("examples")

}
