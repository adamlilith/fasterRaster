if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert SpatRaster to a GRaster:
elev <- fast(madElev)

# Create a "stack" of rasters for us to operate on:
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

# We can view a table of app() functions using appFuns():
appFuns()

# We can also get the same table using:
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
app(x, fun = fun, datatype = "float") # output is floating-point
app(x, fun = fun, da = "integer") # output is integer

# Some functions override the "datatype" argument. In this case, the output will
# not be an integer because the sin() function returns a float value.
fun <- "= sin(x2)"
app(x, fun = fun, datatype = "integer")

# Make a raster with random numbers between 1 and 4, with equal probability
# of each:
fun <- "= round(rand(0.5, 4.5))"
rand <- app(elev, fun = fun)
rand

freqs <- freq(rand) # cell frequencies
print(freqs)

}
