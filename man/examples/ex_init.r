if (grassStarted()) {

# Setup
library(terra)

# Elevation raster, rivers vector
madElev <- fastData("madElev")

# Convert to a GRaster
elev <- fast(madElev)

# Cell coordinates
init(elev, "x")
init(elev, "y")

# Cell row or column
init(elev, "row")
init(elev, "col")

# Chess
elevAgg <- aggregate(elev, 32) # make cells bigger so we can see

chessOdd <- init(elevAgg, "chess")
chessEven <- init(elevAgg, "chess", odd = FALSE)

chess <- c(chessOdd, chessEven)
names(chess) <- c("odd", "even")
plot(chess)

# Chess with user-defined values
elevAgg <- aggregate(elev, 32) # make cells bigger so we can see

chessOdd13 <- init(elevAgg, "chess", vals = c(0, 13))
chessEven13 <- init(elevAgg, "chess", odd = FALSE, vals = c(0, 13))

chess13 <- c(chessOdd13, chessEven13)
names(chess13) <- c("odd", "even")
plot(chess13)

# Regular
elevAgg <- aggregate(elev, 32) # make cells bigger so we can see

regOdd <- init(elevAgg, "regular")
regEven <- init(elevAgg, "regular", odd = FALSE)

reg <- c(regOdd, regEven)
names(reg) <- c("odd", "even")
plot(reg)

}
