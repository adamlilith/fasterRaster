if (grassStarted()) {

# Setup
library(sf)

# Points vector of specimens of species in the plant genus Dypsis
madCoast0 <- fastData("madCoast0")

# Convert sf to a GVector:
coast <- fast(madCoast0)

### grid
########

# grid specified by number of cells in x-dimension
g1 <- grid(coast, nx = 10)
plot(coast, col = "cornflowerblue")
plot(g1, add = TRUE)

# grid specified by number of cells in x- and y-dimension
g2 <- grid(coast, nx = 10, ny = 5)
plot(coast, col = "cornflowerblue")
plot(g2, add = TRUE)

# grid specified by size of cells in both dimensions
g3 <- grid(coast, nx = 1250, ny = 2000, use = "size")
plot(coast, col = "cornflowerblue")
plot(g3, add = TRUE)

### hexagons
############

hexes <- hexagons(coast, ny = 10)
plot(hexes)
plot(coast, lwd = 2, add = TRUE)

hexes <- hexagons(coast, ny = 10, expand = c(0.3, 0.1))
plot(hexes)
plot(coast, lwd = 2, add = TRUE)

}
