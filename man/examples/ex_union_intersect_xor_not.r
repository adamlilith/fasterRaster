if (grassStarted()) {

# Setup
library(sf)

# Polygon of coastal Madagascar and Dypsis specimens
madCoast4 <- fastData("madCoast4") # polygons
madDypsis <- fastData("madDypsis") # points

# Convert vectors:
coast4 <- fast(madCoast4)
dypsis <- fast(madDypsis)

# Create another polygons vector from a convex hull around Dypsis points
hull <- convHull(dypsis)

### union()
###########

unioned <- union(coast4, hull)
plot(unioned)

plus <- coast4 + hull # same as union()

### intersect
#############

inter <- intersect(coast4, hull)
plot(coast4)
plot(hull, border = "red", add = TRUE)
plot(inter, border = "blue", add = TRUE)

### xor
#######

xr <- xor(coast4, hull)
plot(coast4)
plot(xr, border = "blue", add = TRUE)

### not
#######

no <- not(coast4, hull)
plot(coast4)
plot(no, border = "blue", add = TRUE)

minus <- coast4 - hull # same as not()

}
