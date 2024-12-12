if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Classify using a scalar indicating number of bins
scalar <- classify(elev, 5)
scalar
levels(scalar)

# Classify using a vector, indicating bin break points
vector <- classify(elev, rcl = c(0, 100, 200, 300, 400, 500, 600))
vector
levels(vector)

# Classify using a 2-column matrix (only valid for integer rasters)
rcl <- data.frame(is = c(1:3, 5, 10), becomes = c(100:102, 105, 110))
twoCol <- classify(elev, rcl = rcl)
twoCol

# Classify using a 3-column table
rcl <- data.frame(
   from = c(0, 100, 200, 300, 400, 500),
   to = c(100, 200, 300, 400, 500, 600),
   becomes = c(1, 2, 3, 10, 12, 15)
)
threeCol <- classify(elev, rcl = rcl)
threeCol
levels(threeCol)

# Convert all values outside range to NA (default)
rcl <- c(100, 200, 300)
v1 <- classify(elev, rcl = rcl)
v1
plot(v1)

# Convert all values outside range to -1
rcl <- c(100, 200, 300)
v2 <- classify(elev, rcl = rcl, others = -1)
v2
plot(v2)

### Left-open/right-closed (default)
minmax(elev) # note min/max values
rcl <- c(1, 200, 570)
v3 <- classify(elev, rcl = rcl, others = 10)
levels(v3)
plot(v3)

### Left-closed/right-open
minmax(elev) # note min/max values
rcl <- c(1, 200, 570)
v4 <- classify(elev, rcl = rcl, others = 10, right = FALSE)
levels(v4)

# Left-open except for lowest bin/right-closed
minmax(elev) # note min/max values
rcl <- c(1, 200, 570)
v5 <- classify(elev, rcl = rcl, others = 10, include.lowest = TRUE)
v5 <- droplevels(v5)
levels(v5)

# Left-closed/right-open except for highest bin
minmax(elev) # note min/max values
rcl <- c(1, 200, 570)
v6 <- classify(elev, rcl = rcl, others = 10,
   right = FALSE, include.lowest = TRUE)
v6 <- droplevels(v6)
levels(v6)

}
