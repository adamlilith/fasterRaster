if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madCoast4 <- fastData("madCoast4")

# Convert to GRaster and crop to a sub-portion (easier for visualizing)
elev <- fast(madElev)
coast4 <- fast(madCoast4)
ant <- coast4[coast4$NAME_4 == "Antanambe"]
elevAnt <- crop(elev, ant)

# Create a set of random points to serve as starting points:
starts <- spatSample(elevAnt, 10, as.points = TRUE, seed = 2)

# Remove points in water:
starts <- starts[complete.cases(starts)]

# Calculate flow paths and label each by ID:
paths <- flowPath(elevAnt, starts)
paths

plot(elevAnt, legend = FALSE, main = "Flow path for each point")
plot(paths, add = TRUE)
plot(starts, pch = 1, add = TRUE)

# Calculate flow paths with cell values indicating the number
# of cells from each start:
seqs <- flowPath(elevAnt, starts, return = "seq")

plot(elevAnt, legend = FALSE, main = "Sequentially-numbered flow paths")
plot(seqs, add = TRUE)
plot(starts, pch = 1, add = TRUE)

# We can convert flow paths to lines:
seqLines <- as.lines(seqs)
plot(seqLines)
seqLines

}
