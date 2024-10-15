if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Plant specimens (points) and land cover
madDypsis <- fastData("madDypsis")
madCover <- fastData("madCover")

# Convert to GVector and GRaster
dypsis <- fast(madDypsis)
cover <- fast(madCover)

### GVector

# Look at the data table:
as.data.table(dypsis)

# Which rows have no NAs?
complete.cases(dypsis)

# Which rows have at least one NA (opposite of above)?
missing.cases(dypsis)

### GRaster

# Look at the levels table:
levels(cover)

# Which rows of levels table have no NAs?
complete.cases(cover)

# Which rows have at least one NA (opposite of above)?
missing.cases(cover)

}
