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

# Setup
library(terra)

# Example data: Land cover raster
madCover <- fastData("madCover")

# Start GRASS session for examples only
faster(x = madCover, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert categorical SpatRaster to categorical GRaster:
cover <- fast(madCover)

# Properties of categorical rasters:
cover # note categories
is.factor(cover) # Is the raster categorical?
nlevels(cover) # number of levels
levels(cover) # just the value and active column
cats(cover) # all columns
minmax(cover) # min/max values
minmax(cover, levels = TRUE) # min/max categories


# Remove unused levels:
nlevels(cover)
cover <- droplevels(cover)
nlevels(cover)

# Frequency of each category (number of cells):
freq(cover)

# Which column sets the category labels?
activeCat(cover)

# Choose a different column for category labels:
levels(cover)
activeCat(cover) <- 2
levels(cover)


# Re-assign levels:
vals <- freq(cover)
value <- c(20, 30, 40, 50, 120, 130, 140, 170, 300)
label <- c("Cropland", "Cropland", "Forest", "Forest", "Forest", "Shrubland", "Herbaceous", "Flooded")

cats <- data.frame(value = value, label = label)
cover <- categories(cover, layer = 1, value = cats)
levels(cover)

# Are there any values not assigned a category?
missingCats(cover)

# Let's assign a category for value 210 (water):
water <- data.frame(value = 210, label = "Water")
cover <- addCats(cover, water)
cover

# We can implement logical operations on categorical rasters:
cover < "Forest" # 1 for cells with a value < 40, 0 otherwise
cover <= "Forest" # 1 for cells with a value < 120, 0 otherwise
cover == "Forest" # 1 for cells with value of 40-120, 0 otherwise
cover != "Forest" # 1 for cells with value that is not 40-120, 0 otherwise
cover > "Forest" # 1 for cells with a value > 120, 0 otherwise
cover >= "Forest" # 1 for cells with a value >= 120, 0 otherwise

cover %in% c("Cropland", "Forest") # 1 for cropland/forest cells, 0 otherwise

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession("examples")

}
