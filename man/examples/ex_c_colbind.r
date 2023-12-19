if (grassStarted()) {

# Setup
library(sf)

# Rivers vector
madRivers <- fastData("madRivers")

# Convert sf to a GVector
rivers <- fast(madRivers)

# Subset rivers vector
rivers1 <- rivers[1:2]
rivers2 <- rivers[10:11]

# Concatenate rivers
riversCombo <- c(rivers1, rivers2)
riversCombo

# Add columns
newCol <- data.frame(x = 1:11)
riversCols <- colbind(rivers, newCol)
riversCol

as.data.frame(riversCol)

}
