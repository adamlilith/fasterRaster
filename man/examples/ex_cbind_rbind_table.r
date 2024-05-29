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
riversCombo <- rbind(rivers1, rivers2)
riversCombo

# Add columns
newCol <- data.frame(new = 1:11)
riversCol <- colbind(rivers, newCol)
riversCol

as.data.frame(riversCol)

# Remove table
riversCopy <- rivers
riversCopy # 11 rows in data table
table(riversCopy) <- NULL
riversCopy # no data table

# Add a new table
newTable <- data.frame(num = 1:11, letters = letters[1:11])
table(riversCopy) <- newTable
riversCopy

}
