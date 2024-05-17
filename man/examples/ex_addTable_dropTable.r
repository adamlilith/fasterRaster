if (grassStarted()) {

# Setup
library(sf)

# Points vector of specimens of species in the plant genus Dypsis
madDypsis <- fastData("madDypsis")

# Convert sf to a GVector:
dypsis <- fast(madDypsis)

# Get just the data table:
df <- as.data.frame(dypsis)
head(df)

# Remove the table
dypsisTable <- dropTable(dypsis)
dypsis
dypsisTable

# Add table:
addTable(dypsisTable) <- df
dypsisTable

}
