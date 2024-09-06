if (grassStarted()) {

# Setup
library(terra)

# Example data
madRivers <- fastData("madRivers")

# What file formats can we attempt to write?
writeVector()

# Convert SpatVector to GVector
rivers <- fast(madRivers)
rivers

# Save GVector to disk as GeoPackage
filename <- tempfile(fileext = ".gpkg")
writeVector(rivers, filename)

# Save GVector to disk as ESRI Shapefile
filename <- tempfile(fileext = ".shp")
writeVector(rivers, filename)

# Save GVector to disk as Google Earth KML
filename <- tempfile(fileext = ".klm")
writeVector(rivers, filename)

# Save GVector data table to disk as comma-separated file
filename <- tempfile(fileext = ".csv")
writeVector(rivers, filename)

# Save GVector data table to disk as NetCDF
filename <- tempfile(fileext = ".ncdf")
writeVector(rivers, filename)

# Save GVector data table to disk as Excel file
filename <- tempfile(fileext = ".xlsx")
writeVector(rivers, filename)

}
