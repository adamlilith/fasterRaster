if (grassStarted()) {

# Setup
library(terra)

# Example data
madRivers <- fastData("madRivers")

# Convert SpatRasters to GRasters
rivers <- fast(madRivers)
rivers

# Save using a temporary file name
filename <- tempfile()

### Save GVector to disk as GeoPackage
fn <- paste0(filename, ".gpkg")
writeVector(rivers, fn)

### Save GVector to disk as ESRI Shapefile
fn <- paste0(filename, ".shp")
writeVector(rivers, filename)

### Save GVector to disk as Google Earth KML
fn <- paste0(filename, ".kml")
writeVector(rivers, filename)

### Save GVector data table to disk as comma-separated file
fn <- paste0(filename, ".csv")
writeVector(rivers, filename)

### Save GVector data table to disk as NetCDF
fn <- paste0(filename, ".ncdf")
writeVector(rivers, filename)

### Save GVector data table to disk as Excel file
fn <- paste0(filename, ".xlsx")
writeVector(rivers, filename)

### load raster from disk
rivers2 <- fast(paste0(filename, ".gpkg"))
rivers2

}
