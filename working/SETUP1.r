

resolution <- raster::res(rast)[1]
proj4 <- raster::projection(rast)
ext <- raster::extent(rast)
xmax<-ext@xmax
xmin<-ext@xmin
ymax<-ext@ymax
ymin<-ext@ymin

tempDir <- tempfile()
dir.create(tempDir, recursive=TRUE, showWarnings=FALSE)



############ set up GRASS RUNTIME environment

# define the GRASS executable path
if(Sys.info()["sysname"] == "Windows"){
   # grass.gis.base<-'C:/OSGeo4W64/apps/grass/grass-7.0.5'
   # grass.gis.base<-'C:/OSGeo4W64/apps/grass/grass78'
   grassDir <-'C:/Program Files/GRASS GIS 7.8'
}else {
   grass.gis.base<-'/usr/lib/grass72'
}

# set path for optional GRASS addons
# Sys.setenv(GRASS_ADDON_PATH="~/.grass7/addons")
Sys.setenv(GRASS_ADDON_PATH="C:/Users/adam/AppData/Roaming/GRASS7/addons")

# create the TEMPORARY GRASS location
rgrass7::initGRASS(gisBase=grass.gis.base,
                    # home=tempdir(),
                    home=tempDir,
                    mapset='PERMANENT',
                    override=TRUE
)

# assign GRASS projection according to data set
rgrass7::execGRASS('g.proj',
                    flags=c('c','quiet'),
                    proj4=proj4
)

# assign GRASS extent and resolution
rgrass7::execGRASS('g.region',
                    flags=c('quiet'),
                    n=as.character(ymax),
                    s=as.character(ymin),
                    e=as.character(xmax),
                    w=as.character(xmin),
                    res=as.character(resolution)
)


#############   now do GRASS STUFF

# writeVECT(meuse,"meuse")
rgrass7::use_sp()
# rgrass7::use_sf()
madElevSGDF <- as(rast, 'SpatialGridDataFrame')
writeRAST(madElevSGDF,"me")

flags <- c('quiet', 'overwrite')
rgrass7::execGRASS('r.latlong', input='me', output='longitude', flags=c(flags, 'l'))
		long <- rgrass7::readRAST('longitude')

		
		