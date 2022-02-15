##### Reading and writing spatial data in R #####

# download files

download.file("https://github.com/mgimond/Spatial/raw/main/Data/Income_schooling.zip", 
              destfile = "Income_schooling.zip" , mode='wb')
unzip("Income_schooling.zip", exdir = ".")
file.remove("Income_schooling.zip")

download.file("https://github.com/mgimond/Spatial/raw/main/Data/rail_inters.gpkg", 
              destfile = "./rail_inters.gpkg", mode='wb')

download.file("https://github.com/mgimond/Spatial/raw/main/Data/elev.img",  
              destfile = "./elev.img", mode='wb')

# reading a shapefile
library(sf)
s.sf <- st_read("Income_schooling.shp")
# use only .shp file

# view first few records
head(s.sf, n=4)
# stores geometry and coordinate system information and attribute data

# reading a geopackage
st_layers("rail_inters.gpkg")
# two separate layers in gpkg

# extract each layer
inter.sf <- st_read("rail_inters.gpkg", layer="Interstate")
rail.sf <- st_read("rail_inters.gpkg", layer="Rail")

# reading a raster
rgdal::gdalDrivers()

# read Imagine raster file
library(raster)
elev.r <- raster("elev.img")

# raster objects are by default not loaded into memory. to check
inMemory(elev.r)

# to force raster into memory use, for reducing performance cost, use readAll()
elev.r <- readAll(raster("elev.img"))

elev.r

### creating a spatial object from a data frame ####
# need to specify the coordinate system used to record the coordinate pairs

# Create a simple dataframe with lat/long values
df <- data.frame(lon = c(-68.783, -69.6458, -69.7653),
                 lat = c(44.8109, 44.5521, 44.3235),
                 Name= c("Bangor", "Waterville", "Augusta"))

# Convert the dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the 
# spatial object
p.sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
p.sf  

### converting from an sf object ####
# convert sf object to a spatial* object (spdep/sp)
s.sp <- as(s.sf, "Spatial")
class(s.sp)

### converting sf polygon into an owin object ####
# requires the use of maptools package
library(maptools)
s.owin <- as(s.sp, "owin")
# Error: 'spatstat.options' is not an exported object from 'namespace:spatstat'

### converting sf point to a ppp object ####
p.sp  <- as(p.sf, "Spatial")  # Create Spatial* object
p.ppp <- as(p.sp, "ppp")      # Create ppp object

# project p.sf object to a UTM coordinate system
p.sf.utm <- st_transform(p.sf, 32619) # project from geographic to UTM
p.sp  <- as(p.sf.utm, "Spatial")      # Create Spatial* object
p.ppp <- as(p.sp, "ppp")              # Create ppp object
class(p.ppp)
# Error: 'owin' is not an exported object from 'namespace:spatstat'

### converting a raster object to an im object ####
elev.im <- as.im.RasterLayer(elev.r) # From the maptools package
class(elev.im)
# Error: 'transmat' is not an exported object from 'namespace:spatstat'

### converting to an sf object ####
st_as_sf(p.ppp)  # For converting a ppp object to an sf object
st_as_sf(s.sp)   # For converting a Spatial* object to an sf object

### dissecting the sf file object ####
head(s.sf,3)
extent(s.sf)
st_crs(s.sf)

# extract object's table to a dedicated dataframe
s.df <- data.frame(s.sf)
class(s.df)
head(s.df, 5)
str(s.df)

# choose to remove column with geometry
s.nogeom.df <- st_set_geometry(s.sf, NULL)
class(s.nogeom.df)
head(s.nogeom.df,5)

#### exporting sf object to different data file formats ####
st_write(s.sf, "shapefile_out.shp", driver="ESRI Shapefile")  # create to a shapefile 
st_write(s.sf, " s.gpkg", driver="GPKG")  # Create a geopackage file
# one drive has trouble syncing with .gpkg files

writeRaster(elev.r, "elev_out.tif", format="GTiff" ) # Create a geoTiff file
writeRaster(elev.r, "elev_out.img", format="HFA" )  # Create an Imagine raster file
