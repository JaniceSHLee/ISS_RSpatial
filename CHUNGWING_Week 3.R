####downloading sample files from github####
download.file("https://github.com/mgimond/Spatial/raw/main/Data/Income_schooling.zip", 
              destfile = "Income_schooling.zip" , mode='wb')
unzip("Income_schooling.zip", exdir = ".")
file.remove("Income_schooling.zip")
download.file("https://github.com/mgimond/Spatial/raw/main/Data/rail_inters.gpkg", 
              destfile = "./rail_inters.gpkg", mode='wb')
download.file("https://github.com/mgimond/Spatial/raw/main/Data/elev.img",  
              destfile = "./elev.img", mode='wb')

#libraries needed
library(sf)
#### read shapefile ####
s.sf <- st_read("Income_schooling.shp")
## only need .shp file for sf
#look at first few records in the spatial data object
head(s.sf, n=4)
## geometry type: multipolygon - multiple sheet files/vectors
## projected CRS (coordinate reference system) based on where globally
## table is attribute table containing info related to each polygon
### this also contains geometry for each polygon in file

#### read geopackage ####
# list available layers in geopackage
st_layers("rail_inters.gpkg")
# extract each layer separately
inter.sf <- st_read("rail_inters.gpkg", layer="Interstate")
rail.sf  <- st_read("rail_inters.gpkg", layer="Rail")

#### read raster ####
library(raster)
library(rgdal)
#reading Imagine raster file
elev.r <- raster("elev.img")
#check if loaded into memory
inMemory(elev.r)
#force raster into memory
elev.r <- readAll(raster("elev.img"))
#check again
inMemory(elev.r)
#look at raster's properties
elev.r

#### convert dataframe to spatial object ####
#create a simple dataframe with lat/long values
df <- data.frame(lon = c(-68.783, -69.6458, -69.7653),
                 lat = c(44.8109, 44.5521, 44.3235),
                 Name= c("Bangor", "Waterville", "Augusta"))

#convert the dataframe to a spatial object. Note that the
#crs= 4326 parameter assigns a WGS84 coordinate system to the 
# spatial object
p.sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326) 
# coords argument specifies columns to pull from
p.sf  

#### converting sf object (for unsupported pkgs) ####
#to spatial object 
s.sp <- as(s.sf, "Spatial")
class(s.sp)
#to owin object (from sf polygon)
library(maptools)
s.owin <- as(s.sp, "owin")
class(s.owin)
#to ppp object
p.sp  <- as(p.sf, "Spatial")  # Create Spatial* object
p.ppp <- as(p.sp, "ppp")      # Create ppp object
#error: need to project p.sp or ps.f layer to projected coordinate system
p.sf.utm <- st_transform(p.sf, 32619) # project from geographic to UTM
p.sp  <- as(p.sf.utm, "Spatial")      # Create Spatial* object
p.ppp <- as(p.sp, "ppp")              # Create ppp object
class(p.ppp)

#raster to im
elev.im <- as.im.RasterLayer(elev.r) # From the maptools package
class(elev.im)

#all except owin can go back to sf object with same function
st_as_sf(p.ppp)  # For converting a ppp object to an sf object
st_as_sf(s.sp)   # For converting a Spatial* object to an sf object

#dissecting sf object
head(s.sf,3)
#extract extent (boundary extent)
extent(s.sf)
#extract coordinate info
st_crs(s.sf)
#extract the object’s table to a dedicated data frame
s.df <- data.frame(s.sf)
class(s.df)
head(s.df, 5)
#geometry column stores coordinates as a list (makes it more flexible)
str(s.df)
#can also remove geometry column
s.nogeom.df <- st_set_geometry(s.sf, NULL) 
class(s.nogeom.df)
head(s.nogeom.df, 5)

#### export sf file to other file formats ####
st_write(s.sf, "shapefile_out.shp", driver="ESRI Shapefile")  # create to a shapefile 
st_write(s.sf, " s.gpkg", driver="GPKG")  # Create a geopackage file
#export raster to data file
writeRaster(elev.r, "elev_out.tif", format="GTiff" ) # Create a geoTiff file
writeRaster(elev.r, "elev_out.img", format="HFA" )  # Create an Imagine raster file
#name column is format parameter name used in the writeRaster() function

#### creating point sf object ####

library(sf)
#1. creating point geometry
p1.sfg <- st_point(c(-70, 45)) 
p2.sfg <- st_point(c(-69, 44)) 
p3.sfg <- st_point(c(-69, 45)) 
#check class -look for sfg
class(p1.sfg)
#if multipart point feature object is desired, define points in matrix
#st_multipoint(matrix(c(-70, 45, -69, 44, -69, 45), ncol = 2, byrow = TRUE))

#2. combine point geometries into single object (simple feature column - sfc)
#can define coordinate system here
p.sfc <- st_sfc( list(p1.sfg, p2.sfg, p3.sfg), crs = 4326 )
class(p.sfc)
#each point was assigned its own component via the list function
p.sfc
#can access each point (for example, point 2)
p.sfc[[2]]

#3. create simple feature object
p.sf <- st_sf(p.sfc)
p.sf
attributes(p.sf)
#check sf_column to make sure geometry column is correctly specified
#rename column and redefine geometry column
names(p.sf) <- "coords"
st_geometry(p.sf) <- "coords"
p.sf
typeof(p.sf$coords) #looking at datatype
#adding attributes
p.sf$val1 <- c("A", "B", "C") #new column
p.sf
#graph points
plot(p.sf, pch = 16, axes = TRUE, main = NULL)
#adding geometry column to existing non-spatial dataframe
##can append sfc object to existing dataframe
#create dataframe
df <- data.frame(col1 = c("A", "B","C"))
#append geometry column
st_geometry(df) <- p.sfc
#now a spatial feature object
df

#### create polyline sf object ####

#define vertices
#order of vertices matters: defines each connecting line segment ends
#coordinate pairs stored in a matrix
l <- rbind(c(-70, 45), c(-69, 44), c(-69, 45))
#create a polyline geometry object
l.sfg <- st_linestring(l)
#create the simple feature column and add reference system
l.sfc <- st_sfc(list(l.sfg), crs = 4326)
#create simple feature object
l.sf <- st_sf(l.sfc)
l.sf
#multiple line segments share single polyline feature (and thus attribute)
plot(l.sf, type = "b", pch = 16, main = NULL, axes = TRUE)

#create branching polyline features
#just make sure coordinate values for the overlapping vertices share the same values
#define coordinate pairs
l1 <- rbind( c(-70, 45), c(-69, 44), c(-69, 45) )
l2 <- rbind( c(-69, 44), c(-70, 44) )
l3 <- rbind( c(-69, 44), c(-68, 43) )
#create simple feature geometry object
l.sfg <- st_multilinestring(list(l1, l2, l3))
#create simple feature column object
l.sfc <- st_sfc(list(l.sfg), crs = 4326)
#create simple feature object
l.sf <- st_sf(l.sfc)
#plot the data
plot(l.sf, type = "b", pch = 16, axes = TRUE)

#### create polygon sf object ####

#take note of direction of "inside": CCW for sf, CW for shapefile
##generally importing shapefile into R automatically reverses direction

#define vertices of each polygon in a matrix
poly1.crd <- rbind(c(-66, 43), c(-70, 47), c(-70,43),  c(-66, 43))
#create list object from each matrix object (will differ between POLYGON and MULTIPOLYGON geometries)
poly1.geom <- st_polygon(list(poly1.crd ))
poly1.geom 
#create sfg polygon geometry object from list (and define crs)
poly.sfc <- st_sfc(list(poly1.geom), crs = 4326)
poly.sfc
#create sf spatial object
poly.sf <- st_sf(poly.sfc)
poly.sf

#can change column name 
names(poly.sf) <- "coords"
st_geometry(poly.sf) <- "coords"
poly.sf
#plot polygon
plot(poly.sf, col = "bisque", axes = TRUE)

#add a hole
#polygon 1
poly1.outer.crd <- rbind( c(-66, 43),c(-70, 47), c(-70,43), c(-66, 43) ) # Outer ring
poly1.inner.crd  <- rbind( c(-68, 44), c(-69,44), c(-69, 45), c(-68, 44) ) # Inner ring
#create list object
poly1.geom <- st_polygon(list(poly1.outer.crd, poly1.inner.crd))
#create simple feature column object
poly.sfc <- st_sfc(list(poly1.geom), crs = 4326)
#create sf object
poly.sf <- st_sf(poly.sfc)
#rename coordinate column
names(poly.sf) <- "coords"
st_geometry(poly.sf) <- "coords"
poly.sf
#plot
plot(poly.sf, col = "bisque", axes = TRUE)

#make TWO polygons
#define coordinate matrix
poly2.crd <- rbind( c(-67, 45),c(-67, 47), c(-69,47), c(-67, 45) ) 
#create polygon geometry
poly2.geom <- st_polygon( list(poly2.crd))
#combine into sf column
poly.sfc <- st_sfc( list(poly1.geom , poly2.geom), crs = 4326 )
#each polygon has own row
poly.sfc
#create sf object
poly.sf <- st_sf(poly.sfc)
poly.sf
#rename geometry column
names(poly.sf) <- "coords"
st_geometry(poly.sf) <- "coords"
poly.sf
#plot
plot(poly.sf, col = "bisque", axes = TRUE)
#adding attributes (order matters!)
poly.sf$id <- c("A", "B")
poly.sf
#plot
plot(poly.sf["id"],  axes = TRUE, main = NULL)

#### multipolygon (multiple polygons share same attribute record) ####

#create multipolygon geometry
mpoly1.sfg  <- st_multipolygon( list(
  list( poly1.outer.crd,  # Outer loop
        poly1.inner.crd), # Inner loop
  list( poly2.crd)) )     # Separate polygon
##groups polygons into single list (hole is combined in sublist)
#create simple feature column object
mpoly.sfc <- st_sfc( list(mpoly1.sfg), crs = 4326)
#create simple feature object
mpoly.sf <- st_sf(mpoly.sfc)
mpoly.sf

##when to use multipolygon vs single polygons depends on what u want to do
#ex: running analysis across diff locations - can combine to multi for easier processing
#vs. wanting to focus on one area, convert to single polygon 
#basically if you need to cluster analysis or separate ?

#mixing multipart and singlepart elements 
##arcGIS can't do this
#overlapping polygons can't be stored as multipart element - will cause problems
poly3.coords <- rbind(c(-66, 44), c(-64, 44), c(-66,47), c(-66, 44))
poly4.coords <- rbind(c(-67, 43), c(-64, 46), c(-66.5,46), c(-67, 43)) 
mpoly1.sfg  <- st_multipolygon( list(
  list( poly1.outer.crd,  # Outer loop
        poly1.inner.crd), # Inner loop
  list( poly2.crd)) )     # Separate poly
mpoly2.sfg  <- st_multipolygon( list(
  list(poly3.coords)))  # Unique polygon
mpoly3.sfg  <- st_multipolygon( list(
  list(poly4.coords)) )  # Unique polygon
#generate simple feature object and assign coordinate system
mpoly.sfc <- st_sfc(list(mpoly1.sfg, mpoly2.sfg, mpoly3.sfg), crs = 4326)
mpoly.sf <- st_sf(mpoly.sfc)
#add attribute values and make transparent
mpoly.sf$ids <- c("A", "B", "C")
plot(mpoly.sf["ids"], axes = TRUE, main = NULL,
     pal = sf.colors(alpha = 0.5, categorical = TRUE))
#make sure simple feature rules not violated
st_is_valid(mpoly.sf)
##returns boolean values - TRUE shows no violation

#### extract geometry from sf object ####
#create sfc from sf
st_geometry(mpoly.sf)
#extract coordinates from single record
st_geometry(mpoly.sf)[[1]]
#extract coordinate pairs from first element in list format
st_geometry(mpoly.sf)[[1]][]

#alternative syntax - bypass matrix creation by using WKT syntax
st_as_sfc("POLYGON ((-66 43, -70 47, -70 43, -66 43), (-68 44, -69 44, -69 45, -68 44))")

#### converting vector --> raster ####
#process is the same for points/polylines/polygons
library(tmap)
##nice pkg to visualise - can zoom in/out and move around in R
#recall data
tm_shape(elev.r) +
  tm_raster(style = "fixed", title = "Elevasi (m)",
            
            breaks = c(0, 100, 200, 400, 800, 1200, 1600),
            palette = rev(heat.colors(6))) +
  tm_shape(inter.sf)+ tm_lines(col="grey80", lwd = 0.7)+
  tm_shape(rail.sf)+ tm_lines(col="grey20", lwd = 0.7)
##rasterization of rail.sf
#prepare raster template using elev data
raster_temp <- raster(elev.r)
##resolution and processing tradeoff
#assign all data into NA
raster_temp[] <- NA
#check values
summary(raster_temp)
#rasterise rail
rail.ras <- raster::rasterize(rail.sf, raster_temp, field = 1:nrow(rail.sf))
#plot
library(mapview)
mapview(rail.ras)

#### converting raster --> vector ####
rail.ras.sf <- raster::rasterToPolygons(rail.ras, dissolve = T)
mapview(rail.ras.sf)
#vectorise elevation
#first reclass the elev
elev.re <- raster::reclassify(elev.r, c(-Inf, 400, 1, 400, 800, 2, 800, 1200, 3, 1200, Inf, 4))
##changing into categorical bins rather than discrete values
#then vectorise it
elev.re.sf <- raster::rasterToPolygons(elev.re, dissolve = T)
mapview(elev.re.sf)
