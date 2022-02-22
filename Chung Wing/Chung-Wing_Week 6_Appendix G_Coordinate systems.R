##note that PROJ has changed significantly in newer versions

#load data
load(url("https://github.com/mgimond/Spatial/raw/main/Data/Sample1.RData"))
#remove unnecessary data objects from environment (not used here)
rm(list=c("inter.sf", "p.sf", "rail.sf"))
#load libraries
library(sf)
#check linked library versions
sf_extSoftVersion()[1:3]

#extract coordinate system info
st_crs(s.sf)
#extract CS info from raster with crs function
library(raster)
crs(elev.r)
##notes that output format differs (WKT string vs. simplified?)
##CRS can be defined through EPSG numeric code or PROJ4 formatted string
##or newer version can define sf object CRS using Well Known Text format
##prefer to stay with EPSG code for simplicity to manually define a CS
##long list of parameters in PROJ4 syntax 

#assigning a coordinate system (can fill empty definition or overwrite existing)\
s.sf <- st_set_crs(s.sf, "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") #assigning manually
st_crs(s.sf) #check object's CS
##note: some parameters now undefined
#so now define EPSG code
s.sf <- st_set_crs(s.sf, 26919)
st_crs(s.sf)  
#defining CRS for raster
crs(elev.r) <- "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83"
crs(elev.r) #checking it
#define CRS using EPSG code
crs(elev.r) <- "+init=EPSG:26919"
crs(elev.r) 
##note: recreate defined CS in software (ArcGIS) by extracting CS WKID/EPSG code
##then use that code to look up PROJ4 syntax

#transform coordinate values to a diff coordinate system
##process calculates new coordinate pair values for all points defining spatial object
s.sf.gcs <- st_transform(s.sf, "+proj=longlat +datum=WGS84")
st_crs(s.sf.gcs)
#using EPSG code equivalent instead of proj4 string
s.sf.gcs <- st_transform(s.sf, 4326)
st_crs(s.sf.gcs)
#transform raster object
elev.r.gcs <- projectRaster(elev.r, crs="+proj=longlat +datum=WGS84")
crs(elev.r.gcs)
#transform raster using EPSG code
elev.r.gcs <- projectRaster(elev.r, crs="+init=EPSG:4326")
crs(elev.r.gcs)

#overlay transformation with OpenStreetMap to check transformation
library(leaflet)
leaflet(s.sf.gcs) %>% 
  addPolygons() %>% 
  addTiles()

#explore other transformations
library(tmap)
data(World)  #dataset is stored as an sf object

#check its current coordinate system
st_crs(World)
#transforms world map to custom azimuthal equidistant projection centered on 0,0
World.ae <- st_transform(World, 
                         "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 
                         +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
st_crs(World.ae) 
tm_shape(World.ae) + tm_fill() #different map based on projection
#now centered on Maine
World.aemaine <- st_transform(World, 
                              "+proj=aeqd +lat_0=44.5 +lon_0=-69.8 
                              +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World.aemaine) + tm_fill()  
#transforms to World Robinson projection
World.robin <- st_transform(World,
                            "+proj=robin +lon_0=0 +x_0=0 +y_0=0 
                            +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World.robin) + tm_fill()  
#transform to World sinusoidal projection
World.sin <- st_transform(World,
                          "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 
                          +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World.sin) + tm_fill()  
#transform to World Mercator projection

World.mercator <- st_transform(World,
                               "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 
                               +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World.mercator) + tm_fill()  
#example of a failed transformation
##note: issue when polygon features are split across 180 deg meridian
World.mercator2 <- st_transform(World, 
                                "+proj=merc +lon_0=-69 +k=1 +x_0=0 
                                +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World.mercator2) + tm_borders()
##polygons are split and R doesn't know how to piece them together

#can fix with maptools - requires object be of "Spatial" type and geographic reference system
library(maptools)
library(rgeos)
#convert to lat/long reference system
wld.ll <- st_transform(World, "+proj=longlat +datum=WGS84 +no_defs")
#convert to a spatial object, then split the polygons at a given longitude (111 in this example)
wld.sp <- nowrapSpatialPolygons(as(wld.ll, "Spatial"), offset = 111)
#convert back to an sf object, reproject to a new longitude center at -69 degrees 
#then plot it
wld.sf <- st_as_sf(wld.sp)
wld.merc2.sf <- st_transform(wld.sf, "+proj=merc +lon_0=-69 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(wld.merc2.sf) + tm_borders()
#remove deformed polygon or use diff projection
#this uses Robinson projection
wld.rob.sf <- st_transform(wld.sf,
                           "+proj=robin +lon_0=-69 +x_0=0 +y_0=0 
                           +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(wld.rob.sf) + tm_borders()
#now lost attribute table data
head(data.frame(wld.rob.sf), 4)
#can also create centroid from polygons then spatial join (to retain attribute info)
#create centroids from polygons
pt <- st_centroid(World, of_largest_polygon = TRUE)
#transform points to the recentered Robinson projection
pt.rob <- st_transform(pt,"+proj=robin +lon_0=-69 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#perform the spatial join (joining the point attribute values to the wld.rob.sf polygons)
wld.rob.df.sf <- st_join(wld.rob.sf, pt.rob, join = st_contains)
# map the output
tm_shape(wld.rob.df.sf) + tm_polygons(col="pop_est_dens", style="quantile") +
  tm_legend(outside=TRUE)
#missing values --zoom in using unprojected layers and overlap map with centroids
library(dplyr)
#extract the extent for the Norwar/Sweden region
nor.bb <- World %>% filter(name == "Norway" | name == "Sweden") %>% st_bbox()
#plot the data zoomed in on the region. Add the point layer for reference
tm_shape(World, bbox=nor.bb) + tm_polygons(col="pop_est_dens", style="quantile") +
  tm_shape(pt) + tm_dots() +
  tm_text("name", just="left", xmod=0.5, size=0.8) +
  tm_legend(outside=TRUE)
#st_centroid uses geometric center rather than center of mass (so curved shapes don't contain)
#place points inside polygons instead
pt <- st_point_on_surface(World)
pt.rob <- st_transform(pt,"+proj=robin +lon_0=-69 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
wld.rob.df.sf <- st_join(wld.rob.sf, pt.rob, join = st_contains)
tm_shape(wld.rob.df.sf) + tm_polygons(col="pop_est_dens", style="quantile") +
  tm_legend(outside=TRUE)

##note: transformations affect points, not line segments - so points enclosed may sometimes fall outside polygon when projection changes
#define a few projections
miller <- "+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
lambert <- "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#subset the World data layer
wld.mil <-  World %>% filter( iso_a3  == "CAN" |  iso_a3 == "USA") %>% st_transform( miller)
#create polygon and point layers in the Miller projection  
sf1 <- st_sfc( st_polygon(list(cbind(c(-13340256,-13340256,-6661069, -6661069, -13340256),
                                     c(7713751, 5326023, 5326023,7713751, 7713751 )))), crs = miller) 

pt1 <- st_sfc( st_multipoint(rbind(c(-11688500,7633570), c(-11688500,5375780),
                                   c(-10018800,7633570), c(-10018800,5375780),
                                   c(-8348960,7633570), c(-8348960,5375780))),  crs = miller)
pt1 <- st_cast(pt1, "POINT") #create single part points (geometry split into diff rows)
#plot the data layers in their native projection
tm_shape(wld.mil) +tm_fill(col="grey") + 
  tm_graticules(x = c(-60,-80,-100, -120, -140), 
                y = c(30,45, 60), 
                labels.col = "white", col="grey90") +
  tm_shape(sf1) + tm_polygons("red", alpha = 0.5, border.col = "yellow") +
  tm_shape(pt1) + tm_dots(size=0.2) 
#confirm that points are contained inside polygon
st_contains(sf1, pt1)
#reproject data into Lambert conformal projection
#transform the data
wld.lam <- st_transform(wld.mil, lambert)
pt1.lam <- st_transform(pt1, lambert)
sf1.lam <- st_transform(sf1, lambert)
#plot the data in the Lambert coordinate system
tm_shape(wld.lam) +tm_fill(col="grey") + 
  tm_graticules( x = c(-60,-80,-100, -120, -140), 
                 y = c(30,45, 60), 
                 labels.col = "white", col="grey90") +
  tm_shape(sf1.lam) + tm_polygons("red", alpha = 0.5, border.col = "yellow") +
  tm_shape(pt1.lam) + tm_dots(size=0.2)   
#check if points contained
st_contains(sf1.lam, pt1.lam) #only 3/6 points contained
#to resolve, densify polygon by adding vertices (based on resolution needed)
#add vertices every 1000 meters along the polygon's line segments
sf2 <- st_segmentize(sf1, 1000)
#transform the newly densified polygon layer
sf2.lam <- st_transform(sf2, lambert)
#plot the data
tm_shape(wld.lam) + tm_fill(col="grey") + 
  tm_graticules( x = c(-60,-80,-100, -120, -140), 
                 y = c(30,45, 60), 
                 labels.col = "white", col="grey90") +
  tm_shape(sf2.lam) + tm_polygons("red", alpha = 0.5, border.col = "yellow") +
  tm_shape(pt1.lam) + tm_dots(size=0.2) 
#check contains
st_contains(sf2.lam, pt1.lam)

#projections create distortions of area and shape
#visualise distortions by creating geodesic circles
#create point layer to define circle centers
tissot.pt <- st_sfc( st_multipoint(rbind(c(-60,30), c(-60,45), c(-60,60),
                                         c(-80,30), c(-80,45), c(-80,60),
                                         c(-100,30), c(-100,45), c(-100,60),
                                         c(-120,30), c(-120,45), c(-120,60) )),  crs = "+proj=longlat")
tissot.pt <- st_cast(tissot.pt, "POINT") #create single part points
#create geodesic circles from these points
library(geosphere)
cr.pt <- list() #create an empty list
#loop through each point in tissot.pt and generate 360 vertices at 300 km
#from each point in all directions at 1 degree increment. These vertices
#will be used to approximate the Tissot circles
for (i in 1:length(tissot.pt)){
  cr.pt[[i]] <- list( destPoint( as(tissot.pt[i], "Spatial"), b=seq(0,360,1), d=300000) )
}
#create a closed polygon from the previously generated vertices
tissot.sfc <- st_cast( st_sfc(st_multipolygon(cr.pt ),crs = "+proj=longlat"), "POLYGON" )

#check these are geodesic circles by computing geodesic area of each polygon
tissot.sf <- st_sf( geoArea =  st_area(tissot.sfc), tissot.sfc )
#compute error in output and report as fractions
( (pi * 300000^2) -  as.vector(tissot.sf$geoArea) ) / (pi * 300000^2)
##small errors due to discretization of circle parameter

#distortions associated with popular CS
#mercator projection
#transform geodesic circles and compute area error as a percentage
tissot.merc <- st_transform(tissot.sf, "+proj=merc +ellps=WGS84")
tissot.merc$area_err <- round((st_area(tissot.merc, tissot.merc$geoArea)) / 
                                tissot.merc$geoArea * 100 , 2)
#plot the map
tm_shape(World, bbox = st_bbox(tissot.merc), projection = st_crs(tissot.merc)) + 
  tm_borders() + 
  tm_shape(tissot.merc) + tm_polygons(col="grey", border.col = "red", alpha = 0.3) + 
  tm_graticules(x = c(-60,-80,-100, -120, -140), 
                y = c(30,45, 60),
                labels.col = "white", col="grey80") +
  tm_text("area_err", size=.8, alpha=0.8, col="blue")
##shape preserved but distortion increases poleward

#Lambert azimuthal equal area projection
#transform geodesic circles and compute area error as a percentage
tissot.laea <- st_transform(tissot.sf, "+proj=laea +lat_0=45 +lon_0=-100 +ellps=WGS84")
tissot.laea$area_err <- round( (st_area(tissot.laea ) - tissot.laea$geoArea) / 
                                 tissot.laea$geoArea * 100, 2)
#plot the map
tm_shape(World, bbox = st_bbox(tissot.laea), projection = st_crs(tissot.laea)) + 
  tm_borders() + 
  tm_shape(tissot.laea) + tm_polygons(col="grey", border.col = "red", alpha = 0.3) + 
  tm_graticules(x=c(-60,-80,-100, -120, -140), 
                y = c(30,45, 60),
                labels.col = "white", col="grey80") +
  tm_text("area_err", size=.8, alpha=0.8, col="blue")
##area error low but shape distorts away from projection center

#Robinson projection
#transform geodesic circles and compute area error as a percentage
tissot.robin <- st_transform(tissot.sf, "+proj=robin  +ellps=WGS84")
tissot.robin$area_err <- round(  (st_area(tissot.robin ) - tissot.robin$geoArea) / 
                                   tissot.robin$geoArea * 100, 2)
#plot the map
tm_shape(World, bbox = st_bbox(tissot.robin), projection = st_crs(tissot.robin)) + 
  tm_borders() + 
  tm_shape(tissot.robin) + tm_polygons(col="grey", border.col = "red", alpha = 0.3) + 
  tm_graticules(x=c(-60,-80,-100, -120, -140), 
                y = c(30,45, 60),
                labels.col = "white", col="grey80") +
  tm_text("area_err", size=.8, alpha=0.8, col="blue")
##both area and shape are distorted for North American continent

