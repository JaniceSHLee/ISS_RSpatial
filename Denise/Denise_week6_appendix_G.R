# Appendix G: coordindate systems in R
#more cautious for data at the polar regions

load(url("https://github.com/mgimond/Spatial/raw/main/Data/Sample1.RData"))

rm(list=c("inter.sf", "p.sf", "rail.sf"))

library(sf)   

#check linked library version - different versions may result in different outcomes
sf_extSoftVersion()[1:3]

# get the coordinate system information
st_crs(s.sf)

library(raster)
crs(elev.r)

# 3 types of defining coordinate system: EPSG, PROG4, WTK. Mostly use EPSG

#maunally assign coordinate system
s.sf <- st_set_crs(s.sf, "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 
st_crs(s.sf)


# another way to define the cs. UTM NAD83 Zone 19N EPSG code equivalent is 26919
s.sf <- st_set_crs(s.sf, 26919)

crs(elev.r) <- "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83"
crs(elev.r) <- "+init=EPSG:26919"
#to chcek the defined CS, look on http://spatialreference.org/ref/

##### transforming coordinate systems #####

#same coordinate system
## using proj4 string
s.sf.gcs <- st_transform(s.sf, "+proj=longlat +datum=WGS84")
st_crs(s.sf.gcs)
##using EPSG code 
s.sf.gcs <- st_transform(s.sf, 4326)
st_crs(s.sf.gcs)

#to transform raster object
elev.r.gcs <- projectRaster(elev.r, crs="+proj=longlat +datum=WGS84")
crs(elev.r.gcs)
## for EPSG code
elev.r.gcs <- projectRaster(elev.r, crs="+init=EPSG:4326")
crs(elev.r.gcs)

# overlapping with web based mapping - to check if the object is properly transformed
library(leaflet) # open street map
leaflet(s.sf.gcs) %>% 
    addPolygons() %>% 
    addTiles()

# exploring other transformations
library(tmap)
data(World)  # The dataset is stored as an sf object

# Let's check its current coordinate system
st_crs(World)

World.ae <- st_transform(World, "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

st_crs(World.ae) 

#mapped output
tm_shape(World.ae) + tm_fill() 

# changeing the projection type, recentering the projection to another area.
World.aemaine <- st_transform(World, "+proj=aeqd +lat_0=44.5 +lon_0=-69.8 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World.aemaine) + tm_fill()  

World.robin <- st_transform(World,"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World.robin) + tm_fill()  

World.sin <- st_transform(World,"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World.sin) + tm_fill()  

World.mercator <- st_transform(World,"+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World.mercator) + tm_fill()  

### failed transformation ###
World.mercator2 <- st_transform(World, "+proj=merc +lon_0=-69 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

tm_shape(World.mercator2) + tm_borders()

# using map tools - not working for me
library(maptools)

# Convert to lat/long reference system
wld.ll <- st_transform(World, "+proj=longlat +datum=WGS84 +no_defs")

# Convert to a spatial object, then split the polygons at a given longitude (111 in this example)
wld.sp <- nowrapSpatialPolygons(as(wld.ll, "Spatial"), offset = 111)

# Now convert back to an sf object, reproject to a new longitude center at -69 degrees 
# then plot it
wld.sf <- st_as_sf(wld.sp)
wld.merc2.sf <- st_transform(wld.sf, "+proj=merc +lon_0=-69 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(wld.merc2.sf) + tm_borders()
# antartica polygon is deformed. either remove or use another projection eg using robinson.
wld.rob.sf <- st_transform(wld.sf,"+proj=robin +lon_0=-69 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(wld.rob.sf) + tm_borders()


# Alternative - create a centriod. Centriod is the mean position of all points in the figure. 
# Create centroids from polygons
pt <- st_centroid(World, of_largest_polygon = TRUE)

# Transform points to the recentered Robinson projection
pt.rob <- st_transform(pt,"+proj=robin +lon_0=-69 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Perform the spatial join (joining the point attribute values to the wld.rob.sf polygons)
wld.rob.df.sf <- st_join(wld.rob.sf, pt.rob, join = st_contains)

# Map the output
tm_shape(wld.rob.df.sf) + tm_polygons(col="pop_est_dens", style="quantile") +
    tm_legend(outside=TRUE)


## some polygons have missing values.
library(dplyr)

# Extract the extent for the Norwar/Sweden region
nor.bb <- World %>% filter(name == "Norway" | name == "Sweden") %>% st_bbox()

# Plot the data zoomed in on the region. Add the point layer for reference
tm_shape(World, bbox=nor.bb) + tm_polygons(col="pop_est_dens", style="quantile") +
    tm_shape(pt) + tm_dots() +
    tm_text("name", just="left", xmod=0.5, size=0.8) +
    tm_legend(outside=TRUE)

pt <- st_point_on_surface(World)

pt.rob <- st_transform(pt,"+proj=robin +lon_0=-69 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
wld.rob.df.sf <- st_join(wld.rob.sf, pt.rob, join = st_contains)
tm_shape(wld.rob.df.sf) + tm_polygons(col="pop_est_dens", style="quantile") +
    tm_legend(outside=TRUE)

### COntainment ###
#use function st_contains to check, use st_segmentize to add more vertices to contain the points.
#  if a point is inside of a polygon and very close to one of its boundaries in its native projection, reprojection may lead to points outside the polygon.

# Define a few projections
miller <- "+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
lambert <- "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Subset the World data layer
wld.mil <-  World %>% filter( iso_a3  == "CAN" |  iso_a3 == "USA") %>% st_transform( miller)

# Create polygon and point layers in the Miller projection  
sf1 <- st_sfc( st_polygon(list(cbind(c(-13340256,-13340256,-6661069, -6661069, -13340256),
                                     c(7713751, 5326023, 5326023,7713751, 7713751 )))), crs = miller) 

pt1 <- st_sfc( st_multipoint(rbind(c(-11688500,7633570), c(-11688500,5375780),
                                   c(-10018800,7633570), c(-10018800,5375780),
                                   c(-8348960,7633570), c(-8348960,5375780))),  crs = miller)
pt1 <- st_cast(pt1, "POINT") # Create single part points

# Plot the data layers in their native projection
tm_shape(wld.mil) +tm_fill(col="grey") + 
    tm_graticules(x = c(-60,-80,-100, -120, -140), 
                  y = c(30,45, 60), 
                  labels.col = "white", col="grey90") +
    tm_shape(sf1) + tm_polygons("red", alpha = 0.5, border.col = "yellow") +
    tm_shape(pt1) + tm_dots(size=0.2) 

# to confirm if points are inside the boundary
st_contains(sf1, pt1)

#replotted using Lambert projection
# Transform the data
wld.lam <- st_transform(wld.mil, lambert)
pt1.lam <- st_transform(pt1, lambert)
sf1.lam <- st_transform(sf1, lambert)

# Plot the data in the Lambert coordinate system
tm_shape(wld.lam) +tm_fill(col="grey") + 
    tm_graticules( x = c(-60,-80,-100, -120, -140), 
                   y = c(30,45, 60), 
                   labels.col = "white", col="grey90") +
    tm_shape(sf1.lam) + tm_polygons("red", alpha = 0.5, border.col = "yellow") +
    tm_shape(pt1.lam) + tm_dots(size=0.2)   

##in this projection, only 3 out of 6 points are contained.
st_contains(sf1.lam, pt1.lam)

# to solve this issue, should densify the polygon by adding more vertices along the line segment
# use the st_segmentize function. Add vertices every 1000 meters along the polygon's line segments
sf2 <- st_segmentize(sf1, 1000)

# Transform the newly densified polygon layer
sf2.lam <- st_transform(sf2, lambert)

# Plot the data
tm_shape(wld.lam) + tm_fill(col="grey") + 
    tm_graticules( x = c(-60,-80,-100, -120, -140), 
                   y = c(30,45, 60), 
                   labels.col = "white", col="grey90") +
    tm_shape(sf2.lam) + tm_polygons("red", alpha = 0.5, border.col = "yellow") +
    tm_shape(pt1.lam) + tm_dots(size=0.2) 
st_contains(sf2.lam, pt1.lam)

##### Creating Tissot indicatrix circles ####
# reduce distortion during proejction, create geodesic circles kind of like a buffer?
# calculate errors when converting between projections

#create point layer with circle centers.
tissot.pt <- st_sfc( st_multipoint(rbind(c(-60,30), c(-60,45), c(-60,60),
                                         c(-80,30), c(-80,45), c(-80,60),
                                         c(-100,30), c(-100,45), c(-100,60),
                                         c(-120,30), c(-120,45), c(-120,60) )),  crs = "+proj=longlat")
tissot.pt <- st_cast(tissot.pt, "POINT") # Create single part points

# construct geodesic circles
library(geosphere)

cr.pt <- list() # Create an empty list

# Loop through each point in tissot.pt and generate 360 vertices at 300 km
# from each point in all directions at 1 degree increment. These vertices
# will be used to approximate the Tissot circles
for (i in 1:length(tissot.pt)){
    cr.pt[[i]] <- list( destPoint( as(tissot.pt[i], "Spatial"), b=seq(0,360,1), d=300000) )
}

# Create a closed polygon from the previously generated vertices
tissot.sfc <- st_cast( st_sfc(st_multipolygon(cr.pt ),crs = "+proj=longlat"), "POLYGON" )

#check 
tissot.sf <- st_sf( geoArea =  st_area(tissot.sfc), tissot.sfc )

# compute error of tissot output.
( (pi * 300000^2) -  as.vector(tissot.sf$geoArea) ) / (pi * 300000^2)

#distortions with few coordinate systems

## mercator projection
# Transform geodesic circles and compute area error as a percentage
tissot.merc <- st_transform(tissot.sf, "+proj=merc +ellps=WGS84")
tissot.merc$area_err <- round((st_area(tissot.merc, tissot.merc$geoArea)) / 
                                  tissot.merc$geoArea * 100 , 2)

# Plot the map
tm_shape(World, bbox = st_bbox(tissot.merc), projection = st_crs(tissot.merc)) + 
    tm_borders() + 
    tm_shape(tissot.merc) + tm_polygons(col="grey", border.col = "red", alpha = 0.3) + 
    tm_graticules(x = c(-60,-80,-100, -120, -140), 
                  y = c(30,45, 60),
                  labels.col = "white", col="grey80") +
    tm_text("area_err", size=.8, alpha=0.8, col="blue")

## lambert projection
# Transform geodesic circles and compute area error as a percentage
tissot.laea <- st_transform(tissot.sf, "+proj=laea +lat_0=45 +lon_0=-100 +ellps=WGS84")
tissot.laea$area_err <- round( (st_area(tissot.laea ) - tissot.laea$geoArea) / 
                                   tissot.laea$geoArea * 100, 2)

# Plot the map
tm_shape(World, bbox = st_bbox(tissot.laea), projection = st_crs(tissot.laea)) + 
    tm_borders() + 
    tm_shape(tissot.laea) + tm_polygons(col="grey", border.col = "red", alpha = 0.3) + 
    tm_graticules(x=c(-60,-80,-100, -120, -140), 
                  y = c(30,45, 60),
                  labels.col = "white", col="grey80") +
    tm_text("area_err", size=.8, alpha=0.8, col="blue")

## Robinson projection
# Transform geodesic circles and compute area error as a percentage
tissot.robin <- st_transform(tissot.sf, "+proj=robin  +ellps=WGS84")
tissot.robin$area_err <- round(  (st_area(tissot.robin ) - tissot.robin$geoArea) / 
                                     tissot.robin$geoArea * 100, 2)

# Plot the map
tm_shape(World, bbox = st_bbox(tissot.robin), projection = st_crs(tissot.robin)) + 
    tm_borders() + 
    tm_shape(tissot.robin) + tm_polygons(col="grey", border.col = "red", alpha = 0.3) + 
    tm_graticules(x=c(-60,-80,-100, -120, -140), 
                  y = c(30,45, 60),
                  labels.col = "white", col="grey80") +
    tm_text("area_err", size=.8, alpha=0.8, col="blue")
