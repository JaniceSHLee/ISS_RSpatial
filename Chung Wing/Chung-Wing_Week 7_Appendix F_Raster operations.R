#loading spatial objects from remote website
load(url("https://github.com/mgimond/Spatial/raw/main/Data/raster.RData"))

#load library
library(raster)

#algebraic operations same as vectors
bath2 <- bath * (-1)
#conditional statements (ex: reclassification)
bath3 <- bath2 < 0
#plot results
library(tmap)
tm_shape(bath3) + tm_raster(palette = "Greys") + 
  tm_legend(outside = TRUE, text.size = .8) 
#or reclassify function for more elaborate reclassification
m <- c(0, 100, 100,  100, 500, 500,  500, 
       1000,  1000, 1000, 11000, 11000) #create plain matrix of interval and new classification
#use overlapping values and right = T to be inclusive to the right
m <- matrix(m, ncol=3, byrow = T)
m
bath3 <- reclassify(bath, m, right = T) #intervals are closed to the right (inclusive)
#plot results
tm_shape(bath3) + tm_raster(style="cat") + tm_legend(outside = TRUE, text.size = .8) 
#assigning NA values
bath3[bath3 == 100] <- NA
#and plot
tm_shape(bath3) + tm_raster(showNA=TRUE, colorNA="grey") + 
  tm_legend(outside = TRUE, text.size = .8) 

#binary operations (using two rasters)
#adding two rasters together for a single global elevation raster
elevation <- elev - bath
#plot
tm_shape(elevation) + tm_raster(palette="-RdBu",n=6) + 
  tm_legend(outside = TRUE, text.size = .8) #negative sign inverts color palette

#focal operations involve user defined neighboring cells
#like smoothing functions
f1 <- focal(elevation, w=matrix(1,nrow=11,ncol=11)  , fun=mean) #averaging cells around
#plot
tm_shape(f1) + tm_raster(palette="-RdBu",n=6) + 
  tm_legend(outside = TRUE, text.size = .8) 
#edge cells by default are assigned NA
f1 <- focal(elevation, w=matrix(1,nrow=11,ncol=11)  , 
            fun=mean, pad=TRUE,  na.rm = TRUE) #remedy with na.rm=TRUE and pad=TRUE
#plot
tm_shape(f1) + tm_raster(palette="-RdBu",n=6) + 
  tm_legend(outside = TRUE, text.size = .8)
#these fixes are not always right
#used mean function, so will sort weights based on # of input values
#if using explicitly defined weights, pad=TRUE will be unbalanced (problematic!)
#for example, compare these three:
# using the mean function
f_mean     <- focal(elevation, w=matrix(1,nrow=3,ncol=3), fun=mean, na.rm=TRUE, pad=TRUE)
# using explicitly defined weights
f_wt_nopad <- focal(elevation, w=matrix(1/9,nrow=3,ncol=3), na.rm=TRUE, pad=FALSE) #blank edges for NA values
f_wt_pad   <- focal(elevation, w=matrix(1/9,nrow=3,ncol=3), na.rm=TRUE, pad=TRUE) #has edge effect issues 
#focal function wraps raster from east to west (if layer spans globe) so no edge effects on sides
#neighbor's matrix (kernel) can be explicitly defined
#for example, excluding the center cell
m  <- matrix(c(1,1,1,1,0,1,1,1,1)/8,nrow = 3) 
f2 <- focal(elevation, w=m, fun=sum)
#more complicated kernels
Sobel <- matrix(c(-1,0,1,-2,0,2,-1,0,1) / 4, nrow=3) 
f3    <- focal(elevation, w=Sobel, fun=sum) 
#plot
tm_shape(f3) + tm_raster(palette="Greys") + 
  tm_legend(legend.show = FALSE) 

#zonal operations (ex: aggregation of cells) - summary values of zones
#creates new raster layer with lower resolution (larger cells)
z1 <- aggregate(elevation, fact=2, fun=mean, expand=TRUE) #fact=2 expresses # cells in each direction 
## fact = 2 leads to 4x fewer cells (can also specify two numbers, which will aggregate in horizontal and vertical respectively)
#plot
tm_shape(z1) + tm_raster(palette="-RdBu",n=6) + 
  tm_legend(outside = TRUE, text.size = .8) 
#looking at pixel sizes
res(elevation)
res(z1) #pixels are half the dimension of elevation layer
#can also use disaggregate function to revert to parent cell resolution

#zonal operations can use two layers (one for zones, one for values)
#computes mean elevation for cont layer polygons
cont.elev.sp <- extract(elevation, cont, fun=mean, sp=TRUE) 
## sp=TRUE parameter says to output as SpatialPolygonsDataFrame object rather than matrix
## this is the same as the input zonal object (continents layer)
#convert object to sf vector object for flexibility
library(sf)
cont.elev <- st_as_sf(cont.elev.sp)
#extract average elevation
st_drop_geometry(cont.elev)
#if using original SpatialPolygonsDataFrame, use cont.elev.sp@data to extract dataframe
#map avg elevation by continent
tm_shape(cont.elev) + tm_polygons(col="layer") + 
  tm_legend(outside = TRUE, text.size = .8)
#many more functions
##extract max elevation by continent
cont.elev.sp <- extract(elevation, cont, fun=max, sp=TRUE) 
cont.elev.max <- st_as_sf(cont.elev.sp)
st_drop_geometry(cont.elev.max)
#extract # pixels per polygon with customised function
cont.elev.sp <- extract(elevation, cont, fun=function(x,...){length(x)}, sp=TRUE) 
#use zonal() function instead if you want zonal statistics of raster as zones rather than vector layer
##like zonal(x, z) where z contains the zones, both layers are raster layers
##rasterizing a shapefile modifies boundaries of sf - may not want to do this for small areas
##so for small boundaries or many sf files, keep to extract function
##if your two layers already in raster, can use zonal function

#global operations using all input cells
#ex: calculating euclidean distance to a certain destination
r1   <- raster(ncol=100, nrow=100, xmn=0, xmx=100, ymn=0, ymx=100) #creating new raster layer
r1[] <- NA              # assign NoData values to all pixels
r1[c(850, 5650)] <- 1   # change the pixels #850 and #5650  to 1 (creating two points)
crs(r1) <- "+proj=ortho"  # assign an arbitrary coordinate system (needed for mapping with tmap)
#plot
tm_shape(r1) + tm_raster(palette="red") + 
  tm_legend(outside = TRUE, text.size = .8) 
#computing distance from these two points
r1.d <- distance(r1) #assigns distance from NA points to nearest point that's not NA
#plot output (output extent is input extent by default)
tm_shape(r1.d) + tm_raster(palette = "Greens", style="order", title="Distance") + #maps green gradient
  tm_legend(outside = TRUE, text.size = .8) + #legend
  tm_shape(r1) + tm_raster(palette="red", title="Points") #maps points in same layer

#can also compute distance from point objects rather than raster
#create a blank raster (to define extent of distance output)
r2 <- raster(ncol=100, nrow=100, xmn=0, xmx=100, ymn=0, ymx=100)
crs(r2) <- "+proj=ortho"  #assign an arbitrary coordinate system 
#create a point layer
xy <- matrix(c(25,30,87,80),nrow=2, byrow=T) 
p1 <- SpatialPoints(xy)
crs(p1) <- "+proj=ortho"  # Assign an arbitrary coordinate system 
#compute distance
r2.d <- distanceFromPoints(r2, p1) #function calculates distance from points to all cells in raster
#plot output
tm_shape(r2.d) + tm_raster(palette = "Greens", style="order") + 
  tm_legend(outside = TRUE, text.size = .8) +
  tm_shape(p1) + tm_bubbles(col="red") 

#computing cumulative distances (gdistance package)
##can demonstrate impact of adjacent cells in final result
library(gdistance)
#create raster and assign value of 1 to all cells (ex: traversing cost)
r   <- raster(nrows=100,ncols=100,xmn=0,ymn=0,xmx=100,ymx=100)
r[] <- rep(1, ncell(r)) #translation matrix defines traversing cost from one cell to next
#adjacency can be defined in 4 ways
# 1. four nodes (vertical + horizontal directions - rook move)
h4   <- transition(r, transitionFunction = function(x){1}, directions = 4)
# 2. eight nodes (vertical, horizontal, diagonal - queen move)
h8   <- transition(r, transitionFunction = function(x){1}, directions = 8)
# 3. sixteen nodes (queen + knight move)
h16  <- transition(r, transitionFunction=function(x){1},16,symm=FALSE)
# 4. four nodes (diagonal only - bishop move)
hb   <- transition(r, transitionFunction=function(x){1},"bishop",symm=FALSE)
##transition function treats all adjacent cells as equidistant from source cell
##can correct for true distance with geoCorrection
##geoCorrection also corrects for distance distortions associated with geographic coordinate system
###just make sure to define raster's crs with projection function
#correcting distances
h4    <- geoCorrection(h4,  scl=FALSE)
h8    <- geoCorrection(h8,  scl=FALSE)
h16   <- geoCorrection(h16, scl=FALSE)
hb    <- geoCorrection(hb,  scl=FALSE)
#so for example, diagonal cells now have higher cost than vertical cells
#map cumulative distances from A to all raster cells (using 4 diff adjacency definitions)
A       <- c(50,50) # Location of source cell
h4.acc  <- accCost(h4,A)
h8.acc  <- accCost(h8,A)
h16.acc <- accCost(h16,A)
hb.acc  <- accCost(hb,A)
#bishop case leaves many undefined cells (convert to NA)
hb.acc[hb.acc == Inf] <- NA
#plot
tm_shape(h4.acc) + tm_raster()
tm_shape(h8.acc) + tm_raster()
tm_shape(h16.acc) + tm_raster()
tm_shape(hb.acc) + tm_raster()

#example: raster layer with barriers - 
##ID all cells within 290 km distance from upper left corner
##compare results of 8-node and 16-node adjacency definition
# create an empty raster
r   <- raster(nrows=300,ncols=150,xmn=0,ymn=0,xmx=150000, ymx=300000)
# define a UTM projection (this sets map units to meters)
projection(r) = "+proj=utm +zone=19 +datum=NAD83" 
# each cell is assigned a value of 1
r[] <- rep(1, ncell(r)) 
# Generate 'baffles' by assigning NA to cells. Cells are identified by their index and not their coordinates.
# Baffles need to be 2 cells thick to prevent the 16-node case from "jumping" a one pixel thick NA cell.
a <- c(seq(3001,3100,1),seq(3151,3250,1))
a <- c(a, a+6000, a+12000, a+18000, a+24000, a+30000, a+36000)
a <- c(a , a+3050)
r[a] <- NA

# Let's check that the baffles are properly placed
tm_shape(r) + tm_raster(colorNA="red") + 
  tm_legend(legend.show=FALSE)

# Next, generate a transition matrix for the 8-node case and the 16-node case
h8   <- transition(r, transitionFunction = function(x){1}, directions = 8)
h16  <- transition(r, transitionFunction = function(x){1}, directions = 16)

# Now assign distance cost to the matrices. 
h8   <- geoCorrection(h8)
h16  <- geoCorrection(h16)

# Define a point source and assign a projection
A <- SpatialPoints(cbind(50,290000))
crs(A) <- "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"  

# Compute the cumulative cost raster
h8.acc   <- accCost(h8, A)
h16.acc  <- accCost(h16,A)

# Replace Inf with NA
h8.acc[h8.acc   == Inf] <- NA
h16.acc[h16.acc == Inf] <- NA

#plot results (yellow shows within 290km distance)
tm_shape(h8.acc) + tm_raster(n=2, style="fixed", breaks=c(0,290000,Inf)) +
  tm_facets() + tm_shape(A) + tm_bubbles(col="green", size = .5) + 
  tm_legend(outside = TRUE, text.size = .8)

tm_shape(h16.acc) + tm_raster(n=2, style="fixed", breaks=c(0,290000,Inf)) + 
  tm_facets() + tm_shape(A) + tm_bubbles(col="green", size = .5) + 
  tm_legend(outside = TRUE, text.size = .8)

#compute difference between diff adjacency definitions
table(h8.acc[]  <= 290000) 
table(h16.acc[] <= 290000)
#8-node definition has 10742 cells within 290km
#16-node definition has 11358 cells within 290km
#difference of 5.4%




