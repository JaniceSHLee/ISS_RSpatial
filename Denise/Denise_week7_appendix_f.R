#### Raster operations in R ####

library(raster)

# load files
load(url("https://github.com/mgimond/Spatial/raw/main/Data/raster.RData"))
# both rasters cover the entire globe!

### Local operations and functions ####
### Unary operations and functions (applied to single rasters) ###

# map algebraic operations
bath2 <- bath*(-1) # to convert positive values to neg values

# RECLASSIFICATION - helpful!! for looking at patterns
bath3 <- bath2 < 0 #all values less than 0 become 1

# plot raster
library(tmap)

tm_shape(bath3) + tm_raster(palette = "Greys") + 
  tm_legend(outside = TRUE, text.size = .8) 

# possible to do more elaborate form of reclassification using reclassify function

# create a plain matrix - 1st and 2nd columns list starting and ending values
# 3rd column lists the new raster cell values

m <- c(0, 100, 100,  100, 500, 500,  500, 
       1000,  1000, 1000, 11000, 11000)
m <- matrix(m, ncol=3, byrow = T)
m

bath3 <- reclassify(bath, m, right = T) 
# right = T parameter indicates the inervals should be closed to the right

tm_shape(bath3) + 
  tm_raster(style="cat") + 
  tm_legend(outside = TRUE, text.size = .8) 

# assign NA values to pixels
bath3[bath3 == 100] <- NA

# highlights all NA in grey and labels them as missing
tm_shape(bath3) + 
  tm_raster(showNA=TRUE, colorNA="grey") + 
  tm_legend(outside = TRUE, text.size = .8) 

### Binary operations and functions (where two rasters are used) ###
# add elev and bath together
elevation <- elev - bath

tm_shape(elevation) + 
  tm_raster(palette="-RdBu",n=6) + # negative sign inverses color palette
  tm_legend(outside = TRUE, text.size = .8) 

### Focal operations and functions ####
#smoothing function
## these involve user defined neighboring cells
f1 <- focal(elevation, w=matrix(1,nrow=11,ncol=11), fun = mean) # use an 11x11 kernel

# map
tm_shape(f1) + tm_raster(palette="-RdBu",n=6) + 
  tm_legend(outside = TRUE, text.size = .8) 

# by default cells outside of input raster extent have no value and causes 
# edge cells to be assigned a value of NA
# to remedy this, use the following parameters:  pad=TRUE,  na.rm = TRUE. Ignores cells outside the extent
f1 <- focal(elevation, w=matrix(1,nrow=11,ncol=11)  , 
            fun=mean, pad=TRUE,  na.rm = TRUE)

tm_shape(f1) + tm_raster(palette="-RdBu",n=6) + 
  tm_legend(outside = TRUE, text.size = .8) 

# be careful of using this as it varies in output depending on smoothing operation
# especially with weighted means
# Using the mean function
f_mean     <- focal(elevation, w=matrix(1,nrow=3,ncol=3), fun=mean, na.rm=TRUE, pad=TRUE)

# Using explicitly defined weights
f_wt_nopad <- focal(elevation, w=matrix(1/9,nrow=3,ncol=3), na.rm=TRUE, pad=FALSE)
f_wt_pad   <- focal(elevation, w=matrix(1/9,nrow=3,ncol=3), na.rm=TRUE, pad=TRUE)

# this is only an issue with top and bottom of raster extent
# left and right extents not affected as focal function will wrap eastern and western edge

# neighbors matrix or kernel that defines the moving window can be customized

# compute the average of all 8 neighboring cells excluding central cell
m  <- matrix(c(1,1,1,1,0,1,1,1,1)/8,nrow = 3) 
f2 <- focal(elevation, w=m, fun=sum)

# sobel filter used for edge detection in image processing
Sobel <- matrix(c(-1,0,1,-2,0,2,-1,0,1) / 4, nrow=3) 
f3    <- focal(elevation, w=Sobel, fun=sum) 

tm_shape(f3) + tm_raster(palette="Greys") + 
  tm_legend(legend.show = FALSE) 

### Zonal operations and functions ####

# aggregation of cells
z1 <- aggregate(elevation, fact=2, fun=mean, expand=TRUE)

tm_shape(z1) + tm_raster(palette="-RdBu",n=6) + 
  tm_legend(outside = TRUE, text.size = .8) 

res(elevation)
res(z1)

# able to reverse this process by using disaggregate function
# splits the cell into a desired number of subcells while assigning each one
# the same parent value

# elevation values averaged by zones defined by cont polygon layer
cont.elev.sp <- extract(elevation, cont, fun=mean, sp=TRUE) 
# sp=TRUE represent output to a SpatialPolygonsDataFrame
# output of this function is a polygon
# if sp=FALSE, becomes a matrix/array

# convert SpatialPolygonsDataFrame to sf
library(sf)
cont.elev <- st_as_sf(cont.elev.sp)

# average elevation can be extracted from sf object
st_drop_geometry(cont.elev)

# map
tm_shape(cont.elev) + tm_polygons(col="layer") + 
  tm_legend(outside = TRUE, text.size = .8)

# many custom functions can be applied to extract
# extract maximum elevation value by continent

cont.elev.sp <- extract(elevation, cont, fun=max, sp=TRUE)

# use customized function
cont.elev.sp <- extract(elevation, cont, fun=function(x,...){length(x)}, sp=TRUE) 

# extract function also works with lines and point spatial objects
# compute zonal statistics using another raster as zones - use zonal() function instead

### Global operations and functions ####
# Global operations and functions make use of all input cells of a grid to 
# compute an output cell value

# Euclidean distance function, distance, computes shortest distance between 
# pixel and a source

r1   <- raster(ncol=100, nrow=100, xmn=0, xmx=100, ymn=0, ymx=100)
r1[] <- NA              # Assign NoData values to all pixels
r1[c(850, 5650)] <- 1   # Change the pixels #850 and #5650  to 1
crs(r1) <- "+proj=ortho"  # Assign an arbitrary coordinate system (needed for mapping with tmap)


tm_shape(r1) + tm_raster(palette="red") + 
  tm_legend(outside = TRUE, text.size = .8) 

# compute Euclidean distance raster from these two cells
r1.d <- distance(r1)

# plot
tm_shape(r1.d) + tm_raster(palette = "Greens", style="order", title="Distance") + 
  tm_legend(outside = TRUE, text.size = .8) +
  tm_shape(r1) + tm_raster(palette="red", title="Points") 

# compute distance using SpatialPoints objects or a simple x, y data table
# Create a blank raster
r2 <- raster(ncol=100, nrow=100, xmn=0, xmx=100, ymn=0, ymx=100)
crs(r2) <- "+proj=ortho"  # Assign an arbitrary coordinate system 

# Create a point layer
xy <- matrix(c(25,30,87,80),nrow=2, byrow=T) 
p1 <- SpatialPoints(xy)
crs(p1) <- "+proj=ortho"  # Assign an arbitrary coordinate system 

# compute Euclidean distance using distanceFromPoints function
r2.d <- distanceFromPoints(r2, p1)

# plot
tm_shape(r2.d) + tm_raster(palette = "Greens", style="order") + 
  tm_legend(outside = TRUE, text.size = .8) +
  tm_shape(p1) + tm_bubbles(col="red") 

# note: these still require plotting from one cell to a point in raster layer
# what about computing distance from edge?

### Computing cumulative distances ####
library(gdistance)

# create a raster where 1 is the cost in traversing that pixel, cost is uniform 
# in this layer
r   <- raster(nrows=100,ncols=100,xmn=0,ymn=0,xmx=100,ymx=100)
r[] <- rep(1, ncell(r))

# function(x){1}, to the translation between a cell and its adjacent cells 
# (i.e. translation cost is uniform in all directions)
# adjacency can be defined as a 4-node connection
h4   <- transition(r, transitionFunction = function(x){1}, directions = 4)

# adjacency defined as a 8-node connection
h8   <- transition(r, transitionFunction = function(x){1}, directions = 8)

# adjacency can be defined as a 16-node connection
h16  <- transition(r, transitionFunction=function(x){1},16,symm=FALSE)

# adjacency can be defined as a 4-node diagonal connection
hb   <- transition(r, transitionFunction=function(x){1},"bishop",symm=FALSE)

# geoCorrection corrects for 'true' local distance
# it adds an additional cost to traversing from one cell to an adjacent cell
# define the raster layer's coordinate system first

h4    <- geoCorrection(h4,  scl=FALSE)
h8    <- geoCorrection(h8,  scl=FALSE)
h16   <- geoCorrection(h16, scl=FALSE)
hb    <- geoCorrection(hb,  scl=FALSE)

# map the cumulative distance )accCost) from a central point A to all cells in raster
A       <- c(50,50) # Location of source cell
h4.acc  <- accCost(h4,A)
h8.acc  <- accCost(h8,A)
h16.acc <- accCost(h16,A)
hb.acc  <- accCost(hb,A) 

# for 'bishop', cells in diag direction identified but leaves many undefined cells labeled
# Inf. change Inf to NA
hb.acc[hb.acc == Inf] <- NA

# working example