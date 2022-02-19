### Vector operations in R ####
# many vector operations can now be performed in sf, previously we used raster and rgeos

library(sf)

z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/Income_schooling_sf.rds"))
s1.sf <- readRDS(z)

z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/Dist_sf.rds"))
s2.sf <- readRDS(z)

z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/Highway_sf.rds"))
l1.sf <- readRDS(z)

# plot layers
library(ggplot2)

ggplot() + 
  geom_sf(data = s1.sf) +
  geom_sf(data = s2.sf, alpha = 0.5, col = "red") +
  geom_sf(data = l1.sf, col = "blue")

s1.sf
s2.sf
### Dissolving geometries ####
# dissolve geometries that share a common boundary

## Option 1: st_union
ME <- st_union(s1.sf, by_feature = FALSE)

ggplot(ME) + geom_sf(fill = "grey")

ME <- st_sf(ME)
# this removed all attributes from the original spatial object
# st_union returns an sfc object even though the input object is sf
# convert output to an sf object using st_sf() function

## Option 2: make use of dplyr - use this.
library(dplyr)

ME <- s1.sf %>% 
  group_by() %>% 
  summarise()
ME #only have the shape of the polygon left. remove all other attributes

ggplot(ME) + geom_sf(fill = "grey")
# this removes any attributes associated with the input spatial object 
# but the output remains an sf object

### Dissolving by attribute ####
# create a new column whose value will be binary (TRUE/FALSE) depending on
# whether or not the county income is below the counties' median income value
s1.sf$med <- s1.sf$Income > median(s1.sf$Income)

ggplot(s1.sf) + geom_sf(aes(fill = med))

## Option 1
ME.inc <- aggregate(s1.sf["med"], by = list(diss = s1.sf$med), 
                    FUN = function(x)x[1], do_union = TRUE)

st_drop_geometry(ME.inc) # Print the layer's attributes table

## Option 2
ME.inc <- s1.sf %>% 
  group_by(med) %>% 
  summarise() 

st_drop_geometry(ME.inc)

# plot
ggplot(ME.inc) + geom_sf(aes(fill = med))
# this will be default eliminate all other attribute values
# to summarize other attribute values along with the attribute used for dissolving,


# use dplyr piping operation option - e.g., compute median Income value for each group
ME.inc <- s1.sf %>%  
  group_by(med) %>%   
  summarize(medinc = median(Income)) 

ggplot(ME.inc) + geom_sf(aes(fill = medinc))

# view attribute table
st_drop_geometry(ME.inc)

### Subsetting by attribute ####

# option 1: conventional R dataframe manipulation
ME.ken <- s1.sf[s1.sf$NAME == "Kennebec",]

# option 2: use dplyr and piping operations  to filter for one county
ME.ken <- s1.sf %>%
  filter(NAME == "Kennebec")

# plot
ggplot(ME.ken) + geom_sf()

## subset by a range of attributes - subset by income values less than median values
ME.inc2 <- s1.sf %>%
  filter(Income < median(Income))

ggplot(ME.inc2) + geom_sf()

### Intersecting layers ####
# use sf's st_intersection function

clp1 <- st_intersection(s1.sf, s2.sf) # to get intersecting layers between two sf

ggplot(clp1) + geom_sf()

# st_intersection keeps all features that overlap along with their combined
# attributes. New polygons are created which will increase the size of the attribute table
# beynd the size of the combined input attributes table

st_drop_geometry(clp1)

### Clipping spatial objects using other spatial objects ####
# possible to clip using another layer's outer geometry boundaries
# this may require dissolving though

clp2 <- st_intersection(s2.sf, st_union(s1.sf))

ggplot(clp2) + geom_sf()

# the order in which layers are passed to the st_intersection function matters

clp2 <- st_intersection(s1.sf, st_union(s2.sf))

ggplot(clp2) + geom_sf()

# line geometries can also be clipped to polygon features
# output will be a line object that falls within the polygons of the input polygon object

clp3 <- st_intersection(l1.sf, st_union(s2.sf))

# plot
ggplot(clp3) + 
  geom_sf(data = clp3) +
  geom_sf(data = st_union(s2.sf), col = "red", fill = NA )

### Unioning layers ####
un1 <- st_union(s2.sf, s1.sf) #vs st_union on one layer that dissolve the geometry

ggplot(un1) + geom_sf(aes(fill = NAME), alpha = 0.4)
# this produces many overlapping geometries- all possible pairs of geometries.
un1 %>% filter(NAME == "Aroostook")

### Buffering geometries ####
# generate 10 km (10000 m) buffer around polyline segments
l1.sf.buf <- st_buffer(l1.sf, dist = 10000)

ggplot(l1.sf.buf) + geom_sf() + coord_sf(ndiscr = 1000) #looks messy

# to create a continuous polygon geometry, i.e., to eliminate overlapping buffers
# use dissolving techniques earlier in tutorial
l1.sf.buf.dis <- l1.sf.buf %>% 
  group_by()  %>% 
  summarise()

ggplot(l1.sf.buf.dis) + geom_sf() 

# to preserve attribute value such as highway number, modify above code
l1.sf.buf.dis <- l1.sf.buf %>% 
  group_by(Number)  %>% 
  summarise()

ggplot(l1.sf.buf.dis, aes(fill=Number) ) + geom_sf(alpha = 0.5)
