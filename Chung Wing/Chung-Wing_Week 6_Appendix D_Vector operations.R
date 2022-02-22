#using sf for vector operations

#loading spatial objects
library(sf)
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/Income_schooling_sf.rds"))
s1.sf <- readRDS(z) #note that rds saves space when sharing data
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/Dist_sf.rds"))
s2.sf <- readRDS(z)
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/Highway_sf.rds"))
l1.sf <- readRDS(z)

#mapping layers using ggplot2
library(ggplot2)

ggplot() + 
  geom_sf(data = s1.sf) +
  geom_sf(data = s2.sf, alpha = 0.5, col = "red") +
  geom_sf(data = l1.sf, col = "blue")

#dissolve geometries that share boundaries

#can dissolve all polygons that share line segment for single outline
#option 1
ME <- st_union(s1.sf, by_feature = FALSE) #using this union function
ggplot(ME) + geom_sf(fill = "grey")
#can convert output (sfc) to sf object using st_sf(ME)
#option 2
library(dplyr)
ME <- s1.sf %>% 
  group_by() %>% 
  summarise()
ggplot(ME) + geom_sf(fill = "grey")
#here, the output remains an sf object

#dissolve by attribute
#create new column for whether income is higher/lower than median
s1.sf$med <- s1.sf$Income > median(s1.sf$Income)
#and plot based on variable
ggplot(s1.sf) + geom_sf(aes(fill = med))
#now dissolve based on attribute
#option 1 with sf aggregate function
ME.inc <- aggregate(s1.sf["med"], by = list(diss = s1.sf$med), 
                    FUN = function(x)x[1], do_union = TRUE)
#by = defines new field
st_drop_geometry(ME.inc) # print the layer's attributes table
#option 2 with dplyr approach
ME.inc <- s1.sf %>% 
  group_by(med) %>% 
  summarise() #limits attributes to those listed in group_by()
st_drop_geometry(ME.inc)
ggplot(ME.inc) + geom_sf(aes(fill = med)) #dissolving
#to summarize other attribute values (rather than eliminating)
ME.inc <- s1.sf %>%  
  group_by(med) %>%   
  summarize(medinc = median(Income)) 
ggplot(ME.inc) + geom_sf(aes(fill = medinc))
#view attributes table
st_drop_geometry(ME.inc) #shows values rather than binary

#subset by attribute
ME.ken <- s1.sf[s1.sf$NAME == "Kennebec",] #subset by county name
ME.ken <- s1.sf %>% 
  filter(NAME == "Kennebec") #can also pipe
ggplot(ME.ken) + geom_sf()

#subset by range of attribute values
ME.inc2 <- s1.sf %>% 
  filter(Income < median(Income)) #subset of income less than median
ggplot(ME.inc2) + geom_sf()

#intersect two polygon objects
clp1 <- st_intersection(s1.sf, s2.sf)
ggplot(clp1) + geom_sf()
#creates new polygons so increases size of attribute table
st_drop_geometry(clp1)

#clip spatial objects using other spatial objects
##note: outer geometry must be limited to outer boundary (might need to dissolve first)
clp2 <- st_intersection(s2.sf, st_union(s1.sf)) #clip using outline of another
ggplot(clp2) + geom_sf() #plot
##note that the order of layers matters
clp2 <- st_intersection(s1.sf, st_union(s2.sf)) #union() layer is the outline
ggplot(clp2) + geom_sf()
#can also clip line segments to fit inside polygon
clp3 <- st_intersection(l1.sf, st_union(s2.sf))
ggplot(clp3) + 
  geom_sf(data = clp3) +
  geom_sf(data = st_union(s2.sf), col = "red", fill = NA)

#unioning two polygon objects
un1  <- st_union(s2.sf,s1.sf)
ggplot(un1) + geom_sf(aes(fill = NAME), alpha = 0.4)
##note: union generates overlapping geometries
##looking at example
un1 %>% filter(NAME == "Aroostook")

#buffering geometries (wider space around geometries?)
l1.sf.buf <- st_buffer(l1.sf, dist = 10000) #10000 m buffer around polyline segments
ggplot(l1.sf.buf) + geom_sf() + coord_sf(ndiscr = 1000)
#eliminate overlapping buffers for a continuous geometry
l1.sf.buf.dis <- l1.sf.buf %>% 
  group_by()  %>% 
  summarise()
ggplot(l1.sf.buf.dis) + geom_sf() 
#preserve an attribute value
l1.sf.buf.dis <- l1.sf.buf %>% 
  group_by(Number)  %>% 
  summarise()
ggplot(l1.sf.buf.dis, aes(fill=Number) ) + geom_sf(alpha = 0.5) #using fill function












