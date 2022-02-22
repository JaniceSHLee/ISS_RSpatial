##### Mapping data in R #####

# download files
library(sf)

load(url("https://github.com/mgimond/Spatial/raw/main/Data/Sample1.RData"))

### use ggplot ####
library(ggplot2)

ggplot(data=s.sf) + geom_sf()
ggplot() + geom_sf(data=s.sf) #same thing 
# more helpful this way as multiple plots added to map

# remove graticule (there by default)
ggplot(data=s.sf) + geom_sf() + theme_void()

# adopt native crs
ggplot(data=s.sf) + geom_sf() + coord_sf(datum = NULL)
# this allows you to prevent ggplot from figuring out how to 
# convert the layer's native crs to a geographic one

ggplot(data = s.sf) + geom_sf() +
  scale_x_continuous(breaks = c(-70, -69, -68)) +
  scale_y_continuous(breaks = 44:47)

ggplot(data = s.sf) + geom_sf() + 
  coord_sf(datum = NULL) +
  scale_x_continuous(breaks = c(400000, 500000, 600000)) +
  scale_y_continuous(breaks = c(4900000, 5100000))

# symbolize geometries
ggplot(data = s.sf, aes(fill = Income)) + geom_sf()

ggplot(data = s.sf, aes(fill = Income)) + 
  geom_sf(col = "white") 

ggplot(data = s.sf, aes(fill = Income)) + 
  geom_sf(col = NA) 

# tweaking classification schemes
# binning color scheme by assigning ranges of income values
ggplot(data = s.sf, aes(fill = Income)) + geom_sf() +
  scale_fill_stepsn(colors = c("#D73027", "#FC8D59", "#FEE08B", 
                               "#D9EF8B", "#91CF60") ,
                    breaks = c(22000, 25000, 27000, 30000))

ggplot(data = s.sf, aes(fill = Income)) + geom_sf() +
  scale_fill_fermenter(type = "div", palette = "PRGn", n.breaks = 4)

ggplot(data = s.sf, aes(fill = Income)) + geom_sf() +
  scale_fill_fermenter(type = "div", palette = "PRGn", n.breaks = 4, direction = 1)

ggplot(data = s.sf, aes(fill = Income)) + geom_sf() +
  scale_fill_stepsn(colors = c("#D73027", "#FC8D59", "#FEE08B", 
                               "#D9EF8B", "#91CF60", "#1A9850") ,
                    breaks = c(22000, 25000, 26000, 27000, 30000),
                    values = scales::rescale(c(22000, 25000, 26000, 27000, 30000), c(0,1)),
                    guide = guide_coloursteps(even.steps = FALSE,
                                              show.limits = TRUE,
                                              title = "Per capita Income \n(US dollars)",
                                              barheight = unit(2.2, "in"),
                                              barwidth = unit(0.15, "in"))) 

# combining layers
ggplot() + 
  geom_sf(data = s.sf, aes(fill = Income)) +
  geom_sf(data = rail.sf, col = "white") +
  geom_sf(data = p.sf, col = "green")
# ggplot will convert coordinate systems on the fly as needed

# adding rasters (must be in dataframe format)
library(raster) # Used to coerce raster to dataframe

ggplot() + 
  geom_raster(data = as.data.frame(elev.r, xy=TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = elev)) +
  scale_fill_gradientn(colours = terrain.colors(7)) +
  geom_sf(data = rail.sf, col = "white") +
  geom_sf(data = p.sf, col = "black") +
  theme(axis.title = element_blank())  # Removes axes labels
# raster layer must be in a dataframe format with x, y, and z columns
# add parameters xy=TRUE that instructs the function to create x and y 
# coordinate columns from the data, and na.rm = TRUE removes blank cells

## very basics with sf package (like base functions)

#simple plot function will plot all attributes
plot(s.sf)
#to specify one plot
plot(s.sf["Income"])
#to limit to geometric outlines
plot(st_geometry(s.sf))
#control fill and borders
plot(st_geometry(s.sf), col ="grey", border = "white")
#add graticule and labels
plot(st_geometry(s.sf), col ="grey", border = "white", graticule = TRUE, axes= TRUE)
#combining layers
plot(st_geometry(s.sf), col ="grey", border = "white", graticule = TRUE, axes= TRUE)
plot(rail.sf,  col = "grey20", add = TRUE)
##note: these require same coordinate system
#combine sf layer with raster layer
plot(elev.r, col = terrain.colors(30))
plot(st_geometry(rail.sf), col ="grey", border = "white", add = TRUE)
# map raster layer first!
#tweak colors
OP <- par(las = 1, omi=c(0,0,0,0.6))
p1 <- plot(s.sf["Income"], breaks = c(20000, 22000, 25000, 26000, 27000, 30000, 33000),
           pal = c("#D73027", "#FC8D59", "#FEE08B", 
                   "#D9EF8B", "#91CF60", "#1A9850"),
           key.width = 0.2,
           at = c(20000, 22000, 25000, 26000, 27000, 30000, 33000))
par(OP)

### use tmap ####
# tmap specifically developed for mapping spatial data 
# and offers greatest mapping options

library(sf)

load(url("https://github.com/mgimond/Spatial/raw/main/Data/Sample1.RData"))

library(tmap)
tm_shape(s.sf) + tm_polygons(col="grey", border.col="white")
# tmap maps shapefiles too
# tm_shape() loads the spatial object
# tm_polygons() dictates how spatial object is to be mapped

tm_shape(s.sf) + tm_polygons(col="Income", border.col="white")
# similar to ggplot syntax

# move legend outside main map
tm_shape(s.sf) + tm_polygons("Income", border.col="white") +
  tm_legend(outside=TRUE)

# choose to omit legend box and remove frame
tm_shape(s.sf) +
  tm_polygons("Income", border.col="white", legend.show=FALSE) +
  tm_layout(frame=FALSE)

# omit polygon borderline
tm_shape(s.sf) + 
  tm_polygons("Income", border.col = NULL) + 
  tm_legend(outside = TRUE)
# tm_fill almost the same as tm_polygons except that it does not draw 
# polygon borders

### Combining layers ###
tm_shape(s.sf) + 
  tm_polygons("Income", border.col = NULL) + 
  tm_legend(outside = TRUE) +
  tm_shape(rail.sf) + tm_lines(col="grey70") +
  tm_shape(p.sf) + tm_dots(size=0.3, col="black") 
# layers are stacked in the order in which they are listed
# note: tmap will reproject on the fly any layer whose coordinate system
# does not match the first layer in the stack - WOWOW!

### Tweaking classification schemes ###
# other style classification schemes : fixed, equal, jenks, kmeans, sd
tm_shape(s.sf) + 
  tm_polygons("Income", style = "quantile", n = 6, palette = "Greens") + 
  tm_legend(outside = TRUE)

# set breaks using style=fixed
tm_shape(s.sf) + 
  tm_polygons("Income", style = "fixed",palette = "Greens",
              breaks = c(0, 23000, 27000, 100000 )) + 
  tm_legend(outside = TRUE)

# control legend parameters
tm_shape(s.sf) + 
  tm_polygons("Income", style = "fixed",palette = "Greens",
              breaks = c(0, 23000, 27000, 100000 ),
              labels = c("under $23,000", "$23,000 to $27,000", "above $27,000"),
              text.size = 1) + 
  tm_legend(outside = TRUE)

### Tweaking colors ###

# map county names
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1") + 
  tm_legend(outside = TRUE)

# map % population
tm_shape(s.sf) + 
  tm_polygons("NoSchool", style="quantile", palette = "YlOrBr", n=8, 
              title="Fraction without \na HS degree") + 
  tm_legend(outside = TRUE)
# tm_shape function as where you place object
# tm_polygon function as specifying how you represent object

### Adding labels ###
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1", border.col = "white") + 
  tm_legend(outside = TRUE) +
  tm_shape(p.sf) +   # add another layer
  tm_dots(size=  .3, col = "red") +
  tm_text("Name", just = "left", xmod = 0.5, size = 0.8)
# tm_text accepts an auto placement option via parameter auto.placement = TRUE
## ensures a lot of text doesn't overlap - keep in mind may shift between runs

### Adding grid or graticule ###
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1") + 
  tm_legend(outside = TRUE) +
  tm_layout(outer.margins = c(.1,.1,.1,.1)) +
  tm_grid(labels.inside.frame = FALSE,
          n.x = 4, n.y = 5)
# changing grid's coordinate system to geographic one using EPSG defined coordinate system

# try not to use PROJ, adopt EPSG or OGC code
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1") + 
  tm_legend(outside = TRUE) +
  tm_layout(outer.margins = c(.1,.1,.1,.1)) +
  tm_grid(labels.inside.frame = FALSE, 
          x = c(-70.5, -69, -67.5),
          y = c(44, 45, 46, 47),
          projection = "EPSG:4326")
# can also specifically specify lat/long grid values

# degree to lat/long
tm_shape(s.sf) + 
  tm_polygons("NAME", palette = "Pastel1") + 
  tm_legend(outside = TRUE) +
  tm_layout(outer.margins = c(.1,.1,.1,.1)) +
  tm_grid(labels.inside.frame = FALSE, 
          x =  c(-70.5, -69, -67.5) ,
          y = c(44, 45, 46, 47),
          projection = "+proj=longlat",
          labels.format = list(fun=function(x) {paste0(x,intToUtf8(176))} ) ) #degree symbol
# use unicode decimal representation of the deg symbol
# other symbols: https://en.wikipedia.org/wiki/List_of_Unicode_characters#Latin-1_Supplement

### Adding statistical plots ###
tm_shape(s.sf) + 
  tm_polygons("NoSchool", palette = "YlOrBr", n = 6, 
              legend.hist = TRUE, title = "% no school") + 
  tm_legend(outside = TRUE, hist.width = 2) 
# this is very helpful - adding legend of histogram

### Mapping raster files ###
tm_shape(elev.r) + 
  tm_raster(style = "cont", title = "Elevation (m)",
            palette = terrain.colors(64))+
  tm_legend(outside = TRUE)
# cont for continuous color scheme
# now tm_raster to specify representation rather than tm_polygon

# choose classification breaks for raster
tm_shape(elev.r) + 
  tm_raster(style = "fixed", title = "Elevation (m)",
            breaks = c(0, 50, 100, 500, 750, 1000, 15000),
            palette = terrain.colors(5))+
  tm_legend(outside = TRUE)

# create own color scheme
tm_shape(elev.r) + 
  tm_raster(style = "quantile", n = 12, title = "Elevation (m)",
            palette = colorRampPalette( c("darkolivegreen4","yellow", "brown"))(12),
            legend.hist = TRUE)+
  tm_legend(outside = TRUE, hist.width = 2)

### Changing coordinate system ###
# Define the Albers coordinate system
aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"

# Map the data
tm_shape(elev.r, projection = aea) + # input coordinate system here
  tm_raster(style = "quantile", n = 12,
            palette = colorRampPalette( c("darkolivegreen4","yellow", "brown"))(12),
            legend.show = FALSE) +
  tm_shape(rail.sf) + tm_lines(col = "grey70")+
  tm_shape(p.sf) +tm_dots(size=0.5) + #note these didn't specify projection
  tm_layout(outer.margins = c(.1,.1,.1,.1)) +
  tm_grid(labels.inside.frame = FALSE, 
          x = c(-70.5, -69, -67.5),
          y = c(44, 45, 46, 47),
          projection = "+proj=longlat")
# so will reproject all data to base layer (here, specified aea)
## will default of first layer's reference system if not stated

### Side-by-side maps ###
inc.map <- tm_shape(s.sf) + tm_polygons(col="Income")+
  tm_legend(outside=TRUE) 
school.map <- tm_shape(s.sf) + tm_polygons(col="NoSchool")+
  tm_legend(outside=TRUE) 
name.map <- tm_shape(s.sf) + tm_polygons(col="NAME")+
  tm_legend(outside=TRUE) 
#creating all maps first, then arrange (below)

tmap_arrange(inc.map, school.map, name.map)
# very useful, need to create each map first before using tmap_arrange()

### Splitting data by polygons or group of polygons ###
tm_shape(s.sf) + tm_polygons(col = "Income") +
  tm_legend(outside = TRUE) +
  tm_facets( by = "NAME", nrow = 2)
# will show alphabetically unless otherwise specified
# to change order, change level order