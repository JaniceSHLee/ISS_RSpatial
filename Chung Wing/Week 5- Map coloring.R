#looking al all map styles available in tmap package
#loading libraries
library(spData) # example datasets
library(tmap)   # map creation
library(sf)     # spatial data reprojection
library(shinyjs) 

#reprojecting object to a selected coordinate reference system
world_moll = st_transform(world, crs = "+proj=moll")

#visualise simple world map and fill polygons with color
tm_shape(world_moll) +
  tm_polygons(col = "lightblue") #tm_borders and tm_fill also, but this allows fill
#color so adjacent polygons are diff colors
tm_shape(world_moll) +
  tm_polygons(col = "MAP_COLORS")

#explore palettes
tmaptools::palette_explorer()
#add palette
tm_shape(world_moll) +
  tm_polygons(col = "MAP_COLORS",
              palette = "Pastel1")
#minimizing colors
tm_shape(world_moll) +
  tm_polygons(col = "MAP_COLORS",
              minimize = TRUE)

#color by variable instead
tm_shape(world_moll) +
  tm_polygons(col = "subregion")+
  tm_layout(legend.outside = TRUE) #note this is a categorical variable
#discrete/continuous variable
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp", ##default uses style = "pretty" which evenly spaces whole numbers
              legend.hist = TRUE) + #shows distribution of values
  tm_layout(legend.outside = TRUE) 
#indicate preferred number of classes
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp",
              legend.hist = TRUE,
              n = 4) +
  tm_layout(legend.outside = TRUE) 

#manual selection of breaks
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp", 
              style = "fixed", #include this for manual
              breaks = c(45, 60, 75, 90),
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE) 
#overwrite labels
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp", 
              style = "fixed",
              breaks = c(45, 60, 75, 90),
              labels = c("low", "medium", "high"),
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE) 
#using standard deviation as break width
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp", 
              style = "sd", #calculates sd to use
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE)
#creates groups with maximalized homogeneity
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp",
              style = "fisher",
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE) 
#IDs groups of similar values and maximizes differences between categories
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp",
              style = "jenks",
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE) 
#hierarchical clustering
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp",
              style = "hclust",
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE)
#bagged clustering
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp",
              style = "bclust",
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE) 
#k-means clustering
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp", 
              style = "kmeans",
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE) 
#quantile breaks
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp", 
              style = "quantile",
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE) 
#equal breaks (not recommended for skewed data)
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp", 
              style = "equal",
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE) 

#continuous color field
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp",
              style = "cont") +
  tm_layout(legend.outside = TRUE) 
#continuous - for skewed data
tm_shape(world_moll) +
  tm_polygons(col = "lifeExp",
              style = "order") +
  tm_layout(legend.outside = TRUE) 

#logrithmic distribution
##default style not proper for skew
tm_shape(world_moll) +
  tm_polygons(col = "pop") +
  tm_layout(legend.outside = TRUE) #mostly same color
##can use order but difficult to interpret
tm_shape(world_moll) +
  tm_polygons(col = "pop", 
              style = "order") +
  tm_layout(legend.outside = TRUE) 
##so use logrithmic instead!
tm_shape(world_moll) +
  tm_polygons(col = "pop", 
              style = "log10_pretty") +
  tm_layout(legend.outside = TRUE) 
##can also use continuous scale
tm_shape(world_moll) +
  tm_polygons(col = "pop", 
              style = "log10") +
  tm_layout(legend.outside = TRUE) 
