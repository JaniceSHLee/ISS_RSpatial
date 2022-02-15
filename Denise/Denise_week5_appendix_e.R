#Refer to chapter 5.4 and 5.5. To cope with unstable rates,  use the empirical bayes method.

#for rates we use spdep package?
   
library(spdep)
library(classInt)
library(RColorBrewer)
library(rgdal)

#colour palettes
pal1 <- brewer.pal(6,"Greys")
pal2 <- brewer.pal(8,"RdYlGn")
pal3 <- c(brewer.pal(9,"Greys"), "#FF0000")

#load dataset from sepdep package
auckland <- readOGR(system.file("shapes/auckland.shp", package="spData")[1])
#use readOGR function
auckland #check dataset

# initial data map
brks1 <- classIntervals(auckland$M77_85, n = 6, style = "equal")
brks2 <- classIntervals(auckland$M77_85, n = 6, style = "quantile") #use quantile intervals instead. 
print(spplot(auckland, "M77_85", at = brks1$brks, col.regions = pal1)
      ,position=c(0,0,.5,1),more=T)
print(spplot(auckland, "M77_85", at = brks2$brks, col.regions = pal1)
      ,position=c(0.5,0,1,1),more=T)
# map seems to convey that the south has higher death rates. the maps made may not actually convey real effects. map stability to convey REAL EFFECTS.

#compute mortality rates. compute number of deaths per population count.
pop <- auckland$Und5_81 * 9 #assume population under 5 yo is constant over 9 years
mor <- auckland$M77_85  #total mortality of children over 9 years.

auckland$raw.rate <- mor / pop * 1000 #infant deaths per 1000 individuals per year
brks1 <- classIntervals(auckland$raw.rate, n = 6, style = "equal")
brks2 <- classIntervals(auckland$raw.rate, n = 6, style = "quantile")
print(spplot(auckland, "raw.rate", at = brks1$brks, col.regions = pal1)
      ,position=c(0,0,.5,1),more=T)
print(spplot(auckland, "raw.rate", at = brks2$brks, col.regions = pal1)
      ,position=c(0.5,0,1,1),more=T)
