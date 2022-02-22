library(spdep)
library(classInt)
library(RColorBrewer)
library(rgdal)

#color palettes
pal1 <- brewer.pal(6,"Greys")
pal2 <- brewer.pal(8,"RdYlGn")
pal3 <- c(brewer.pal(9,"Greys"), "#FF0000")

#dataset
auckland <- readOGR(system.file("shapes/auckland.shp", package="spData")[1])

#map total infant deaths for diff areas
brks1 <- classIntervals(auckland$M77_85, n = 6, style = "equal")
brks2 <- classIntervals(auckland$M77_85, n = 6, style = "quantile")
print(spplot(auckland, "M77_85", at = brks1$brks, col.regions = pal1)
      ,position=c(0,0,.5,1),more=T)
print(spplot(auckland, "M77_85", at = brks2$brks, col.regions = pal1)
      ,position=c(0.5,0,1,1),more=T)
#choropleth maps where aggregated data are shown in two classification schemes
#impact on perceptual interpretation
#we want to produce perceptually tenable maps/map stability which conveys real effects

#can correct this bias by normalizing count data by area
#might make more sense for different measurements (vs. per population, for ex)
#assigning pop count and mortality count and assigning variables
pop <- auckland$Und5_81 * 9 #multiplying by number years (assumed the same)
mor <- auckland$M77_85 #this is mortality count over nine years

#calculate raw rate
auckland$raw.rate <- mor / pop * 1000
#establish breaks at equal and quantile intervals
brks1 <- classIntervals(auckland$raw.rate, n = 6, style = "equal")
brks2 <- classIntervals(auckland$raw.rate, n = 6, style = "quantile")
#map rate by census unit area
print(spplot(auckland, "raw.rate", at = brks1$brks, col.regions = pal1)
      ,position=c(0,0,.5,1),more=T)
print(spplot(auckland, "raw.rate", at = brks2$brks, col.regions = pal1)
      ,position=c(0.5,0,1,1),more=T)

#standardized mortality ratios/relative risk
##compare to an expected death count - pop x death rate
auck.rate <- sum(mor) / sum(pop)
mor.exp   <- pop * auck.rate  # Expected count over a nine year period
auckland$rel.rate <- 100 * mor / mor.exp
brks <- classIntervals(auckland$rel.rate, n = 6, style = "fixed", 
                       fixedBreaks = c(0,47, 83, 118, 154, 190, 704))
spplot(auckland, "rel.rate", at = brks$brks,col.regions=pal1)

#another way to standardize through chi squared
## follows normal distribution more (vs poisson, as before)
## using divergent color scheme (green less than expected, red more)
auckland$chi.squ = (mor - mor.exp) / sqrt(mor.exp)
brks <- classIntervals(auckland$chi.squ, n = 6, style = "fixed", 
                       fixedBreaks = c(-5,-3, -1, -2, 0, 1, 2, 3, 5))
spplot(auckland, "chi.squ", at = brks$brks,col.regions=rev(pal2))

#problem of sensitivity to small pop counts (unstable ratios)
#based on area instead
brks <- classIntervals(auckland$Und5_81, n = 6, style = "equal")
spplot(auckland, "Und5_81", at = brks$brks,col.regions=pal1)

#sensitivity towards low pop counts
#solution to generate probability map of data
#Global Empirical Bayes (EB) rate estimate
##based on a priori estimate of value to stabilize unstable ratios
##a priori estimate often based on some global mean
EB.est         <- EBest(auckland$M77_85, auckland$Und5_81 * 9 )
auckland$EBest <- EB.est$estmm * 1000
brks1          <- classIntervals(auckland$EBest, n = 10, style = "quantile")
brks2          <- classIntervals(auckland$raw.rate, n = 10, style = "quantile")
print(spplot(auckland, "EBest", at = brks1$brks, col.regions = pal3, 
             main="EB rates") ,position=c(0,0,.5,1),more=T)
print(spplot(auckland, "raw.rate", at = brks2$brks, col.regions = pal3, 
             main="Raw Rates") ,position=c(0.5,0,1,1),more=T)
#top 10% rates highlighted in red
#unstable rates (small pop) assigned lower weights

#can also adjust a priori mean/variance based on local mean rather than global
#known as local empirical Bayes rate estimates
nb      <- poly2nb(auckland) 
EBL.est <- EBlocal(auckland$M77_85, 9*auckland$Und5_81, nb)
auckland$EBLest <- EBL.est$est * 1000
brks1           <- classIntervals(auckland$EBLest, n = 10, style = "quantile")
brks2           <- classIntervals(auckland$raw.rate, n = 10, style = "quantile")
print(spplot(auckland, "EBLest", at = brks1$brks, col.regions = pal3, 
             main="Local EB rates") ,position=c(0,0,.5,1),more=T)
print(spplot(auckland, "raw.rate", at = brks2$brks, col.regions = pal3, 
             main="Raw Rates") ,position=c(0.5,0,1,1),more=T)
#highlighting top 10% rates in red




