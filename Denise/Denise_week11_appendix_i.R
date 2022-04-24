# Spatial autocorrelation
#looking at features with similar values to determine if they're clustered or random
#Moran's I test is a popular way to quantify spatial autocorrelation 
#Moran’s I statistic is the correlation coefficient for the relationship between a variable & its surrounding values

#load data
load(url("https://github.com/mgimond/Spatial/raw/main/Data/moransI.RData"))

#map income distribution
library(tmap)
tm_shape(s1) + tm_polygons(style="quantile", col = "Income") +
    tm_legend(outside = TRUE, text.size = .8) 

#define neighboring polygons
#can define neighbor as contiguous polygon, distance bands, and k nearest neighbors
#last two use polygon centroids to measure distance
library(spdep)
nb <- poly2nb(s1, queen=TRUE) #queen = true means shared vertex (otherwise, shared edge)

#lists neighboring polygons
nb[[1]]
#extracting name of polygon 1
s1$NAME[1]
s1$NAME[c(2,3,4,5)] #extracting other county names

#assigning weights to polygons
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
#here we use equal weight with (1/# neighbors)
#drawback is that edge polygons have fewer neighbors so may change estimates
#zero.policy=TRUE option allows for lists of non-neighbors, use with caution

#see weights of first polygon's neighbors
lw$weights[1]

#compute spatially lagged values (average neighbor income value for each polygon)
#indicates an expected value if there is no pattern
Inc.lag <- lag.listw(lw, s1$Income) 

#computing Moran's I statistic (need to assesss how different slope is from 0)
#can compute this analytically (makes some restrictive assumptions so not always reliable)
#can also use Monte Carlo test (permutation bootstrapping) which has no assumptions

#method 1 (hard way/Monte Carlo method)
M <- lm(Inc.lag ~ s1$Income) #create a regression model of lagged income vs income
plot( Inc.lag ~ s1$Income, pch=20, asp=1, las=1) #plot data
coef(M)[2] #slope of line is Moran's I coefficient

#assess if slope is significantly different from zero (null hypothesis)
n <- 599L   #define the number of simulations
I.r <- vector(length=n)  #create an empty vector

for (i in 1:n){
    #randomly shuffle income values
    x <- sample(s1$Income, replace=FALSE)
    #compute new set of lagged values
    x.lag <- lag.listw(lw, x)
    #compute the regression slope and store its value
    M.r    <- lm(x.lag ~ x)
    I.r[i] <- coef(M.r)[2]
}

hist(I.r, main=NULL, xlab="Moran's I", las=1) #plot the histogram of simulated Moran's I values
abline(v=coef(M)[2], col="red") #then add our observed Moran's I value to the plot
#suggests this is different from null 

#compute a pseudo p-value from this simulation
N.greater <- sum(coef(M)[2] > I.r) #find # simulated values > observed value

p <- min(N.greater + 1, n + 1 - N.greater) / (n + 1) #compute one sided p
p #implies rejection of null

#method 2 for Moran's I statistic (easy way)
moran.test(s1$Income,lw) #p value is analytically computed rather than simulated (may not be most accurate)

#simulate instead
MC<- moran.mc(s1$Income, lw, nsim=599)
MC #view results (including p value)

#plot the distribution (note that this is a density plot instead of a histogram)
plot(MC, main="", las=1)

#defining neighbors with distance bands instead of polygons
#can study range of autocorrelation values as function of distance
#would look at how Moran's I values changes over distance
coo <- coordinates(s1) #extract center of each polygon
S.dist  <-  dnearneigh(coo, 0, 50000) #define search radius for centers within given distance (0,50000 are inner and outer radiuses)
lw <- nb2listw(S.dist, style="W",zero.policy=T) #ID all neighboring polygons for each polygon

#run MC simulation
MI  <-  moran.mc(s1$Income, lw, nsim=599,zero.policy=T) 
plot(MI, main="", las=1) #plot
MI #display p value and summary stats

#can also decompose Moran's I value to look at localised components
#maps hot spots and cold spots
#can then assess whether these high and low spots are significant through Monte Carlo methods

