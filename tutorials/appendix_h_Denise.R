# Appendix H: Point pattern analysis in R

# Load files 

load(url("https://github.com/mgimond/Spatial/raw/main/Data/ppa.RData"))
# in this tutorial we are using spatstat package which accepts ppp, owin and im data formats

starbucks # A ppp point layer of Starbucks stores in Massachusetts;
ma # An owin polygon layer of Massachusetts boundaries;
pop # An im raster layer of population density distribution.

#### This is an example of how to import the data if your layers are stored as shapefile and raster file formats ####
# directly downloading files from the author’s Github page: http://github.com/mgimond/Spatial/raw/master/Data
#library(sf)
#library(maptools)
#library(raster)

# Load an MA.shp polygon shapefile 
#s  <- st_read("MA.shp")
#w  <- as.owin(s)
#w.km <- rescale(w, 1000)) 

# Load a starbucks.shp point feature shapefile
#s  <- st_read("starbucks.shp")  
#starbucks  <- as.ppp(s)
#marks(starbucks) <- NULL
#starbucks <- rescale(starbucks, 1000)
#Window(starbucks) <- starbucks 

# Load a pop_sqmile.img population density raster layer
#img  <- raster("pop_sqmile.img")
#pop  <- as.im(img)

#### end of example ####

# prepping the data
library(spatstat)
marks(starbucks)  <- NULL # remove attribute information

## define study boundaries
Window(starbucks) <- ma #Windows function binds the starbucks point layer with the Massachusetts boundary layer
plot(starbucks, main=NULL, cols=rgb(0,0,0,.2), pch=20) #plot to ensure boundary is defined

## population density check
hist(pop, main=NULL, las=1) #left skewed, hence need to log transform
pop.lg <- log(pop)
hist(pop.lg, main=NULL, las=1)

# Density based analysis: First order property (how points are distributed relative to study area)

## Quadrat density 
Q <- quadratcount(starbucks, nx= 6, ny=3) # 3 rows and 6 columns

plot(starbucks, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE)  # Add quadrat grid

### Compute the density for each quadrat
Q.d <- intensity(Q)

### Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # number of points per square meter
plot(starbucks, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

### Rescale all layers to km
starbucks.km <- rescale(starbucks, 1000, "km")
ma.km <- rescale(ma, 1000, "km")
pop.km    <- rescale(pop, 1000, "km")
pop.lg.km <- rescale(pop.lg, 1000, "km")

### Compute the density for each quadrat (in counts per km2)
Q   <- quadratcount(starbucks.km, nx= 6, ny=3)
Q.d <- intensity(Q)

### Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(starbucks.km, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

## Quadrat density on a tessellated surface

hist(pop.lg.km, main=NULL, las=1)

brk  <- c( -Inf, 4, 6, 8 , Inf)  # Define the breaks
Zcut <- cut(pop.lg.km, breaks=brk, labels=1:4)  # Classify the raster
E    <- tess(image=Zcut)  # Create a tesselated surface

plot(E, main="", las=1) #plot tessellated object

### Density: Number of points per quadrat area
Q   <- quadratcount(starbucks.km, tess = E)  # Tally counts
Q.d <- intensity(Q)  # Compute density
Q.d

### Plot density values
plot(intensity(Q, image=TRUE), las=1, main=NULL)
plot(starbucks.km, pch=20, cex=0.6, col=rgb(1,1,1,.5), add=TRUE)

cl <-  interp.colours(c("lightyellow", "orange" ,"red"), E$n) #modify colours
plot( intensity(Q, image=TRUE), las=1, col=cl, main=NULL)
plot(starbucks.km, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)

## Kernel density
# use the function density from the spatstat package
# https://www.rdocumentation.org/packages/spatstat.core/versions/2.1-2/topics/density.ppp

K1 <- density(starbucks.km) # Using the default bandwidth. What is this default?
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)

### 50km bandwidth
K2 <- density(starbucks.km, sigma=50) # Using a 50km bandwidth
plot(K2, main=NULL, las=1)
contour(K2, add=TRUE)

### Try different smoothing function. The default is gaussian. 
K3 <- density(starbucks.km, kernel = "disc", sigma=50) # Using a 50km bandwidth
K3 <- density(starbucks.km, kernel = "quartic", sigma=50) # Using a 50km bandwidth
K3 <- density(starbucks.km, kernel = "epanechnikov", sigma=50) # Using a 50km bandwidth
plot(K3, main=NULL, las=1)
contour(K3, add=TRUE)

## Kernel density adjusted for covariate
# Starbucks store and population density

### Compute rho using the ratio method. Other methods are re-weight and transform
rho <- rhohat(starbucks.km, pop.lg.km,  method="ratio")
# Generate rho vs covariate plot
plot(rho, las=1, main=NULL, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))
# starbucks store increase exponentially with increasing population density

### map of predicted starbucks density if population density is the driving process
pred <- predict(rho)
cl   <- interp.colours(c("lightyellow", "orange" ,"red"), 100) # Create color scheme
plot(pred, col=cl, las=1, main=NULL) 
plot(pred, col=cl, las=1, main=NULL, gamma = 0.25) # gamma is used to stretch the colour scheme of the plot

### Predicted vs observed 
K1_vs_pred <- pairs(K1, pred, plot = FALSE)
plot(K1_vs_pred$pred ~ K1_vs_pred$K1, pch=20,
     xlab = "Observed intensity", 
     ylab = "Predicted intensity", 
     col = rgb(0,0,0,0.1))

### An example of the plot if the predicted vs observed matches
K1_vs_K1 <- pairs(K1, K1, labels = c("K1a", "K1b"), plot = FALSE)
plot(K1_vs_K1$K1a ~ K1_vs_K1$K1b, pch=20,
     xlab = "Observed intensity", 
     ylab = "Observed intensity")

### Summary statistics
summary(as.data.frame(K1_vs_pred))

plot(K1_vs_pred$pred ~ K1_vs_pred$K1, pch=20,
     xlab = "Observed intensity", 
     ylab = "Predicted intensity", 
     col = rgb(0,0,0,0.1),
     xlim = c(0, 0.04), ylim = c(0, 0.1))
abline(a=0, b = 1, col = "red")

## Modeling intensity as a function of a covariate - to mathematically model the relationship

# Create the Poisson point process model
PPM1 <- ppm(starbucks.km ~ pop.lg.km)
# Plot the relationship
plot(effectfun(PPM1, "pop.lg.km", se.fit=TRUE), main=NULL, 
     las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))

PPM1

# Distance based analysis: second order property (how points are distributed relative to each other)
# assumes points are stationary

## ANN analysis of starbucks stores
mean(nndist(starbucks.km, k=1)) # first nearest neighbour
mean(nndist(starbucks.km, k=2)) # second nearest neighbour

ANN <- apply(nndist(starbucks.km, k=1:100),2,FUN=mean) # nearest neighbour plot
plot(ANN ~ eval(1:100), type="b", main=NULL, las=1)

## K and L functions: cumulative analysis (distance between points for all distances)
 
K <- Kest(starbucks.km) # k function
plot(K, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))

L <- Lest(starbucks.km, main=NULL) # L function
plot(L, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) )) 
plot(L, . -r ~ r, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))

## Pair correlation function g 
g  <- pcf(starbucks.km)
plot(g, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))

# Hypothesis tests

## Test for clustering/dispersion (2nd order effect)
ann.p <- mean(nndist(starbucks.km, k=1)) ## ANN assuming uniform point density
ann.p

### generating expected ANN values using Monte Carlo methods
### if points are homogenously distributed

### null model
n     <- 599L               # Number of simulations
ann.r <- vector(length = n) # Create an empty object to be used to store simulated ANN values
for (i in 1:n){
    rand.p   <- rpoint(n=starbucks.km$n, win=ma.km)  # Generate random point locations
    ann.r[i] <- mean(nndist(rand.p, k=1))  # Tally the ANN values
}

plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")


### Taking into account population density 
n     <- 599L
ann.r <- vector(length=n)
for (i in 1:n){
    rand.p   <- rpoint(n=starbucks.km$n, f=pop.km) 
    ann.r[i] <- mean(nndist(rand.p, k=1))
}

Window(rand.p) <- ma.km  # Replace raster mask with ma.km window
plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

## Computing a pseudo p-value from the simulation
N.greater <- sum(ann.r > ann.p)
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1)
p

## Test for a poisson point process model with a covariate effect (1st order effect)
PPM1 <- ppm(starbucks.km ~ pop.lg.km)
PPM1

PPM0 <- ppm(starbucks.km ~ 1)
PPM0

starbucks.km$n / area(ma.km) 

anova(PPM0, PPM1, test="LRT")
