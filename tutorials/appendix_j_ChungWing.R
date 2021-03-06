#load libraries
library(rgdal)
library(tmap)

#load precipitation data
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/precip.rds"))
P <- readRDS(z)

#load Texas boundary map
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/texas.rds"))
W <- readRDS(z)

#replace point boundary extent with that of Texas
P@bbox <- W@bbox
#note this step is necessary to interpolate across entire extent 
#of Texas rather than smallest rectangular extent enclosing point object

#visualise point data on map of Texas
tm_shape(W) + tm_polygons() +
  tm_shape(P) +
  tm_dots(col="Precip_in", palette = "RdBu", auto.palette.mapping = FALSE,
          title="Sampled precipitation \n(in inches)", size=0.7) +
  tm_text("Precip_in", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)

## Deterministic interpolation

## Proximity (Thiessen) interpolation
# tessellated surface splits polygons along midpoints of each sampled location

library(spatstat)  #used for the dirichlet tessellation function
library(maptools)  #used for conversion from SPDF to ppp
library(raster)    #used to clip out thiessen polygons

#create a tessellated surface
th <- as(dirichlet(as.ppp(P)), "SpatialPolygons")
#dirichlet function requires ppp format (like most functions in spatsat pkg)

#the dirichlet function does not carry over projection information
#requiring that this information be added manually
proj4string(th) <- proj4string(P)

#The tessellated surface does not store attribute information
#from the point data layer. We'll use the over() function (from the sp
#package) to join the point attributes to the tesselated surface via
#a spatial join. The over() function creates a dataframe that will need to
#be added to the `th` object thus creating a SpatialPolygonsDataFrame object
th.z <- over(th, P, fn=mean)
th.spdf <- SpatialPolygonsDataFrame(th, th.z)

#finally, we'll clip the tessellated  surface to the Texas boundaries
th.clp <- raster::intersect(W,th.spdf)
#specifying package (as many packages share function names)

#map the data
tm_shape(th.clp) + 
  tm_polygons(col="Precip_in", palette="RdBu", auto.palette.mapping=FALSE,
              title="Predicted precipitation \n(in inches)") +
  tm_legend(legend.outside=TRUE)

## Inverse Distance Weighted (IDW) 
#output is a raster, so create empty raster grid, then interpolate
#for this example, using IDW power value of 2
#choice of power coefficient is quite subjective - often falls between 1 and 3

library(gstat) #use gstat's idw routine
library(sp)    #used for the spsample function

#create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(P, "regular", n=50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE  # Create SpatialPixel object
fullgrid(grd) <- TRUE  # Create SpatialGrid object

#add P's projection information to the empty grid
proj4string(P) <- proj4string(P) #temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

#interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Precip_in ~ 1, P, newdata=grd, idp=1)

#convert to raster object then clip to Texas
r <- raster(P.idw)
r.m <- mask(r, W)

#plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#since choice of power function is subjective, can fine-tune
#leave-one-out validation routine can measure error in interpolated values

#leave-one-out validation routine
IDW.out <- vector(length = length(P))
for (i in 1:length(P)) {
  IDW.out[i] <- idw(Precip_in ~ 1, P[-i,], P[i,], idp=4)$var1.pred
}

#plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ P$Precip_in, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ P$Precip_in), col="red", lw=2,lty=2)
abline(0,1)
par(OP)

#compute RMSE (root-mean of squared residuals)
sqrt(sum((IDW.out - P$Precip_in)^2)/length(P))

#Can also create 95% CI map of interpolation model using jackknife technique
#this example uses power parameter of 2

#create the interpolated surface
img <- gstat::idw(Precip_in~1, P, newdata=grd, idp=2.0)
n <- length(P)
Zi <- matrix(nrow = length(img$var1.pred), ncol = n)

#remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(Precip_in~1, P[-i,], newdata=grd, idp=2.0)
  st <- addLayer(st,raster(Z1,layer=1))
  #calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

#jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

#compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            #compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) #sum the square of the difference

#compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

#create (CI / interpolated value) raster
img.sig <- img
img.sig$v <- CI /img$var1.pred 

#clip the confidence raster to Texas
r <- raster(img.sig, layer="v")
r.m <- mask(r, W)

#plot the map
tm_shape(r.m) + tm_raster(n=7,title="95% confidence interval \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

## Statistical interpolation methods

## Surface trends 

#define the 1st order polynomial equation
f.1 <- as.formula(Precip_in ~ X + Y) 

#add X and Y to P
P$X <- coordinates(P)[,1]
P$Y <- coordinates(P)[,2]

#run the regression model
lm.1 <- lm( f.1, data=P)

#use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd))) 

#clip the interpolated raster to Texas
r <- raster(dat.1st)
r.m <- mask(r, W)

#plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Predicted precipitation \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#define the 2nd order polynomial equation
f.2 <- as.formula(Precip_in ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

#add X and Y to P
P$X <- coordinates(P)[,1]
P$Y <- coordinates(P)[,2]

#run the regression model
lm.2 <- lm( f.2, data=P)

#use the regression model output to interpolate the surface
dat.2nd <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.2, newdata=grd))) 

#clip the interpolated raster to Texas
r <- raster(dat.2nd)
r.m <- mask(r, W)

#plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE,
            title="Predicted precipitation \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

## Kriging

#first create variogram model (computed on detrended data)

#define the 1st order polynomial equation
f.1 <- as.formula(Precip_in ~ X + Y) 

#compute the sample variogram; note that the f.1 trend model is one of the
#parameters passed to variogram(). This tells the function to create the 
#variogram on the de-trended data.
var.smpl <- variogram(f.1, P, cloud = FALSE, cutoff=1000000, width=89900)
var.smpl
#compute the variogram model by passing the nugget, sill and range values
#to fit variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=14, model="Sph", range=590000, nugget=0))

#the following plot allows us to assess the fit
plot(var.smpl, dat.fit, xlim=c(0,1000000))

#then generate Kriged surface
#krige function allows us to include trend model (so no need to detrend)

#define the trend model
f.1 <- as.formula(Precip_in ~ X + Y) 

#perform the krige interpolation (note the use of the variogram model
#created in the earlier step)
dat.krg <- krige(f.1, P, grd, dat.fit)

#convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, W)

#plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Predicted precipitation \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#generate variance and CI maps

#object stores variance as well as interpolation
r <- raster(dat.krg, layer="var1.var")
r.m <- mask(r, W)

#plot variance map
tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance map \n(in squared inches)") +tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#generate 95% CI map from variance object
#values are amounts above and below
r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m <- mask(r, W)

tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="95% CI map \n(in inches)") +tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)
