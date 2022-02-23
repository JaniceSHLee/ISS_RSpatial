# Survey design and sampling
# Create random-systematic and stratified sampling 
# by M. I. Lubis Feb 2022

# Load spatial data
library(rgdal)
mydata <- readOGR("Income_schooling.shp")
mydata

# Plot
plot(mydata)

#### CREATE SAMPLING FOR THE WHOLE STUDY AREA ####
# Random sampling 
sp1 <- spsample(x = mydata, n = 10, type = 'random') #specifying number of points and type of sampling
points(sp1, pch=16, col = 'black')

# Systematic sampling (typically more representative)
sp2 <- spsample(x = mydata, n = 10, type = 'regular') #changing type of sampling
points(sp2, pch=15, col = 'blue')

#### CREATE SAMPLING FOR EACH POLYGON ####
# Create sampling sites for each polygon
# Simple random
sps1 <- list() # Create an empty output to save the result
for (i in 1:nrow(mydata)) {
  sps1[[i]] <- spsample(x = mydata[i,], type = 'random', n = 3)
}
# Combine into file (before was in list)
spss1 <- do.call(rbind, sps1)

# Plot
plot(mydata)
points(spss1, pch=16, col = 'black')


# Systematic
sps2 <- list()
for (i in 1:nrow(mydata)) {
  sps2[[i]] <- spsample(x = mydata[i,], type = 'regular', n = 3)
}
# Combine
spss2 <- do.call(rbind, sps2)

# Plot
plot(mydata)
points(spss2, pch=16, col = 'red')

# Stratified based on specified number of samples
sps3 <- list()
## statify based on layer/variable that you're interested in

# Create number of sample for each polygon
# Usually based on the proportion of suitable areas, 
# more sampling sites in a more suitable habitat, less points in a less suitable habitat
# no_sample <- 1:nrow(mydata)
no_sample <- rep(1:4, times = 4)

# Create stratified random with specified number of random points
for (i in 1:nrow(mydata)) {
  sps3[[i]] <- spsample(x = mydata[i,], type = 'random', n = no_sample[i])
}
# Combine
spss3 <- do.call(rbind, sps3)
#get coordinates of points
as.data.frame(spss3)

# Plot
plot(mydata)
points(spss3, pch=16, col = 'blue')
