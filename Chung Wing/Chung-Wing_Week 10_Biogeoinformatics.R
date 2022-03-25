#Biogeoinformatics (sdm package)

library(sdm)
installAll()

#species distribution models have three steps: data prep, model fitting, prediction
# sdmData to read data and prep data object
# sdm to fit and evaluate species distribution models
# predict after model is fitted
# ensemble to predict given new dataset and combine into final projection

#read in example data (a sf object and four raster datasets)
library(raster)
library(rgdal)

file <- system.file("external/species.shp", package="sdm") # get the location of the species shapefile

# so, file is simply a filename (with path):
file

# read the species shapefile using the function shapefile:
species <- shapefile(file)
class(species) # it is a SpatialPointsDataFrame

#plot
plot(species)

# we can take a look at the head of attribute table in the species dataset:
head(species)
# you can see that there is a column containing the presence-absence records (i.e., Occurrence)

#we can plot presence and absence points separately with different colours:
plot(species[species$Occurrence == 1,],col='blue',pch=16)
points(species[species$Occurrence == 0,],col='red',pch=16)

##########
# Let's read predictor variables (raster datasets)
# We have four Ascii-Grids, so, let's just take the name of all files ending to '.asc' to 
# be able to read them together. list.files function can be used to get a list of files in a given path:

path <- system.file("external", package="sdm") # path to the folder contains the data
# list the name of files in the specified path, match the pattern # (means all files with a name ending to asc). 
# We asked to generate full names (i.e., names with the path)
lst <- list.files(path=path,pattern='asc$',full.names = T) 
lst # this is the name of raster files we want to use as predictor variables

# stack is a function in the raster package, to read/create a multi-layers raster dataset
preds <- stack(lst) # making a raster object
preds # see the specification of the raster layers (e.g., cell size, extent, etc.)
plot(preds)
plot(preds[[4]]) # only plot the 4th layer
plot(species,add=T) # let's add the species point on the previous plot

#now that data is read, can use sdm pkg
#putting data in package that creates a data object
d <- sdmData(formula=Occurrence~., train=species, predictors=preds)
#may want to specify variables and exclude collinear variables
d
# we didn't really need the formula in this example, as it would be easy for the function to guess which 
# dataset is species, and which are predictors. So, it could be like this:
d <- sdmData(train=species, predictors=preds)
d
# However, formula makes it so flexible to handle the variables, specifically if there are several other 
# information (e.g., time). If you have multiple species, you can have their names in the left hand side
# (e.g., sp1+sp2+sp3~.)

# You may also want to take part of variables:
d <- sdmData(formula=Occurrence~precipitation+temperature, train=species, 
             predictors=preds)
d
d <- sdmData(formula= ~., train=species, predictors=preds)

#now can fit the models (specify variables/features through formula)
# in the following example, we use 3 different methods to fit the models.
m1 <- sdm(Occurrence~.,data=d,methods=c('glm','gam','brt'))
m1

# as you can see, a report is generates shows how many percent of models were successful, and 
# their performance

# in the above example, the performance statistics were calculated based on the training dataset 
#(the data that were used to fit the mdoel). It is a better idea to have an independent dataset 
#(if so, we would specify in the test argument of sdmData). However, for most of cases, there is no such 
# data available, therefore, we can split the dataset as an alternative solution. Splitting (partitioning) can 
# be one time or several times (several replicates). There are also several methods to do that 
# (i.e., subsampling, cross-validation, bootsrapping)

# Here we are going to fit 5 models and evaluate them through 2 runs of subsampling, each draw 30 percent
# of training data as test dataset:
m2 <- sdm(Occurrence~.,data=d,methods=c('rf','tree','fda','mars','svm'),replicatin='sub',
          test.percent=30,n=2)
m2

getModelInfo(m2) # info on runs including modelID, whether they are successfully fitted and evaluated, etc.

# We can generate the roc curve and compare the results for all models:
roc(m2) #seeing how good the model is
# the plots can be smoothes:
roc(m2,smooth=T)

# in the following, we just predict the habitat suitability into the whole study area
# since the newdata is a raster object, the output is also a raster object

p1 <- predict(m1,newdata=preds,filename='p1.img') 
# many commonly used raster format is supported (through the package rgdal)
plot(p1)

p2 <- predict(m2,newdata=preds,filename='p2.img')
p2
nlayers(p2)
plot(p2[[1:4]]) # plot the first 12 rasters
# we can take the mean of raster over different runs for each method and species:
p2m <- predict(m2,newdata=preds,filename='p2m.img',mean=T)
p2m
plot(p2m)
# full names of rasters:
getZ(p2m)

#ensemble forecasting to use many models to compare results of projections
## much more robust rather than relying on one single method
## sdm pkg allows ensemble function to generate ensemble prediction based on multiple models

# in the following, we predict the habitat suitability using the ensemble function
# since the newdata is a raster object, the output is also a raster object

# ensemble based on a Weighted averaging that is weighted using AUC statistic
e1 <- ensemble(m1,newdata=preds,filename='e1.img',setting=list(method='weighted',stat='AUC')) 
plot(e1)

# ensemble based on a Weighted averaging that is weighted using TSS statistic 
# with threshold criterion number 2 which is max(Sensitivity+Specificity) or max(TSS)
e2 <- ensemble(m2,newdata=preds,filename='e2.img',
               setting=list(method='weighted',stat='TSS',opt=2))
e2
plot(e2)

# ensemble based on an Unweighted averaging
e3 <- ensemble(m2,newdata=preds,filename='e3.img',setting=list(method='unweighted'))
plot(e3)

# sdm pkg followed by graphical user interfaces (GUIs) that make it more interactive/user friendly

# m2 was the output of sdm function in the above examples, then can use gui to explore everything inside m2:
gui(m2)

#for time series - can make one model for each time and compare
#these sdms are for one time period (looking at species dist at one time)