##Point Pattern Analysis

#mean centre = average coordinate values
#standard distance = variance b/w avg distance b/w points and mean centre
#standard deviational ellipse = separate standard distances for each axis

#now, more powerful analysis based on density or distance

####density based approach (as a measure of intensity of whatever process is going on - first order property)####

#characterizes patterns based on distribution across study area - can be global or local
#global density = number of points/study regions's surface area
#local density - divides up study area to see if intensity differs across area (or constant)
##can use quadrat density or kernel density to measure local density

#quadrat density - divides area into sub-regions and calculates density for each quadrat (can be all diff shapes)
##choice of size and shape is important - small enough to detect subtle changes, large enough to avoid blank quadrats
##quadrats can also be defined by covariate (another variable) rather than a constant size/shape
##tessellation = converting a continuous field into discretized areas
##can plot and see if intensity changes across tessellated covariate - be careful of MAUP (modifiable areal unit problem)

#kernel density - based on subregions overlapping as a moving subregion window (defined as kernel)
#each cell assigned density value computed for window centered on that cell
#can also weight the points following kernel function 
#basic kernel = equal weight, others assign weights inversely proportional to distance from center, others follow distribution function
#can also weight based on covariates
##create intensity function dependent on covariate - estimate function (p) by ratio, re-weight, and transform methods
#can model relationship of points and underlying covariate - uses statistical models (ex: poisson)

####distance based approach (how points are distributed relative to each other - second order property)####

#concerned with how observations influence each other (ex: parent trees and clusters)
#three different approaches of average nearest neighbor, K and L functions, and pair correlation function

#average nearest neighbor (ANN)
#measure avg distance from each point in study to nearest point
#can plot ANN curve for distances to near --> distant neighbors
#shape of curve provides insight into spatial arrangement (clustered vs. scattered)
#this all assumes the process doesn't change (differences in distribution is due to interactions rather than function of location)
#also keep in mind the extent of clustering depends at what scale we are looking at

#K and L functions
#K-function summarises distance b/w points for all distances (avg sum of # points at diff distance lags for each point/area event denstiy)
#dividing number of points within each distance by total number of points
##plot K against Kexpected (if there's a IRP/CSR process at play) 
###IRP process meaning independent random process and CSR meaning complete spatial randomness
###these processes mean any event has equal probability of occurring in any location independent of other events
##so Kexpected derived from area and assumption that points are randomly distributed
##if K > Kexpected, indication of clustering at a distance band
##if K < Kexpected, dispersion of points at a distance band
##also assumes underlying process is constant
#however, K function is hard to see small differences due to upward curve
##can transform values to make Kexpected completely horizontal --> L function
#therefore values under line are clustered vs. values above line are dispersed

#pair correlation function g
#K and L functions are cumulative (so it's difficult to know which exact distance it strays from expected)
#g-function instead sums points in distance bands rather than all points within a distance
#g > 1 = clustered, g < 1 = dispersed
#also assumes point process is constant

#first order effect - observations vary based on place due to changes in underlying property
#second order effect - observations vary based on place due to interaction effects b/w observations
#for ex: first order could be soil nutrients on tree distribution, while second order is seed dispersal and depends on presence of other trees



