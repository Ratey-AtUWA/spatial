# how to mask a rectangular grid.R ####

# first make a rectangular grid
# you will need to choose xmin, xmax, ymin, ymax, step based on your data
grid0 <- expand.grid(seq(xmin, xmax, step),
                     seq(ymin, ymax, step))
# Then make grid into an sp SpatialPoints object.
# The 'proj4string' option is a CRS object that will depend on your data 
rectgrid <- SpatialPoints(grid0, proj4string = myProjection)

# You will need the boundary of the area you want to crop the grid to, 
# in the same projection as the grid
# There are 3 steps:
#   1. make Polygon object (or more than one, if your boundary is in 
#      two or more parts, or has a 'hole' in it)
irregpoly <- Polygon(myBorder[,c(1,2)], hole=F)
#   2. combine Polygon objects into a single Polygons object
irregPolys <- Polygons(list(irregpoly),1)
#   3. convert Polygons object into a SpatialPolygons object, specifying the 
#      same projection as your grid
gridMask <- SpatialPolygons(list(irregPolys), 
                           proj4string = myProjection)
# Use the over() function from sp to categorise points in the rectangular grid
# as being inside or outside the boundary; categories are in object 'inOrOut',
# where a value of 1 means 'in' and NA means 'out'
inOrOut <- as.vector(over(rectgrid, gridMask))
# we use which() as we can't index using NA values
croppedGrid <- rectgrid[which(inOrOut>0)]
cat("Prediction grid:\n"); summary(croppedGrid)
