
#----------------Importing Libraries-----------------------------#
library(lidR)
library(sf)
library(terra)
library(microbenchmark)
library(ggplot2)
library(gstat)

#--------------Reading .las file of leaf-on LiDAR data-------------#

las<- readLAS(files ="//Grs_nas_01/PhDData/Mudassar_Umar/LiDAR Processing/Test Plot/BR01/ULS On/ULS-on_BR01_2019-09-12.laz")
print(las)

#Validating LiDAR data
las_check(las)

# Plotting LiDAR data
plot(las)

# Plot las object by Classification,making the background white, displaying xyz axis and scale colors

plot(las, color = "Classification", bg = "white", axis = TRUE, legend = TRUE)

#----------Visualizing vertical structure using cross section plot------------------#

plot_crossection <- function(las,
                             p1 = c(min(las@data$X), mean(las@data$Y)),
                             p2 = c(max(las@data$X), mean(las@data$Y)),
                             width = 4, colour_by = NULL)
{
  colour_by <- rlang::enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() + theme_minimal()
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}
plot_crossection(las, colour_by = factor(Classification))

#-------------------Creating digital terrain model by using Kriging----------------#

dtm_kriging <- rasterize_terrain(las, 0.5, algorithm = kriging())
# plotting DTM
plot_dtm3d(dtm_kriging, bg = "white") 
# DTM using Delaunay triangulation method
dtm_tin <- rasterize_terrain(las, res = 0.5, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white") 

#-----------------Height Normalization-------------------------------#

plot(dtm_kriging, col=grey(1:50/50))

# Height Normalization

nlas <- las - dtm_kriging
plot(nlas)  #, size=4, bg="white")

hist(las$Z)
hist(nlas$Z)

hist(filter_ground(nlas$Z), breaks = seq(-300, 10, 300), main = "", xlab = "Elevation")
hist(filter_ground(nlas)$Z)

#--------------Creating CHM using Pit-free algorithm-------------------#

chm <- rasterize_canopy(nlas, res = 0.5, pitfree(thresholds = c(0, 10, 20), max_edge = c(0, 1.5)))
plot(chm)
# by increasing the max_edge 
chm1 <- rasterize_canopy(nlas, res = 0.5, pitfree(max_edge = c(0, 2.5)))
plot(chm1)

# using sub-cricle of 15cm
chm2 <- rasterize_canopy(nlas, res = 0.5, pitfree(subcircle = 0.15))
plot(chm2)

#-----------------Individual Tree Detection-------------#












