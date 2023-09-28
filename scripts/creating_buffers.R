library(dplyr)
library(raster)
library(sf)
library(rgdal)
library(rnaturalearth)

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]

# read in points data
a_ionica <- read.csv('./Acis_ionica.csv')
coordinates(a_ionica) <- ~decimalLongitude+decimalLatitude
epsg <- make_EPSG()
wanted_crs <- epsg %>% filter(code == 4326)
wanted_crs <- wanted_crs$prj4
proj4string(a_ionica) <- wanted_crs

plot(a_ionica, col = "red")
plot(land, add = T, col = "white")

# ok so already a problem, would probably discount the one by indonesia

# converting points to geocentric coords (epsg 4978)
epsg <- make_EPSG()
transform_crs <- epsg %>% filter(code == 4978)
transform_crs <- transform_crs$prj4
a_ionica_trans <- spTransform(a_ionica, CRSobj = transform_crs)
  
# averaging x and y values to find centroid
mat <- a_ionica_trans@coords
mat <- data.frame(mat)

q <- quantile(mat$de)

avg_x <- mean(mat$decimalLongitude)
avg_y <- mean(mat$decimalLatitude)
cent <- c(avg_x, avg_y)

plot(a_ionica_trans)
points(cent)

# locating outlier points (3 sd away from cent)
for (i in )
x_dist <- mat$decimalLongitude - cent
x_sd <- sd(mat$decimalLongitude)
y_sd <- sd(mat$decimalLatitude)
upper <- cent + (1* c(x_sd, y_sd))
lower <- cent - (1* c(x_sd, y_sd))
drop_up <- which(a_ionica_trans@coords > upper)
drop_low <- which(a_ionica_trans@coords < lower)
new_mat <- a_ionica_trans@coords[!drop_up]
new_mat <- new_mat[!drop_low]
coords(new_mat) <- ~
proj4string(new_mat) <- transform_crs

# drawing alpha hull
a_ionica_range <- st_convex_hull(new_mat)
a_ionica_range <- spTransform(a_ionica_range, crs = wanted_crs)

# plotting
plot(a_ionica_range, col = "blue")
plot(land, add = T)
plot(a_ionica, add = T, col = "red")



t <- raster('./Ruscus_hypoglossum_SDM.tif')
plot(t)
res(t)
