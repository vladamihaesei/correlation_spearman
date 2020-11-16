library(dplyr)
library(raster)
library(RColorBrewer)
library(rnaturalearth)

###granite pentru layout nu este obligatoriu
ne_countries()
granite <- list("sp.polygons", ne_countries(scale =10),col = "black",lwd = 1.5,first=FALSE)
