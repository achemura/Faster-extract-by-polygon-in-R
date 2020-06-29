# achemura@gmail.com
# V1.1. April 2020

#This R script computes zonals statistics of polygons over rasters faster. 
#One polygon shape is used to extract zonal stats from 7 folders, each with 24 rasters

rm(list=ls())
gc()

library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(tidyverse)

ptm <- proc.time() # start code time

setwd("~/DATABANK/Model_out")

folders = c("All_model", "Bale","Guji","Keffa", "Harar","Limu","Lekempti","Sidamo","Yirgar") # My folders with data 

# read the shapefile
dist.shape <- readOGR("~/DATABANK/Z_Shapefile/Special_all_pnrj.shp")# My polygon shapefile i want to extract with
for (i in folders){
  
  # read the files from different folders
  All_model.files <- list.files(paste0("~/DATABANK/Ethiopia/Coffee/Model_out/",i,"/Class/"), pattern = ".tif$", full=TRUE)
  
  # reading names without path to set column names  (FULLNAMES=F)
  names_1 <- list.files(paste0("~/DATABANK/Model_out/",i,"/Class/"), pattern = ".tif$")
  print(paste0("Processing files from folder: ",i))
  
  # stack all the files in the certain folder
  s <- stack(All_model.files)
  
  # The raster files does nnot have projection. Therefore we set projection of the shapefile 
  proj4string(s) = proj4string(dist.shape)
  
  # we are giving names to the all the bands from above filename
  names(s) = names_1
  
  # Converting stacked raster into points usinf raster to pints
  r.pts = rasterToPoints(s,spatial = T)
  
  # COnverting points and shapefile to sf library format to spatial join with shapefile
  r.pts_sf = sf::st_as_sf(r.pts)
  sf_shp = sf::st_as_sf(dist.shape)
  
  # Spatial join point and polygon
  join_dist = sf::st_join(r.pts_sf,sf_shp)
  
  # Converting spatial dataframe to normal dataframe
  df = as.data.frame(join_dist)
  
  # remove unnecessary columns
  df1 = df[ , -which(names(df) %in% c("geometry","ISO2","WWW","SQKM"))]
  
  # Summarize by grouping using dplyr lib
  df2 = df1 %>% group_by(Type)
  df3 = df2 %>% summarise_each(funs(sum(.,na.rm = TRUE)))
  
  # removing last row which has NA values
  df4 = df3[1:(nrow(df3)-1),]
  
  # Writing all the csv files 
  write.csv(df4,paste0("~/DATABANK/Model_out/",i,".csv"),row.names = F)
  print(paste0("Process completed from folder: ",i))
}

#_____________________
print("Inini ndinonzi inini")

time <- proc.time() - ptm # end code timer
time/60






