library("geosphere") # contains the distm function, which we'll use to calculate distance given long-lat. 
library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(mapedit)
library(mapview)
library(dplyr)
library(lubridate)
library(Rmisc)
library(ggpubr)
library(bslib)
library(stringer)

emntB=read.csv('Biodiversity records.csv')

dt=extractDTstampEMODnetBio(emntB, status='start')
emntB$datetime=dt$timestamp

DataMat<-as.matrix(cbind(emntB$decimallongitude,emntB$decimallatitude))
allLocs=unique(DataMat)
Dist_Mat<-distm(allLocs,allLocs,fun=distHaversine)
hc <- hclust(as.dist(Dist_Mat), method="complete")

# define the distance threshold, in this case 500 m
d=500

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame

clust <-cutree(hc, h=d)
allLocs=as.data.frame(allLocs)
allLocs$clust=clust

names(allLocs) = c("decimallongitude","decimallatitude","Group")

dataG=merge(emntB, allLocs, by = c('decimallatitude', 'decimallongitude')) 

locations=allLocs %>%
  group_by(Group) %>%
  summarise_at(vars(decimallongitude,decimallatitude), list(name = mean))
names(locations)=c('Group', 'long','lat')

# make the coordinates a numeric matrix
loc_mx <- data.matrix(locations[,2:3])
# convert the coordinates to a multipoint feature
loc_mp <- st_multipoint(loc_mx)
# convert the multipoint feature to sf
loc_sf <- st_sf(st_cast(st_sfc(loc_mp), "POINT"), locations, crs=4326)
# make a grid

grd <- st_set_crs(st_make_grid(loc_sf), 4326)
# only keep grid polygons that contain at least one point
grd <- grd[which(sapply(st_contains(st_sf(grd), loc_sf),length)>0)]

save(grd, loc_sf,locations, dataG, file='DTO_DUC2_PpData.Rdata')
