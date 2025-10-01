library(geosphere) # contains the distm function, which we'll use to calculate distance given long-lat. 
library(sf)
library(mapedit)
library(mapview)
library(dplyr)
library(lubridate)
library(Rmisc)
library(ggpubr)
library(bslib)
library(stringer)



####  This is where the data should come from!  Has presence/absence
# https://ipt.vliz.be/eurobis/resource?r=lifewatch_cpod

event=read.delim('event.txt', header = T, sep = '\t')
occurence = read.delim('occurrence.txt', header = T, sep = '\t')

allData=merge(event, occurence, by='id')

depID=as.data.frame(str_split(allData$id, '_'))
names(depID)=NULL
depID=t(depID)
depID=as.data.frame(depID)
names(depID)=c('Event', 'Date', 'Hour')
startDatetime=ymd_h(str_c(depID$Date, ' ',depID$Hour))

data=data.frame(depID$Event, startDatetime, allData$occurrenceStatus, allData$decimalLatitude.x, allData$decimalLongitude.x, allData$locality.x, allData$maximumDepthInMeters.x)
rm(list=setdiff(ls(), 'data'))

names(data)= c('depID', 'datetime', 'status','decLat','decLon','Station', 'maxDepth')

data$PPM=ifelse(data$status=='present',1,0)


## Changed this as there were 23 stations the grouping way, and 21 by station, so these are likely correct.
locations=data %>%
  group_by(Station) %>%
  summarise_at(vars(decLon,decLat), list(name = mean))
names(locations)=c('Station', 'long','lat')

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

save(grd, loc_sf,locations, data, file='DTO_DUC2_PpData.Rdata')




#




























#####  For the as.data.frame()#####  For the Biodiversity Dataset from EMODnet biology

# #emntB=read.csv('Biodiversity records.csv')
# 
# dt=extractDTstampEMODnetBio(emntB, status='start')
# emntB$datetime=dt$timestamp
# 
# DataMat<-as.matrix(cbind(emntB$decimallongitude,emntB$decimallatitude))
# allLocs=unique(DataMat)
# Dist_Mat<-distm(allLocs,allLocs,fun=distHaversine)
# hc <- hclust(as.dist(Dist_Mat), method="complete")
# 
# # define the distance threshold, in this case 500 m
# d=500
# 
# # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
# 
# clust <-cutree(hc, h=d)
# allLocs=as.data.frame(allLocs)
# allLocs$clust=clust
# 
# names(allLocs) = c("decimallongitude","decimallatitude","Group")
# 
# dataG=merge(emntB, allLocs, by = c('decimallatitude', 'decimallongitude')) 
# 
# locations=allLocs %>%
#   group_by(Group) %>%
#   summarise_at(vars(decimallongitude,decimallatitude), list(name = mean))
# names(locations)=c('Group', 'long','lat')
# 
# # make the coordinates a numeric matrix
# loc_mx <- data.matrix(locations[,2:3])
# # convert the coordinates to a multipoint feature
# loc_mp <- st_multipoint(loc_mx)
# # convert the multipoint feature to sf
# loc_sf <- st_sf(st_cast(st_sfc(loc_mp), "POINT"), locations, crs=4326)
# # make a grid
# 
# grd <- st_set_crs(st_make_grid(loc_sf), 4326)
# # only keep grid polygons that contain at least one point
# grd <- grd[which(sapply(st_contains(st_sf(grd), loc_sf),length)>0)]
# 
# save(grd, loc_sf,locations, dataG, file='DTO_DUC2_PpData.Rdata')
