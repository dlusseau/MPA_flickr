##trying to get all photos instead of counting them
###################################################################################################################################
######################################################################################################################################
### trying to get all the photos for 10 years for all non-zeros mpas

mindate="2010-01-01"
maxdate="2019-12-31"
library(photosearcher)
setwd("~/MPAFlickr")
 
 library(sf)
 MPA.shp<-st_read("~/MPAFlickr/shapefile/WDPA_May2020_marine-shapefile-polygons.shp")
 
 MPA.sub<-MPA.shp[!is.element(MPA.shp$MARINE,c("0")),]

i=1
mpa.temp<-st_transform(MPA.sub[i,], 3857)
mpa.buf<-st_buffer(mpa.temp, 500)
mpa.temp<-st_transform(mpa.buf, 4326)
all.photos<-tryCatch( photo_search(mindate_taken = mindate, maxdate_taken = maxdate,sf_layer=mpa.temp),error=function(e) "e")

MPA.photos<-all.photos[,c(2,3,13,16,17,18)]
MPA.photos$WDPA_PID<-MPA.sub$WDPA_PID[i]


for (i in 2:dim(MPA.sub)[1]) {   # here we broke the loop into multiple loops (roughly 500 PID each), each run in a separate session. this was more time efficiency than parellelising it

mpa.temp<-st_transform(MPA.sub[i,], 3857)
mpa.buf<-st_buffer(mpa.temp, 500)
mpa.temp<-st_transform(mpa.buf, 4326)


all.photos<-tryCatch( photo_search(mindate_taken = mindate, maxdate_taken = maxdate,sf_layer=mpa.temp),error=function(e) NA)

if (is.null(dim(all.photos))==FALSE) {
if (dim(all.photos)[1]>0) {

temp<-all.photos[,c(2,3,13,16,17,18)]
temp$WDPA_PID<-MPA.sub$WDPA_PID[i]

MPA.photos<-rbind(MPA.photos,temp)

}
}


print(i)
flush.console()

}

save(MPA.photos,file="~/MPAFlickr/users_details/allMPAphotos.Rdata")


############################################################################################################################################################
###########################################################################################################################################################
############################################################################################################################################################
###########################################################################################################################################################
############################################################################################################################################################
###########################################################################################################################################################
############################################################################################################################################################
###########################################################################################################################################################
############################################################################################################################################################
###########################################################################################################################################################
# stiching

files<-list.files("~/MPAFlickr/users_details",full.names=TRUE)

load(files[1])
MPA.photos.all<-MPA.photos
rm(MPA.photos)

for (i in 2:length(files)) {
load(files[i])
MPA.photos.all<-rbind(MPA.photos.all,MPA.photos)
rm(MPA.photos)

}

whichdup<-duplicated(MPA.photos.all) #in case we had replicated samples as we deal with API connection error and restarted loops

MPA.photos.all<-MPA.photos.all[!whichdup,]

str(MPA.photos.all)


############################################################################################################################################################
###########################################################################################################################################################
############################################################################################################################################################
###########################################################################################################################################################
############################################################################################################################################################
###########################################################################################################################################################
############################################################################################################################################################
###########################################################################################################################################################
############################################################################################################################################################
###########################################################################################################################################################
# controlS


load("~/MPAFlickr/control_polygones.Rdata")

mindate="2010-01-01"
maxdate="2019-12-31"
library(photosearcher)
setwd("~/MPAFlickr")
 
 library(sf)
 
northside<-array(NA,length(bboxes))
for (i in 1:length(northside)) {northside[i]<-sum(abs(st_bbox(bboxes.controlN[[i]])[c(2,4)])>90)}

southside<-array(NA,length(bboxes))
for (i in 1:length(southside)) {southside[i]<-sum(abs(st_bbox(bboxes.controlS[[i]])[c(2,4)])>90)}

eastside<-array(NA,length(bboxes))
for (i in 1:length(eastside)) {eastside[i]<-sum(abs(st_bbox(bboxes.controlE[[i]])[c(1,3)])>180)}

westside<-array(NA,length(bboxes))
for (i in 1:length(westside)) {westside[i]<-sum(abs(st_bbox(bboxes.controlW[[i]])[c(1,3)])>180)}


 MPA.shp<-st_read("~/MPAFlickr/shapefile/WDPA_May2020_marine-shapefile-polygons.shp")
 
 MPA.sub<-MPA.shp[!is.element(MPA.shp$MARINE,c("0")),]
PID<-MPA.sub$WDPA_PID

rm(MPA.shp,MPA.sub)

load("~/MPAFlickr/control.skeleton.Rdata")
control.photosS<-control.photosN
control.photosE<-control.photosN
control.photosW<-control.photosN


for (i in 1:999) { #ditto here, here is a loop example, replicated varying the interval to cumulatively include all PID

if (northside[i]!=1) {
all.photosN<-tryCatch( photo_search(mindate_taken = mindate, maxdate_taken = maxdate,sf_layer=bboxes.controlN[[i]]),error=function(e) NA)
if (is.null(dim(all.photosN))==FALSE) {
if (dim(all.photosN)[1]>0) {

temp<-all.photosN[,c(2,3,13,16,17,18)]
temp$WDPA_PID<-PID[i]

control.photosN<-rbind(control.photosN,temp)

}
}
}

if (southside[i]!=1) {

all.photosS<-tryCatch( photo_search(mindate_taken = mindate, maxdate_taken = maxdate,sf_layer=bboxes.controlS[[i]]),error=function(e) NA)
if (is.null(dim(all.photosS))==FALSE) {
if (dim(all.photosS)[1]>0) {

temp<-all.photosS[,c(2,3,13,16,17,18)]
temp$WDPA_PID<-PID[i]

control.photosS<-rbind(control.photosS,temp)

}
}
}


if (westside[i]!=1) {
all.photosW<-tryCatch( photo_search(mindate_taken = mindate, maxdate_taken = maxdate,sf_layer=bboxes.controlW[[i]]),error=function(e) NA)
if (is.null(dim(all.photosW))==FALSE) {
if (dim(all.photosW)[1]>0) {

temp<-all.photosW[,c(2,3,13,16,17,18)]
temp$WDPA_PID<-PID[i]

control.photosW<-rbind(control.photosW,temp)

}
}
}

if (eastside[i]!=1) {
all.photosE<-tryCatch( photo_search(mindate_taken = mindate, maxdate_taken = maxdate,sf_layer=bboxes.controlE[[i]]),error=function(e) NA)
if (is.null(dim(all.photosE))==FALSE) {
if (dim(all.photosE)[1]>0) {

temp<-all.photosE[,c(2,3,13,16,17,18)]
temp$WDPA_PID<-PID[i]

control.photosE<-rbind(control.photosE,temp)

}
}
}



print(i)
flush.console()

}

save(control.photosN,control.photosS,control.photosW,control.photosE,file="~/MPAFlickr/allcontrolphotos_Jan2021_1_to_1kminus1.Rdata")

