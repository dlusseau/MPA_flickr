library(sf)

load("~/MPAFlickr/control_polygones.Rdata")


MPA.shp<-st_read("~/MPAFlickr/shapefile/WDPA_May2020_marine-shapefile-polygons.shp")

MPA.sub<-MPA.shp[!is.element(MPA.shp$MARINE,c("0")),]
rm(MPA.shp)

library(photosearcher)
source("~/MPAFlickr/photo_search_amended_page1.R")

source("~/MPAFlickr/utils.R")

 


northside<-array(NA,length(bboxes))
for (i in 1:length(northside)) {northside[i]<-sum(abs(st_bbox(bboxes.controlN[[i]])[c(2,4)])>90)}

southside<-array(NA,length(bboxes))
for (i in 1:length(southside)) {southside[i]<-sum(abs(st_bbox(bboxes.controlS[[i]])[c(2,4)])>90)}

eastside<-array(NA,length(bboxes))
for (i in 1:length(eastside)) {eastside[i]<-sum(abs(st_bbox(bboxes.controlE[[i]])[c(1,3)])>180)}

westside<-array(NA,length(bboxes))
for (i in 1:length(westside)) {westside[i]<-sum(abs(st_bbox(bboxes.controlW[[i]])[c(1,3)])>180)}




#setwd("C:/Users/David/OneDrive - University of Aberdeen/MPAFlickr") #for the key
 


##intialise the data.frames
mpa.temp<-st_transform(MPA.sub[315,], 3857)       ##just because it has photos
mpa.buf<-st_buffer(mpa.temp, 500)
mpa.temp<-st_transform(mpa.buf, 4326)


photos<-tryCatch(photo_search_amended_page1(mindate_taken = "2017-01-01", maxdate_taken = "2017-12-31",
                                      sf_layer=mpa.temp), error=function(e) e)

if(exists("photos")==TRUE & class(photos)[1]=="list") {
photo_details_MPA<-as.data.frame(photos[[1]])
photo_details_MPA$WDPA_PID<-MPA.sub$WDPA_PID[1]

rm(photos)
}

photo_details_MPA<-photo_details_MPA[-(1:dim(photo_details_MPA)[1]),]
# 
#############################################


##intialise the data.frame for controls

###North
photo_details_controlN<-photo_details_MPA

###South
photo_details_controlS<-photo_details_MPA

###East
photo_details_controlE<-photo_details_MPA

###West
photo_details_controlW<-photo_details_MPA


#################################################################################
####################################################################################





#cycle through the MPAs
for (i in 1:dim(MPA.sub)[1]) {

#####MPA
mpa.temp<-st_transform(MPA.sub[i,], 3857)
mpa.buf<-st_buffer(mpa.temp, 500)
mpa.temp<-st_transform(mpa.buf, 4326)


photos<-tryCatch(photo_search_amended_page1(mindate_taken = "2017-01-01", maxdate_taken = "2017-12-31",
                                      sf_layer=mpa.temp), error=function(e) e)

if(exists("photos")==TRUE & class(photos)[1]=="list") {
photo.temp<-as.data.frame(photos[[1]])
if (dim(photo.temp)[1]!=0) {
photo.temp$WDPA_PID<-MPA.sub$WDPA_PID[i]

photo_details_MPA<-rbind(photo_details_MPA,photo.temp)
}


rm(photos,photo.temp)
}

######Control N
if (northside[i]!=1) {

photos<-tryCatch(photo_search_amended_page1(mindate_taken = "2017-01-01", maxdate_taken = "2017-12-31",
                                      sf_layer=bboxes.controlN[[i]]), error=function(e) e)

if(exists("photos")==TRUE & class(photos)[1]=="list") {
photo.temp<-as.data.frame(photos[[1]])
if (dim(photo.temp)[1]!=0) {
photo.temp$WDPA_PID<-MPA.sub$WDPA_PID[i]

photo_details_controlN<-rbind(photo_details_controlN,photo.temp)
}


rm(photos,photo.temp)
}
}

######Control S
 if (southside[i]!=1) {

photos<-tryCatch(photo_search_amended_page1(mindate_taken = "2017-01-01", maxdate_taken = "2017-12-31",
                                      sf_layer=bboxes.controlS[[i]]), error=function(e) e)

if(exists("photos")==TRUE & class(photos)[1]=="list") {
photo.temp<-as.data.frame(photos[[1]])
if (dim(photo.temp)[1]!=0) {
photo.temp$WDPA_PID<-MPA.sub$WDPA_PID[i]

photo_details_controlS<-rbind(photo_details_controlS,photo.temp)
}


rm(photos,photo.temp)
}
}

######Control E
if (eastside[i]!=1) {

photos<-tryCatch(photo_search_amended_page1(mindate_taken = "2017-01-01", maxdate_taken = "2017-12-31",
                                      sf_layer=bboxes.controlE[[i]]), error=function(e) e)

if(exists("photos")==TRUE & class(photos)[1]=="list") {
photo.temp<-as.data.frame(photos[[1]])
if (dim(photo.temp)[1]!=0) {
photo.temp$WDPA_PID<-MPA.sub$WDPA_PID[i]

photo_details_controlE<-rbind(photo_details_controlE,photo.temp)
}


rm(photos,photo.temp)
}
}

######Control W
if (westside[i]!=1) {

photos<-tryCatch(photo_search_amended_page1(mindate_taken = "2017-01-01", maxdate_taken = "2017-12-31",
                                      sf_layer=bboxes.controlW[[i]]), error=function(e) e)

if(exists("photos")==TRUE & class(photos)[1]=="list") {
photo.temp<-as.data.frame(photos[[1]])
if (dim(photo.temp)[1]!=0) {
photo.temp$WDPA_PID<-MPA.sub$WDPA_PID[i]

photo_details_controlW<-rbind(photo_details_controlW,photo.temp)
}


rm(photos,photo.temp)
}
}



print(i)
flush.console()

}

save(photo_details_controlE, photo_details_controlN, photo_details_controlS, photo_details_controlW, photo_details_MPA, file="~/MPAFlickr/photo_details_2017.Rdata")
