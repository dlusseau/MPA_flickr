###################################################################################################################################
#######R code associated with Erskine, Baillie & Lusseau (2021) One Earth
#####################################################################################################################################

####all folder structure has been removed, make sure when you read files that you are changing the code to appropriately address them

 library(sf)
 MPA.shp<-st_read("WDPA_May2020_marine-shapefile-polygons.shp") #This is the shapefile containing polygons for all MPA *AS OF MAY 2020* 
 
 # we are unsure about our right to store an additional copy of this shapefile separately from the site of the file's owners
 #we therefore recommend you download the file directly from https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA 
 #
 
 MPA.sub<-MPA.shp[!is.element(MPA.shp$MARINE,c("0")),] #we make sure MPAs have a marine component
 
 
#let's get some land first
library(rworldxtra)
data(countriesHigh)
worldmap<-st_as_sf(countriesHigh)
rm(countriesHigh)

st_crs(worldmap)
st_crs(MPA.sub)


###ok to make sure we compare the same thing we need to add the same buffer

bboxes<-list()

for (i in 1:dim(MPA.sub)[1]) {

mpa.temp<-st_transform(MPA.sub[i,], 3857)
mpa.buf<-st_buffer(mpa.temp, 500)
mpa.temp<-st_transform(mpa.buf, 4326)
bboxes[[i]]<-st_bbox(mpa.temp)
print(i)
flush.console()

 }
 
##ok now we know where all the MPAs are, we can create adjacent bbox of similar size

bboxes.controlN<-list()
#how about we just simply draw all 4 adjacent polygons directly instead of 
#fiddling with randomly selecting one and finding out if it is over land

bboxes.controlS<-list()
bboxes.controlW<-list()
bboxes.controlE<-list()



##we could apply or do something fancy with mutate but more transparent to use loop

for (i in 1:length(bboxes)) { 
 dx<-bboxes[[i]][3]-bboxes[[i]][1]
 dy<-bboxes[[i]][4]-bboxes[[i]][2]
 
 bboxes.controlN[[i]]<-st_sfc(st_polygon(list(rbind(
      c(bboxes[[i]][1],bboxes[[i]][4]),
      c(bboxes[[i]][3],bboxes[[i]][4]),
      c(bboxes[[i]][3],bboxes[[i]][4]+dy),
      c(bboxes[[i]][1],bboxes[[i]][4]+dy),
      c(bboxes[[i]][1],bboxes[[i]][4]) 
      ) )),crs=4326)
 
 bboxes.controlS[[i]]<-st_sfc(st_polygon(list(rbind(
      c(bboxes[[i]][1],bboxes[[i]][2]-dy),
      c(bboxes[[i]][3],bboxes[[i]][2]-dy),
      c(bboxes[[i]][3],bboxes[[i]][2]),
      c(bboxes[[i]][1],bboxes[[i]][2]),
      c(bboxes[[i]][1],bboxes[[i]][2]-dy) 
      ) )),crs=4326)
      
 bboxes.controlE[[i]]<- st_sfc(st_polygon(list(rbind(
      c(bboxes[[i]][3],bboxes[[i]][2]),
      c(bboxes[[i]][3]+dx,bboxes[[i]][2]),
      c(bboxes[[i]][3]+dx,bboxes[[i]][4]),
      c(bboxes[[i]][3],bboxes[[i]][4]),
      c(bboxes[[i]][3],bboxes[[i]][2]) 
      ) )),crs=4326)
 
 
 bboxes.controlW[[i]]<- st_sfc(st_polygon(list(rbind(
      c(bboxes[[i]][1]-dx,bboxes[[i]][2]),
      c(bboxes[[i]][1],bboxes[[i]][2]),
      c(bboxes[[i]][1],bboxes[[i]][4]),
      c(bboxes[[i]][1]-dx,bboxes[[i]][4]),
      c(bboxes[[i]][1]-dx,bboxes[[i]][2]) 
      ) )),crs=4326)

} 



###that should create 4 sets of adjacent polygons of similar size (including the buffer)

##now we can resample photographs

#we are likely to have some naughty bboxes that have abs(lon)>180 or abs(lat)>90
#let's just discard them


northside<-array(NA,length(bboxes))
for (i in 1:length(northside)) {northside[i]<-sum(abs(st_bbox(bboxes.controlN[[i]])[c(2,4)])>90)}

southside<-array(NA,length(bboxes))
for (i in 1:length(southside)) {southside[i]<-sum(abs(st_bbox(bboxes.controlS[[i]])[c(2,4)])>90)}

eastside<-array(NA,length(bboxes))
for (i in 1:length(eastside)) {eastside[i]<-sum(abs(st_bbox(bboxes.controlE[[i]])[c(1,3)])>180)}

westside<-array(NA,length(bboxes))
for (i in 1:length(westside)) {westside[i]<-sum(abs(st_bbox(bboxes.controlW[[i]])[c(1,3)])>180)}




library(photosearcher)
source("~/MPAFlickr/photo_search_amended_total_only.R")
source("~/MPAFlickr/utils.R")


photosN<-array(NA,length(bboxes))
photosS<-array(NA,length(bboxes))
photosE<-array(NA,length(bboxes))
photosW<-array(NA,length(bboxes))

#######################here you have to tune a bit the time period sampled to deal with API restrictions, an annual sampling example given here
for (i in 1:length(bboxes)) {

if (northside[i]!=1) {
photosN[i]<-tryCatch(photo_search_amended_total_only(mindate_taken = "2019-01-01", maxdate_taken = "2019-12-31",
                                      sf_layer=bboxes.controlN[[i]]), error=function(e) e)

                                      }
if (southside[i]!=1) {                                      
photosS[i]<-tryCatch(photo_search_amended_total_only(mindate_taken = "2019-01-01", maxdate_taken = "2019-12-31",
                                      sf_layer=bboxes.controlS[[i]]), error=function(e) e)
                                      }

if (eastside[i]!=1) {
photosE[i]<-tryCatch(photo_search_amended_total_only(mindate_taken = "2019-01-01", maxdate_taken = "2019-12-31",
                                      sf_layer=bboxes.controlE[[i]]), error=function(e) e)
                                      }

if (westside[i]!=1) {
photosW[i]<-tryCatch(photo_search_amended_total_only(mindate_taken = "2019-01-01", maxdate_taken = "2019-12-31",
                                      sf_layer=bboxes.controlW[[i]]), error=function(e) e)
                                      }

}

##perhaps a bit of an overkill?

landN<-array(0,length(bboxes))
landS<-array(0,length(bboxes))
landW<-array(0,length(bboxes))
landE<-array(0,length(bboxes))

#start<-Sys.time()
for (i in 1:length(bboxes)) {
if (northside[i]!=1) {
landN[i]<-as.numeric(length(st_intersection(bboxes.controlN[[i]],worldmap)))
}

if (southside[i]!=1) {                                      
landS[i]<-as.numeric(length(st_intersection(bboxes.controlS[[i]],worldmap)))
}

if (eastside[i]!=1) {
landE[i]<-as.numeric(length(st_intersection(bboxes.controlE[[i]],worldmap)))
}

if (westside[i]!=1) {
landW[i]<-as.numeric(length(st_intersection(bboxes.controlW[[i]],worldmap)))
}

}

#end<-Sys.time()
#end-start
save(landW,landE,landS,landN,photosW,photosE,photosS,photosN,file="~/MPAFlickr/control.photos2019.Rdata")


