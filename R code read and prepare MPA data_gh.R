                        
#########################################################################################################################
## only count photos in the bbox of MPAs
##########################################################################################################################
###let's start a loop

library(sf)

MPA.shp<-st_read("~/MPAFlickr/shapefile/WDPA_May2020_marine-shapefile-polygons.shp")

MPA.sub<-MPA.shp[!is.element(MPA.shp$MARINE,c("0")),]


library(photosearcher)
setwd("~/MPAFlickr")

source("C:/Users/Emily/University of Aberdeen/Lusseau, David - MPAFlickr/photo_search_amended_total_only.R")

source("~/MPAFlickr/utils.R")

###initialise objects

start<-c("2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01")
end<-c("2010-12-31","2011-12-31","2012-12-31","2013-12-31","2014-12-31","2015-12-31","2016-12-31","2017-12-31","2018-12-31","2019-12-31")

photo_nb<-data.frame(year=rep(seq(2010,2019,1),each=dim(MPA.sub)[1]),WDPA_PID=rep(MPA.sub$WDPA_PID,10),number=NA,stringsAsFactors=FALSE)


m<-1
for (j in 1:length(start)) {
#cycle through the MPAs
for (i in 1:dim(MPA.sub)[1]) {

mpa.temp<-st_transform(MPA.sub[i,], 3857)
mpa.buf<-st_buffer(mpa.temp, 500)
mpa.temp<-st_transform(mpa.buf, 4326)


photo_nb$number[m]<-photo_search_amended_total_only(mindate_taken = start[j], maxdate_taken = end[j],sf_layer=mpa.temp)

m<-m+1
print(j)
print(i)
flush.console()

}
}

save(photo_nb,MPA.sub,file="C:/Users/David/OneDrive - University of Aberdeen/MPAFlickr/photosMPA_yearlycount.Rdata")
                          
#