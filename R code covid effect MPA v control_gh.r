####### COVID effect

mindate="2020-01-01"
maxdate="2020-12-31"
library(photosearcher)
setwd("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr")
 
 library(sf)
 MPA.shp<-st_read("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr/shapefile/WDPA_May2020_marine-shapefile-polygons.shp")
 
 MPA.sub<-MPA.shp[!is.element(MPA.shp$MARINE,c("0")),]

i=2
mpa.temp<-st_transform(MPA.sub[i,], 3857)
mpa.buf<-st_buffer(mpa.temp, 500)
mpa.temp<-st_transform(mpa.buf, 4326)
all.photos<-tryCatch( photo_search(mindate_taken = mindate, maxdate_taken = maxdate,sf_layer=mpa.temp),error=function(e) "e")

MPA.photos<-all.photos[,c(2,3,13,16,17,18)]
MPA.photos$WDPA_PID<-MPA.sub$WDPA_PID[i]


for (i in 9159:dim(MPA.sub)[1]) {

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

save(MPA.photos,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr/allMPAphotos_COVID2020.Rdata")

#############################################################################################################
##controls

# controlS


load("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr/control_polygones.Rdata")

mindate="2020-01-01"
maxdate="2020-12-31"
library(photosearcher)
setwd("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr")
 
 library(sf)
 
northside<-array(NA,length(bboxes))
for (i in 1:length(northside)) {northside[i]<-sum(abs(st_bbox(bboxes.controlN[[i]])[c(2,4)])>90)}

southside<-array(NA,length(bboxes))
for (i in 1:length(southside)) {southside[i]<-sum(abs(st_bbox(bboxes.controlS[[i]])[c(2,4)])>90)}

eastside<-array(NA,length(bboxes))
for (i in 1:length(eastside)) {eastside[i]<-sum(abs(st_bbox(bboxes.controlE[[i]])[c(1,3)])>180)}

westside<-array(NA,length(bboxes))
for (i in 1:length(westside)) {westside[i]<-sum(abs(st_bbox(bboxes.controlW[[i]])[c(1,3)])>180)}


 MPA.shp<-st_read("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr/shapefile/WDPA_May2020_marine-shapefile-polygons.shp")
 
 MPA.sub<-MPA.shp[!is.element(MPA.shp$MARINE,c("0")),]
PID<-MPA.sub$WDPA_PID

rm(MPA.shp,MPA.sub)

load("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr/control.skeleton.Rdata")
control.photosS<-control.photosN
control.photosE<-control.photosN
control.photosW<-control.photosN


for (i in 1:16645) {

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

save(control.photosN,control.photosS,control.photosW,control.photosE,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr/allcontrolphotos_COVID2020.Rdata")

##################################################################################################################################################

load("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr/allcontrolphotos_COVID2020.Rdata")

control.photosE$treatment<-"control"
control.photosE$orientation<-"East"
control.photosW$treatment<-"control"
control.photosW$orientation<-"West"
control.photosN$treatment<-"control"
control.photosN$orientation<-"North"
control.photosS$treatment<-"control"
control.photosS$orientation<-"South"

control_photos_all<-rbind(control.photosE,control.photosW,control.photosS,control.photosN)
control_photos_all$days<-format(control_photos_all$datetaken,"%Y-%m-%d")


library(dplyr)

big.table<-control_photos_all %>%
group_by(orientation,WDPA_PID) %>%
 count(owner,days)

small.table<-big.table %>%
group_by(orientation,WDPA_PID) %>%
 summarize(userdays=n())

unfolded_control_2020<-as.data.frame(small.table)

rm(control.photosE,control.photosN,control.photosS,control.photosW,control_photos_all)

load("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr/allMPAphotos_COVID2020.Rdata")
MPA.photos$days<-format(MPA.photos$datetaken,"%Y-%m-%d")


big.table<-MPA.photos %>%
group_by(WDPA_PID) %>%
 count(owner,days)

small.table<-big.table %>%
group_by(WDPA_PID) %>%
 summarize(userdays=n())

unfolded_MPA_2020<-as.data.frame(small.table)

unfolded_MPA_2020$orientation<-"none"
unfolded_MPA_2020<-unfolded_MPA_2020[,c(3,1,2)]

unfolded_MPA_2020$treatment<-"MPA"
unfolded_control_2020$treatment<-"control"

unfolded_MPA_2020$covid<-"yes"
unfolded_control_2020$covid<-"yes"
unfolded_2020<-rbind(unfolded_MPA_2020,unfolded_control_2020)
unfolded_2020$year<-2020
unfolded_2020<-unfolded_2020[,c(1,2,6,3,4,5)]

load("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr/TabUserDaysControl.Rdata")
load("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr/TabUserDaysMPA.Rdata")
unfolded_mpa$treatment<-"MPA"
unfolded_control$treatment<-"control"
unfolded_mpa$covid<-"no"
unfolded_control$covid<-"no"

unfolded_all<-rbind(unfolded_mpa,unfolded_control,unfolded_2020)


destinations<-unique(unfolded_all$WDPA_PID)
years<-unique(unfolded_all$year)
levels<-unique(unfolded_all$orientation)

userdays.df<-data.frame(WDPA_PID=rep(destinations,each=(length(years)*length(levels))),year=rep(rep(years,each=length(levels)),length(destinations)),orientation=rep(levels,length(years)*length(destinations)))

userdays.df$userdays.count<-0

library(data.table)
unfolded.dt<-data.table(unfolded_all)
userdays.df.dt<-data.table(userdays.df)
setkey(unfolded.dt,WDPA_PID,year,orientation)
setkey(userdays.df.dt,WDPA_PID,year,orientation)

tic<-Sys.time()
for (i in 1:dim(userdays.df)[1]) {

#user.df$user.count[i]<-length(unique(unfolded_all$owner[unfolded_all$WDPA_PID==user.df$WDPA_PID[i] & unfolded_all$year==user.df$year[i] & unfolded_all$orientation==user.df$orientation[i]]))
if(length(unfolded.dt[J(userdays.df$WDPA_PID[i],userdays.df$year[i],userdays.df$orientation[i]),nomatch=0L]$userdays)>0) {
userdays.df$userdays.count[i]<-unfolded.dt[J(userdays.df$WDPA_PID[i],userdays.df$year[i],userdays.df$orientation[i]),nomatch=0L]$userdays
}

}
tic-Sys.time()

save(userdays.df,unfolded_all,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/OneDrive - University of Aberdeen/MPAFlickr/alldata_COVID_userdays_Feb2021.Rdata")


userdays.df.trt<-data.frame(WDPA_PID=rep(destinations,each=(length(years)*2)),year=rep(rep(years,each=2),length(destinations)),treatment=rep(c("MPA","control"),length(years)*length(destinations)))

userdays.df.trt$user.count<-0

userdays.df.trt$user.count[userdays.df.trt$treatment=="MPA"]<-userdays.df$userdays.count[userdays.df$orientation=="none"]

userdays.df.trt.control<-subset(userdays.df.trt,treatment=="control")

user.df.dt.c<-data.table(subset(userdays.df,orientation!="none"))
setkey(user.df.dt.c,WDPA_PID,year)

tic<-Sys.time()
for (i in 1:(dim(userdays.df.trt.control)[1])) {
userdays.df.trt.control$user.count[i]<-median(user.df.dt.c[J(userdays.df.trt.control$WDPA_PID[i],userdays.df.trt.control$year[i]),nomatch=0L]$userdays.count)


}
Sys.time()-tic


userdays.df.trt$user.count[userdays.df.trt$treatment=="control"]<-userdays.df.trt.control$user.count


library(sf)
 MPA.shp<-st_read("~/MPAFlickr/shapefile/WDPA_May2020_marine-shapefile-polygons.shp")
 
 MPA.sub<-MPA.shp[!is.element(MPA.shp$MARINE,c("0")),]

 userdays.df.trt$area<-MPA.sub$GIS_AREA[match(userdays.df.trt$WDPA_PID,MPA.sub$WDPA_PID)]
userdays.df.trt$country<-MPA.sub$ISO3[match(userdays.df.trt$WDPA_PID,MPA.sub$WDPA_PID)]

library(countrycode)
userdays.df.trt$continent<-countrycode(userdays.df.trt$country,origin="iso3c",destination="region")

missing.continent<-unique(userdays.df.trt$country[which(is.na(userdays.df.trt$continent)==TRUE)]) #dealing with a few countries code
continents<-unique(userdays.df.trt$continent)

userdays.df.trt$continent[userdays.df.trt$country==missing.continent[1]]<-continents[2]
userdays.df.trt$continent[userdays.df.trt$country==missing.continent[2]]<-continents[5]
userdays.df.trt$continent[userdays.df.trt$country==missing.continent[3]]<-continents[6]
userdays.df.trt$continent[userdays.df.trt$country==missing.continent[4]]<-continents[4]
userdays.df.trt$continent[userdays.df.trt$country==missing.continent[5]]<-continents[5]
userdays.df.trt$continent[userdays.df.trt$country==missing.continent[6]]<-continents[6]
userdays.df.trt$continent[userdays.df.trt$country==missing.continent[7]]<-continents[5]
userdays.df.trt$continent[userdays.df.trt$country==missing.continent[8]]<-continents[5]
userdays.df.trt$continent[userdays.df.trt$country==missing.continent[9]]<-"ABNJ"
userdays.df.trt$continent[userdays.df.trt$country==missing.continent[10]]<-continents[3]
userdays.df.trt$continent[userdays.df.trt$country==missing.continent[11]]<-continents[5]
userdays.df.trt$continent[userdays.df.trt$country==missing.continent[12]]<-continents[6]

userdays.df.trt$log_area<-log10(userdays.df.trt$area)

userdays.df.trt.ready<-subset(userdays.df.trt,year!="2007"&year!="2009")

userdays.df.trt.ready$covid<-"no"
userdays.df.trt.ready$covid[userdays.df.trt.ready$year==2020]<-"yes"


userdays.df.trt.ready$user.count[userdays.df.trt.ready$treatment=="MPA" & userdays.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(7809)]& userdays.df.trt.ready$year<2020]<-NA
userdays.df.trt.ready$user.count[userdays.df.trt.ready$treatment=="MPA" & userdays.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(10147)]& userdays.df.trt.ready$year<2020]<-NA

userdays.df.trt.ready$user.count<-as.integer(userdays.df.trt.ready$user.count) #for a few control cell, median led to .5 values

save(userdays.df.trt.ready,file="~/MPAFlickr/alldata_covid_userdays_photo_Feb2021.Rdata")

load("~/MPAFlickr/alldata_covid_userdays_photo_Feb2021.Rdata")

library(glmmTMB)

zi.covid.userdays<-glmmTMB(user.count~log_area+treatment*covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment*covid,
family=truncated_nbinom2)


zi.covid.userdays2<-glmmTMB(user.count~log_area+treatment*covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

zi.covid.userdays3<-glmmTMB(user.count~log_area+treatment+covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

zi.covid.userdays8<-glmmTMB(user.count~log_area+treatment+covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment+covid,
family=truncated_nbinom2)

zi.covid.userdays9<-glmmTMB(user.count~log_area+treatment*covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment+covid,
family=truncated_nbinom2)


zi.covid.userdays4<-glmmTMB(user.count~log_area+treatment*covid+(1|WDPA_PID)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

zi.covid.userdays5<-glmmTMB(user.count~log_area+treatment+covid+(1|WDPA_PID)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

zi.covid.userdays6<-glmmTMB(user.count~log_area+treatment+covid+(1|WDPA_PID)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment+covid,
family=truncated_nbinom2)

zi.covid.userdays7<-glmmTMB(user.count~log_area+treatment*covid+(1|WDPA_PID)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment+covid,
family=truncated_nbinom2)

AIC(zi.covid.userdays,zi.covid.userdays2,zi.covid.userdays3,zi.covid.userdays4,zi.covid.userdays5,zi.covid.userdays6,zi.covid.userdays7,zi.covid.userdays8,zi.covid.userdays9,zi.covid.userdays10)

covid.ready<-subset(userdays.df.trt.ready,year==2020)

zi.covid.userdays.2020<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID),
data=subset(userdays.df.trt.ready,year==2020),
ziformula=~log_area+treatment,
family=truncated_nbinom2)

###############################################################################################################


zi.covid.user.count<-glmmTMB(user.count~log_area+treatment*covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment*covid,
family=nbinom2)

zi.covid.user.count2<-glmmTMB(user.count~log_area+treatment*covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment+covid,
family=nbinom2)

zi.covid.user.count3<-glmmTMB(user.count~log_area+treatment+covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment+covid,
family=nbinom2)

zi.covid.user.count4<-glmmTMB(user.count~log_area+treatment+covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment*covid,
family=nbinom2)

zi.covid.user.count5<-glmmTMB(user.count~log_area+treatment*covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.covid.user.count6<-glmmTMB(user.count~log_area+treatment+covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)


zi.covid.user.count7<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment+covid,
family=nbinom2)

AIC(zi.covid.user.count,zi.covid.user.count2,zi.covid.user.count3,zi.covid.user.count4,zi.covid.user.count5,zi.covid.user.count6)
# 


####################################################################################################################################################
######################################################################################################################################################
####USERS

##################################################################################################################################################

load("~/MPAFlickr/allcontrolphotos_COVID2020.Rdata")

control.photosE$treatment<-"control"
control.photosE$orientation<-"East"
control.photosW$treatment<-"control"
control.photosW$orientation<-"West"
control.photosN$treatment<-"control"
control.photosN$orientation<-"North"
control.photosS$treatment<-"control"
control.photosS$orientation<-"South"

control_photos_all<-rbind(control.photosE,control.photosW,control.photosS,control.photosN)


library(dplyr)

big.table<-control_photos_all %>%
group_by(owner,orientation,WDPA_PID) %>%
 summarize(count=n())

unfolded_control_2020<-as.data.frame(big.table)

rm(control.photosE,control.photosN,control.photosS,control.photosW,control_photos_all)

load("~/MPAFlickr/allMPAphotos_COVID2020.Rdata")

MPA.photos$orientation<-"none"
big.table<-MPA.photos %>%
group_by(owner,orientation,WDPA_PID) %>%
 summarize(count=n())


unfolded_MPA_2020<-as.data.frame(big.table)

unfolded_MPA_2020$treatment<-"MPA"
unfolded_control_2020$treatment<-"control"


unfolded_MPA_2020$covid<-"yes"
unfolded_control_2020$covid<-"yes"
unfolded_2020<-rbind(unfolded_MPA_2020,unfolded_control_2020)
unfolded_2020$year<-2020

unfolded_2020<-unfolded_2020[,c(1,2,3,7,4,5,6)]


load("~/MPAFlickr/TabUserControl.Rdata")
load("~/MPAFlickr/TabUserMPA.Rdata")
unfolded_mpa$treatment<-"MPA"
unfolded_control$treatment<-"control"
unfolded_mpa$covid<-"no"
unfolded_control$covid<-"no"

unfolded_all<-rbind(unfolded_mpa,unfolded_control,unfolded_2020)


destinations<-unique(unfolded_all$WDPA_PID)
years<-unique(unfolded_all$year)
levels<-unique(unfolded_all$orientation)

user.df<-data.frame(WDPA_PID=rep(destinations,each=(length(years)*length(levels))),year=rep(rep(years,each=length(levels)),length(destinations)),orientation=rep(levels,length(years)*length(destinations)))

user.df$user.count<-0

library(data.table)
unfolded.dt<-data.table(unfolded_all)
setkey(unfolded.dt,WDPA_PID,year,orientation)

tic<-Sys.time()
for (i in 1:dim(user.df)[1]) {

#user.df$user.count[i]<-length(unique(unfolded_all$owner[unfolded_all$WDPA_PID==user.df$WDPA_PID[i] & unfolded_all$year==user.df$year[i] & unfolded_all$orientation==user.df$orientation[i]]))

user.df$user.count[i]<-length(unfolded.dt[J(user.df$WDPA_PID[i],user.df$year[i],user.df$orientation[i]),nomatch=0L]$owner)


}
tic-Sys.time()

user.df$photo.count<-0
tic<-Sys.time()
for (i in 1:dim(user.df)[1]) {

user.df$photo.count[i]<-sum(unfolded.dt[J(user.df$WDPA_PID[i],user.df$year[i],user.df$orientation[i]),nomatch=0L]$count)


}
tic-Sys.time()

user.df<-subset(user.df,year>2009)

destinations<-unique(user.df$WDPA_PID)
years<-unique(user.df$year)
levels<-unique(user.df$orientation)

user.df.trt<-data.frame(WDPA_PID=rep(destinations,each=(length(years)*2)),year=rep(rep(years,each=2),length(destinations)),treatment=rep(c("MPA","control"),length(years)*length(destinations)))

user.df.trt$user.count<-0
user.df.trt$photo.count<-0
user.df.trt$photouser.count<-0

user.df.trt$user.count[user.df.trt$treatment=="MPA"]<-user.df$user.count[user.df$orientation=="none"]
user.df.trt$photo.count[user.df.trt$treatment=="MPA"]<-user.df$photo.count[user.df$orientation=="none"]
user.df.trt$photouser.count[user.df.trt$treatment=="MPA"]<-user.df$photo.count[user.df$orientation=="none"]/user.df$user.count[user.df$orientation=="none"]

user.df.trt.control<-subset(user.df.trt,treatment=="control")
library(data.table)
user.df.dt<-data.table(subset(user.df,orientation!="none")) ###had forgotten to subset none
setkey(user.df.dt,WDPA_PID,year)

tic<-Sys.time()
for (i in 1:(dim(user.df.trt.control)[1])) {
temp<-user.df.dt[J(user.df.trt.control$WDPA_PID[i],user.df.trt.control$year[i]),nomatch=0L]
user.df.trt.control$user.count[i]<-median(temp$user.count)
user.df.trt.control$photo.count[i]<-median(temp$photo.count)

user.df.trt.control$photouser.count[i]<-median(temp$photo.count/temp$user.count,na.rm=TRUE)
}
Sys.time()-tic


user.df.trt$user.count[user.df.trt$treatment=="control"]<-user.df.trt.control$user.count
user.df.trt$photo.count[user.df.trt$treatment=="control"]<-user.df.trt.control$photo.count
user.df.trt$photouser.count[user.df.trt$treatment=="control"]<-user.df.trt.control$photouser.count

user.df.trt[is.na(user.df.trt$photouser.count),]$photouser.count<-0

user.df.trt$photo.count<-as.integer(user.df.trt$photo.count) # to deal with the few control cells with median yielding .5 values
user.df.trt$user.count<-as.integer(user.df.trt$user.count) # to deal with the few control cells with median yielding .5 values




library(sf)
 MPA.shp<-st_read("~/MPAFlickr/shapefile/WDPA_May2020_marine-shapefile-polygons.shp")
 
 MPA.sub<-MPA.shp[!is.element(MPA.shp$MARINE,c("0")),]

 user.df.trt$area<-MPA.sub$GIS_AREA[match(user.df.trt$WDPA_PID,MPA.sub$WDPA_PID)]
user.df.trt$country<-MPA.sub$ISO3[match(user.df.trt$WDPA_PID,MPA.sub$WDPA_PID)]

library(countrycode)
user.df.trt$continent<-countrycode(user.df.trt$country,origin="iso3c",destination="region")

missing.continent<-unique(user.df.trt$country[which(is.na(user.df.trt$continent)==TRUE)])
continents<-unique(user.df.trt$continent)

user.df.trt$continent[user.df.trt$country==missing.continent[1]]<-continents[2]
user.df.trt$continent[user.df.trt$country==missing.continent[2]]<-continents[5]
user.df.trt$continent[user.df.trt$country==missing.continent[3]]<-continents[6]
user.df.trt$continent[user.df.trt$country==missing.continent[4]]<-continents[4]
user.df.trt$continent[user.df.trt$country==missing.continent[5]]<-continents[5]
user.df.trt$continent[user.df.trt$country==missing.continent[6]]<-continents[6]
user.df.trt$continent[user.df.trt$country==missing.continent[7]]<-continents[5]
user.df.trt$continent[user.df.trt$country==missing.continent[8]]<-continents[5]
user.df.trt$continent[user.df.trt$country==missing.continent[9]]<-"ABNJ"
user.df.trt$continent[user.df.trt$country==missing.continent[10]]<-continents[3]
user.df.trt$continent[user.df.trt$country==missing.continent[11]]<-continents[5]
user.df.trt$continent[user.df.trt$country==missing.continent[12]]<-continents[6]

user.df.trt$log_area<-log10(user.df.trt$area)

user.df.trt.ready<-subset(user.df.trt,year!="2007"&year!="2009")
 
#####we are missing data for 10147 and 7809 MPA treatment level
#MPA.sub$WDPA_PID[c(7809,10147)]
#"555542209" "332911"  

user.df.trt.ready$photo.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[7809]]

user.df.trt.ready$user.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(7809)]& user.df.trt.ready$year<2020]<-NA
user.df.trt.ready$photouser.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(7809)]& user.df.trt.ready$year<2020]<-NA

user.df.trt.ready$user.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(10147)]& user.df.trt.ready$year<2020]<-NA
user.df.trt.ready$photouser.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(10147)]& user.df.trt.ready$year<2020]<-NA

user.df.trt.ready$covid<-"no"
user.df.trt.ready$covid[user.df.trt.ready$year==2020]<-"yes"


save(user.df.trt.ready,file="~/MPAFlickr/alldata_covid_users_photo_Mar2021.Rdata")

load("~/MPAFlickr/alldata_covid_users_photo_Mar2021.Rdata")

library(glmmTMB)

zi.covid.photo.count<-glmmTMB(photo.count~log_area+treatment*covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment*covid,
family=truncated_nbinom2)

zi.covid.photo.count2<-glmmTMB(photo.count~log_area+treatment*covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment+covid,
family=truncated_nbinom2)

zi.covid.photo.count3<-glmmTMB(photo.count~log_area+treatment+covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment+covid,
family=truncated_nbinom2)

zi.covid.photo.count4<-glmmTMB(photo.count~log_area+treatment+covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment*covid,
family=truncated_nbinom2)

zi.covid.photo.count5<-glmmTMB(photo.count~log_area+treatment*covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

zi.covid.photo.count6<-glmmTMB(photo.count~log_area+treatment+covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

AIC(zi.covid.photo.count5,zi.covid.photo.count6)




AIC(zi.covid.photo.count2,zi.covid.photo.count3,zi.covid.photo.count4)

zi.covid.user.count<-glmmTMB(user.count~log_area+treatment*covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment*covid,
family=nbinom2)

zi.covid.user.count2<-glmmTMB(user.count~log_area+treatment*covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment+covid,
family=nbinom2)

zi.covid.user.count3<-glmmTMB(user.count~log_area+treatment+covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment+covid,
family=nbinom2)

zi.covid.user.count4<-glmmTMB(user.count~log_area+treatment+covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment*covid,
family=nbinom2)

zi.covid.user.count5<-glmmTMB(user.count~log_area+treatment*covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.covid.user.count6<-glmmTMB(user.count~log_area+treatment+covid+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)


AIC(zi.covid.user.count,zi.covid.user.count2,zi.covid.user.count3,zi.covid.user.count4,zi.covid.user.count5,zi.covid.user.count6)

save(zi.covid.user.count5,zi.covid.photo.count3,file="~/MPAFlickr/bestCOVIDmodels_photo_and_user.Rdata")
save(zi.covid.user.count2,file="~/MPAFlickr/bestCOVIDmodels_userdays.Rdata")


library(gridExtra)
library(glmmTMB)
library(sjPlot)
library(glmmTMB)

g1p<-plot_model(zi.covid.photo.count3,show.values=TRUE,vline.color = "black",value.size=2,value.offset=.3,title="Number of photos",axis.labels =c("COVID-19 year","Treatment (MPA/control)","log10(Area)"),grid=FALSE)$conditional+ theme_sjplot(base_size=8)
g2p<-plot_model(zi.covid.photo.count3,show.values=TRUE,vline.color = "black",value.size=2,value.offset=.3,title="",axis.labels =c("COVID-19 year","Treatment (MPA/control)","log10(Area)"),grid=FALSE)$zero_inflated+ theme_sjplot(base_size=8)
tiff("~/MPAFlickr/covidphotomodel_final.tif",units="cm",width=20,height=10,res=200)
grid.arrange(g1p,g2p,nrow = 2, ncol = 1)

dev.off()



g1u<-plot_model(zi.covid.user.count5,show.values=TRUE,vline.color = "black",value.size=2,value.offset=.3,title="Number of users",axis.labels =c("MPA in COVID-19 year","COVID-19 year","Treatment (MPA/control)","log10(Area)"),grid=FALSE)$conditional+ theme_sjplot(base_size=8)
g2u<-plot_model(zi.covid.user.count5,show.values=TRUE,vline.color = "black",value.size=2,value.offset=.3,title="",axis.labels =c("Treatment (MPA/control)","log10(Area)"),grid=FALSE)$zero_inflated+ theme_sjplot(base_size=8)
tiff("~/MPAFlickr/covidusermodel_final.tif",units="cm",width=20,height=10,res=200)
grid.arrange(g1u,g2u,nrow = 2, ncol = 1)

dev.off()



library(sjPlot)

tiff("~/MPAFlickr/covidusermodel_final.tif",units="cm",width=20,height=5,res=200)
plot_model(zi.covid.user.count2,show.values=TRUE,value.size=2,value.offset=.3,show.zeroinf =FALSE,title="Number of users - fixed effects",axis.labels =c("MPA in COVID-19 year","COVID-19 year","Treatment (MPA/control)","log10(Area)"))
dev.off()

plot_model(zi.covid.userdays9)

g1<-plot_model(zi.covid.user.count2,show.values=TRUE,vline.color = "black",value.size=2,value.offset=.3,title="Number of users.days",axis.labels =c("MPA in COVID-19 year","COVID-19 year","Treatment (MPA/control)","log10(Area)"),grid=FALSE)$conditional+ theme_sjplot(base_size=8)
g2<-plot_model(zi.covid.user.count2,show.values=TRUE,vline.color = "black",value.size=2,value.offset=.3,title="",axis.labels =c("COVID-19 year","Treatment (MPA/control)","log10(Area)"),grid=FALSE)$zero_inflated+ theme_sjplot(base_size=8)

library(gridExtra)

tiff("~/MPAFlickr/coviduserdaysmodel_final.tif",units="cm",width=20,height=10,res=200)
grid.arrange(g1,g2,nrow = 2, ncol = 1)
dev.off()



###################################################################################################################################################
#redo plots

#user model
library(gridExtra)
library(glmmTMB)
library(sjPlot)
library(glmmTMB)

g1<-plot_model(zi.m16.user,show.values=TRUE,vline.color = "black",value.size=2,value.offset=.3,title="Number of users",axis.labels =c("Treatment (MPA/control)","log10(Area)"),grid=FALSE)$conditional+ theme_sjplot(base_size=8)
g2<-plot_model(zi.m16.user,show.values=TRUE,vline.color = "black",value.size=2,value.offset=.3,title="",axis.labels =c("Treatment (MPA/control)","log10(Area)"),grid=FALSE)$zero_inflated+ theme_sjplot(base_size=8)
tiff("~/MPAFlickr/usermodel_final.tif",units="cm",width=20,height=10,res=200)
grid.arrange(g1,g2,nrow = 2, ncol = 1)

dev.off()




g1<-plot_model(zi.m16.userdays,show.values=TRUE,vline.color = "black",value.size=2,value.offset=.3,title="Number of user.days",axis.labels =c("Treatment (MPA/control)","log10(Area)"),grid=FALSE)$conditional+ theme_sjplot(base_size=8)
g2<-plot_model(zi.m16.userdays,show.values=TRUE,vline.color = "black",value.size=2,value.offset=.3,title="",axis.labels =c("Treatment (MPA/control)","log10(Area)"),grid=FALSE)$zero_inflated+ theme_sjplot(base_size=8)
tiff("~/MPAFlickr/userdaysmodel_final.tif",units="cm",width=20,height=10,res=200)
grid.arrange(g1,g2,nrow = 2, ncol = 1)

dev.off()
