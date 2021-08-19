##bring user level data back together

control.f<-list.files("~/MPAFlickr/user_details_control/",full.names=T)

load(control.f[1])
control.photosE$treatment<-"control"
control.photosE$orientation<-"East"
control.photosW$treatment<-"control"
control.photosW$orientation<-"West"
control.photosN$treatment<-"control"
control.photosN$orientation<-"North"
control.photosS$treatment<-"control"
control.photosS$orientation<-"South"

control_photos_all<-rbind(control.photosE,control.photosW,control.photosS,control.photosN)

for (i in 2:length(control.f)) {

load(control.f[i])

control.photosE$treatment<-"control"
control.photosE$orientation<-"East"
control.photosW$treatment<-"control"
control.photosW$orientation<-"West"
control.photosN$treatment<-"control"
control.photosN$orientation<-"North"
control.photosS$treatment<-"control"
control.photosS$orientation<-"South"

control_photos_all<-rbind(control_photos_all,rbind(control.photosE,control.photosW,control.photosS,control.photosN))


}

save(control_photos_all,file="~/MPAFlickr/allcontrolwusers.Rdata")

control_photos_all$year<-format(control_photos_all$datetaken,"%Y")

library(dplyr)
big.table<-control_photos_all %>%
group_by(owner,orientation,WDPA_PID,year) %>%
 summarize(count=n())

unfolded_control<-as.data.frame(big.table)

save(unfolded_control,file="~/MPAFlickr/TabUserControl.Rdata")

#########################################################################################################################################

mpa.f<-list.files("~/MPAFlickr/users_details/",full.names=T)

load(mpa.f[1])
MPA.photos$treatment<-"MPA"
MPA.photos$orientation<-"none"

MPA_photos_all<-MPA.photos


for (i in 2:length(mpa.f)) {

load(mpa.f[i])

MPA.photos$treatment<-"MPA"
MPA.photos$orientation<-"none"

MPA_photos_all<-rbind(MPA_photos_all,MPA.photos)

}


save(MPA_photos_all,file="~/MPAFlickr/allmpawusers.Rdata")
mpa.dup<-duplicated(MPA_photos_all$id)

load("~/MPAFlickr/allmpawusers.Rdata")
MPA_photos_all$year<-format(MPA_photos_all$datetaken,"%Y")


library(dplyr)


big.table<-MPA_photos_all %>%
group_by(owner,orientation,WDPA_PID,year) %>%
 summarize(count=n())

unfolded_mpa<-as.data.frame(big.table)

save(unfolded_mpa,file="~/MPAFlickr/TabUserMPA.Rdata")

load("~/MPAFlickr/TabUserControl.Rdata")
load("~/MPAFlickr/TabUserMPA.Rdata")
unfolded_mpa$treatment<-"MPA"
unfolded_control$treatment<-"control"

unfolded_all<-rbind(unfolded_mpa,unfolded_control)

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

user.df$user.count[i]<-length(unfolded.dt[J(user.df$WDPA_PID[i],user.df$year[i],user.df$orientation[i]),nomatch=0L]$owner)


}
tic-Sys.time()

user.df$photo.count<-0
tic<-Sys.time()
for (i in 1:dim(user.df)[1]) {

user.df$photo.count[i]<-sum(unfolded.dt[J(user.df$WDPA_PID[i],user.df$year[i],user.df$orientation[i]),nomatch=0L]$count)


}
tic-Sys.time()
save(user.df,unfolded_all,file="~/MPAFlickr/alldata_user_Feb2021.Rdata")


##########adjustment 12 Feb 2021 - took the median over control AND MPA!!
load("~/MPAFlickr/alldata_user_Feb2021.Rdata")

destinations<-unique(unfolded_all$WDPA_PID)
years<-unique(unfolded_all$year)
levels<-unique(unfolded_all$orientation)
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

user.df.trt$photo.count<-as.integer(user.df.trt$photo.count) # to deal with the few control cells with median yielding .5 values and being able to model as count data
user.df.trt$user.count<-as.integer(user.df.trt$user.count) # to deal with the few control cells with median yielding .5 values and being able to model as count data



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

user.df.trt.ready$user.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(7809)]]<-NA
user.df.trt.ready$photouser.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(7809)]]<-NA

user.df.trt.ready$user.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(10147)]]<-NA
user.df.trt.ready$photouser.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(10147)]]<-NA


load("~/MPAFlickr/photoMPA_yearlycount_all.Rdata")
photo_nb_all[photo_nb_all$WDPA_PID==photo_nb_all$WDPA_PID[10147],]

for (i in 1:10) {
user.df.trt.ready$photo.count[user.df.trt.ready$year==years[i]&user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[7809]]<-photo_nb_all$number[photo_nb_all$WDPA_PID==photo_nb_all$WDPA_PID[7809]&photo_nb_all$year==years[i]]

user.df.trt.ready$photo.count[user.df.trt.ready$year==years[i]&user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[10147]]<-photo_nb_all$number[photo_nb_all$WDPA_PID==photo_nb_all$WDPA_PID[10147]&photo_nb_all$year==years[i]]
}


save(user.df.trt.ready,file="~/MPAFlickr/alldata_user_photo_Feb2021_corrected.Rdata")

load("~/MPAFlickr/alldata_user_photo_Feb2021.Rdata")

user.df.trt$log_area<-log10(user.df.trt$area)

user.df.trt.ready<-subset(user.df.trt,year!="2007"&year!="2009")


#####we are missing data for 10147 and 7809 MPA treatment level
#MPA.sub$WDPA_PID[c(7809,10147)]
#"555542209" "332911"  

user.df.trt.ready$photo.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[7809]]

user.df.trt.ready$user.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(7809)]]<-NA
user.df.trt.ready$photouser.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(7809)]]<-NA

user.df.trt.ready$user.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(10147)]]<-NA
user.df.trt.ready$photouser.count[user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(10147)]]<-NA



load("~/MPAFlickr/photoMPA_yearlycount_all.Rdata")
photo_nb_all[photo_nb_all$WDPA_PID==photo_nb_all$WDPA_PID[10147],]

for (i in 1:10) {
user.df.trt.ready$photo.count[user.df.trt.ready$year==years[i]&user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[7809]]<-photo_nb_all$number[photo_nb_all$WDPA_PID==photo_nb_all$WDPA_PID[7809]&photo_nb_all$year==years[i]]

user.df.trt.ready$photo.count[user.df.trt.ready$year==years[i]&user.df.trt.ready$treatment=="MPA" & user.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[10147]]<-photo_nb_all$number[photo_nb_all$WDPA_PID==photo_nb_all$WDPA_PID[10147]&photo_nb_all$year==years[i]]
}

save(user.df.trt.ready,file="~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021.Rdata")

rm(photo_nb_all)

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021.Rdata")


library(glmmTMB)
library(bbmle) ## for AICtab
library(ggplot2)


zi.m34<-glmmTMB(photo.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment+log_area,
family=truncated_nbinom2)

zi.m34.2<-glmmTMB(photo.count~log_area+treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~treatment+log_area,
family=truncated_nbinom2)


zi.m34.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

user.df.trt.nona<-subset(user.df.trt.ready,WDPA_PID!="555542209"&WDPA_PID!="332911")

zi.m34gammaphotouser<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.nona,
ziformula=~treatment+log_area,
family=ziGamma(link="log"))


zi.m34tweediephotouser<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.nona,
ziformula=~log_area+treatment,
family=tweedie)

##we want photos/users so we need users

user.df.trt.photouser<-subset(user.df.trt.ready,user.count>0)

zi.m34ziGammaphotouser<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.photouser,
ziformula=~log_area+treatment,
family=ziGamma(link="log"))

m34Gammaphotouser<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.photouser,
family=Gamma(link="log"))

AICtab(m34Gammaphotouser,zi.m34ziGammaphotouser)

library(ggeffects)
ggeffect(m34gammaphotouser,,terms="treatment",type="pred")

library(sjPlot)

plot_model(zi.m34photouser,type="pred")

library(ggeffects)
ggeffect(zi.m34gammaphotouser,type="zi_random")

library(ggeffects)

plot(ggeffect(zi.m34,terms="treatment",type="zi_inflated"))

plot(ggeffect(zi.m34.user,terms="treatment",type="zi_inflated"))

save(zi.m34.user,user.df.trt.ready,file="~/MPAFlickr/zi_user_glmm.Rdata")


save(zi.m34tweediephotouser,zi.m34gammaphotouser,user.df.trt.ready,file="~/MPAFlickr/zi_photouser_glmm.Rdata")



############################################################################################################################
#############################################################################################################################
############################ user model selection


load("~/MPAFlickr/alldata_user_photo_Feb2021_corrected.Rdata")


library(glmmTMB)
library(bbmle) ## for AICtab
library(ggplot2)

zi.m34.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)


zi.m34.user.truncpois<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_poisson)

zi.m34.user.truncnb1<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom1)

zi.m34.user.nb1<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom1)

zi.m34.user.nb2<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

AIC(zi.m34.user,zi.m34.user.truncpois,zi.m34.user.truncnb1,zi.m34.user.nb1)




############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")


library(glmmTMB)
library(bbmle) ## for AICtab

zi.m1.user<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=truncated_nbinom2)

zi.m2.user<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=truncated_nbinom2)

zi.m3.user<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=truncated_nbinom2)

zi.m4.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=truncated_nbinom2)

save(AICtab(zi.m1.user,zi.m2.user,zi.m3.user,zi.m4.user),file="~/MPAFlickr/aic1to4.Rdata")

############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)
library(bbmle) ## for AICtab

zi.m5.user<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=truncated_nbinom2)

zi.m6.user<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=truncated_nbinom2)

zi.m7.user<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=truncated_nbinom2)

zi.m8.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=truncated_nbinom2)

save(AICtab(zi.m5.user,zi.m6.user,zi.m7.user,zi.m8.user),file="~/MPAFlickr/aic5to8.Rdata")


############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)
library(bbmle) ## for AICtab

zi.m9.user<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=truncated_nbinom2)

zi.m10.user<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=truncated_nbinom2)

zi.m11.user<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=truncated_nbinom2)

zi.m12.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=truncated_nbinom2)

save(AICtab(zi.m9.user,zi.m10.user,zi.m11.user,zi.m12.user),file="~/MPAFlickr/aic9to12.Rdata")



############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)
library(bbmle) ## for AICtab

zi.m13.user<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

zi.m14.user<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

zi.m15.user<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

# zi.m16.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),   ###this is zi34
# data=user.df.trt.ready,
# ziformula=~log_area+treatment,
# family=truncated_nbinom2)

save(AICtab(zi.m13.user,zi.m14.user,zi.m15.user),file="~/MPAFlickr/aic13to15.Rdata")

################################################################################################################################
################################################################################################################################


############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")


library(glmmTMB)
library(bbmle) ## for AICtab

zi.m1.userc<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~1,
family=truncated_nbinom2)

zi.m2.userc<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~1,
family=truncated_nbinom2)

zi.m3.userc<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~1,
family=truncated_nbinom2)

zi.m4.userc<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~1,
family=truncated_nbinom2)

save(AICtab(zi.m1.userc,zi.m2.userc,zi.m3.userc,zi.m4.userc),file="~/MPAFlickr/aic1to4c.Rdata")

############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)
library(bbmle) ## for AICtab

zi.m5.userc<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area,
family=truncated_nbinom2)

zi.m6.userc<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area,
family=truncated_nbinom2)

zi.m7.userc<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area,
family=truncated_nbinom2)

zi.m8.userc<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area,
family=truncated_nbinom2)

save(AICtab(zi.m5.userc,zi.m6.userc,zi.m7.userc,zi.m8.userc),file="~/MPAFlickr/aic5to8c.Rdata")


############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)
library(bbmle) ## for AICtab

zi.m9.userc<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~treatment,
family=truncated_nbinom2)

zi.m10.userc<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~treatment,
family=truncated_nbinom2)

zi.m11.userc<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~treatment,
family=truncated_nbinom2)

zi.m12.userc<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~treatment,
family=truncated_nbinom2)

save(AICtab(zi.m9.userc,zi.m10.userc,zi.m11.userc,zi.m12.userc),file="~/MPAFlickr/aic9to12c.Rdata")



############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)
library(bbmle) ## for AICtab

zi.m13.userc<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

zi.m14.userc<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

zi.m15.userc<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

 zi.m16.userc<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year),   ###this is zi34
 data=user.df.trt.ready,
 ziformula=~log_area+treatment,
 family=truncated_nbinom2)

save(AICtab(zi.m13.userc,zi.m14.userc,zi.m15.userc,zi.m16.userc),file="~/MPAFlickr/aic13to16c.Rdata")




############################################################################################################################
#############################################################################################################################
### looks like good old ZI negative binomial is a better fit than the hurdle model
############################################################################################################################
#############################################################################################################################

############################################################################################################################
#############################################################################################################################

############################################################################################################################
#############################################################################################################################
############################ user model selection



############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")


library(glmmTMB)
library(bbmle) ## for AICtab

zi.m1.user<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m2.user<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m3.user<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m4.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=nbinom2)
tab<-AIC(zi.m1.user,zi.m2.user,zi.m3.user,zi.m4.user)
save(tab,file="~/MPAFlickr/aic1to4.Rdata")

############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)
library(bbmle) ## for AICtab

zi.m5.user<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

zi.m6.user<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

zi.m7.user<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

zi.m8.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=nbinom2)
tab<-AIC(zi.m5.user,zi.m6.user,zi.m7.user,zi.m8.user)
save(tab,file="~/MPAFlickr/aic5to8.Rdata")


############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)
library(bbmle) ## for AICtab

zi.m9.user<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m10.user<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m11.user<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m12.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=nbinom2)
tab<-AIC(zi.m9.user,zi.m10.user,zi.m11.user,zi.m12.user)
save(tab,file="~/MPAFlickr/aic9to12.Rdata")



############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)
library(bbmle) ## for AICtab

zi.m13.user<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.m14.user<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.m15.user<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.m16.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),   ###this is zi34
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

tab<-AIC(zi.m13.user,zi.m14.user,zi.m15.user)
save(tab,file="~/MPAFlickr/aic13to15.Rdata")

################################################################################################################################
################################################################################################################################


############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")


library(glmmTMB)
library(bbmle) ## for AICtab

zi.m1.userc<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m2.userc<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m3.userc<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m4.userc<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~1,
family=nbinom2)
tab<-AIC(zi.m1.userc,zi.m2.userc,zi.m3.userc,zi.m4.userc)
save(tab,file="~/MPAFlickr/aic1to4c.Rdata")

############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)
library(bbmle) ## for AICtab

zi.m5.userc<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

zi.m6.userc<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

zi.m7.userc<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

zi.m8.userc<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

AIC(zi.m5.userc,zi.m6.userc,zi.m7.userc,zi.m8.userc)
save(tab,file="~/MPAFlickr/aic5to8c.Rdata")


############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)
library(bbmle) ## for AICtab

zi.m9.userc<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m10.userc<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m11.userc<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m12.userc<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~treatment,
family=nbinom2)
tab<-AIC(zi.m9.userc,zi.m10.userc,zi.m11.userc,zi.m12.userc)
save(tab,file="~/MPAFlickr/aic9to12c.Rdata")



############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_ready4analysis_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)
library(bbmle) ## for AICtab

zi.m13.userc<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.m14.userc<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.m15.userc<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

 zi.m16.userc<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year),   ###this is zi34
 data=user.df.trt.ready,
 ziformula=~log_area+treatment,
 family=nbinom2)
tab<-AIC(zi.m13.userc,zi.m14.userc,zi.m15.userc,zi.m16.userc)
save(tab,file="~/MPAFlickr/aic13to16c.Rdata")


save(zi.m34.user.nb2,file="~/MPAFlickr/userfinalmodel.Rdata")


############################################################################################################################################################
###########################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
####user days


load("~/MPAFlickr/allmpawusers.Rdata")
MPA_photos_all$days<-format(MPA_photos_all$datetaken,"%Y-%m-%d")
MPA_photos_all$year<-format(MPA_photos_all$datetaken,"%Y")

MPA_photos_all<-subset(MPA_photos_all,year!="2007"&year!="2009")


library(dplyr)


big.table<-MPA_photos_all %>%
group_by(orientation,WDPA_PID,year) %>%
 count(owner,days)

small.table<-big.table %>%
group_by(orientation,WDPA_PID,year) %>%
 summarize(userdays=n())

unfolded_mpa<-as.data.frame(small.table)

save(unfolded_mpa,file="~/MPAFlickr/TabUserDaysMPA.Rdata")

load("~/MPAFlickr/allcontrolwusers.Rdata")
control_photos_all$days<-format(control_photos_all$datetaken,"%Y-%m-%d")
control_photos_all$year<-format(control_photos_all$datetaken,"%Y")

control_photos_all<-subset(control_photos_all,year!="2007"&year!="2009")



big.table<-control_photos_all %>%
group_by(orientation,WDPA_PID,year) %>%
 count(owner,days)

small.table<-big.table %>%
group_by(orientation,WDPA_PID,year) %>%
 summarize(userdays=n())

unfolded_control<-as.data.frame(small.table)

save(unfolded_control,file="~/MPAFlickr/TabUserDaysControl.Rdata")

load("~/MPAFlickr/TabUserDaysControl.Rdata")
load("~/MPAFlickr/TabUserDaysMPA.Rdata")
unfolded_mpa$treatment<-"MPA"
unfolded_control$treatment<-"control"

unfolded_all<-rbind(unfolded_mpa,unfolded_control)

destinations<-unique(unfolded_all$WDPA_PID)
years<-unique(unfolded_all$year)
levels<-unique(unfolded_all$orientation)

userdays.df<-data.frame(WDPA_PID=rep(destinations,each=(length(years)*length(levels))),year=rep(rep(years,each=length(levels)),length(destinations)),orientation=rep(levels,length(years)*length(destinations)))

userdays.df$user.count<-0

library(data.table)
unfolded.dt<-data.table(unfolded_all)
userdays.df.dt<-data.table(userdays.df)
setkey(unfolded.dt,WDPA_PID,year,orientation)
setkey(userdays.df.dt,WDPA_PID,year,orientation)

tic<-Sys.time()
for (i in 1:dim(userdays.df)[1]) {

if(length(unfolded.dt[J(userdays.df$WDPA_PID[i],userdays.df$year[i],userdays.df$orientation[i]),nomatch=0L]$userdays)>0) {
userdays.df$user.count[i]<-unfolded.dt[J(userdays.df$WDPA_PID[i],userdays.df$year[i],userdays.df$orientation[i]),nomatch=0L]$userdays
}

}
tic-Sys.time()

save(userdays.df,unfolded_all,file="~/MPAFlickr/alldata_userdays_Feb2021.Rdata")

userdays.df.trt<-data.frame(WDPA_PID=rep(destinations,each=(length(years)*2)),year=rep(rep(years,each=2),length(destinations)),treatment=rep(c("MPA","control"),length(years)*length(destinations)))

userdays.df.trt$user.count<-0

userdays.df.trt$user.count[userdays.df.trt$treatment=="MPA"]<-userdays.df$user.count[userdays.df$orientation=="none"]

userdays.df.trt.control<-subset(userdays.df.trt,treatment=="control")

user.df.dt.c<-data.table(subset(userdays.df,orientation!="none"))
setkey(user.df.dt.c,WDPA_PID,year)

tic<-Sys.time()
for (i in 1:(dim(userdays.df.trt.control)[1])) {
userdays.df.trt.control$user.count[i]<-median(user.df.dt.c[J(userdays.df.trt.control$WDPA_PID[i],userdays.df.trt.control$year[i]),nomatch=0L]$user.count)


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

missing.continent<-unique(userdays.df.trt$country[which(is.na(userdays.df.trt$continent)==TRUE)])
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

#####we are missing data for 10147 and 7809 MPA treatment level
#MPA.sub$WDPA_PID[c(7809,10147)]
#"555542209" "332911"  

userdays.df.trt.ready$user.count[userdays.df.trt.ready$treatment=="MPA" & userdays.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(7809)]]<-NA
userdays.df.trt.ready$user.count[userdays.df.trt.ready$treatment=="MPA" & userdays.df.trt.ready$WDPA_PID==MPA.sub$WDPA_PID[c(10147)]]<-NA

userdays.df.trt.ready$user.count<-as.integer(userdays.df.trt.ready$user.count) #for a few control cell, median led to .5 values

save(userdays.df.trt.ready,file="~/MPAFlickr/alldata_userdays_photo_Feb2021.Rdata")

######################################################################################################################################################
######################################################################################################################################################

load("~/MPAFlickr/alldata_userdays_photo_Feb2021.Rdata")

library(glmmTMB)

zi.m34.userdays<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

zi.m34.userdays.truncpois<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_poisson)

zi.m34.userdays.truncnb1<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom1)

zi.m34.userdays.nb1<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom1)

zi.m34.userdays.nb2<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

AIC(zi.m34.user,zi.m34.user.truncpois,zi.m34.user.truncnb1,zi.m34.user.nb1,zi.m34.user.nb2)




############################################################################################################################
#############################################################################################################################
###12 Feb 2021 final selection

############################################################################################################################
#############################################################################################################################
############################ photo model selection


load("~/MPAFlickr/alldata_user_photo_Feb2021_corrected.Rdata")

zi.m34.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)

zi.m34.usernb<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)


library(glmmTMB)

zi.m34.user<-glmmTMB(photo.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom2)


zi.m34.user.truncpois<-glmmTMB(photo.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_poisson)

zi.m34.user.truncnb1<-glmmTMB(photo.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=truncated_nbinom1)

zi.m34.user.nb1<-glmmTMB(photo.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom1)

zi.m34.user.nb2<-glmmTMB(photo.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

AIC(zi.m34.user,zi.m34.user.truncpois,zi.m34.user.truncnb1,zi.m34.user.nb1,zi.m34.user.nb2)



############################################################################################################################
#############################################################################################################################
############################ photo/user model selection


load("~/MPAFlickr/alldata_user_photo_Feb2021_corrected.Rdata")


library(glmmTMB)

zi.m34.puser.gamma<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=ziGamma)


zi.m34.puser.tweedie<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=tweedie)

zi.m34.puser.beta<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=beta)

zi.m34.puser.bb<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=betabinomial)

AIC(zi.m34.puser,zi.m34.puser.tweedie,zi.m34.puser.beta,zi.m34.puser.bb)


#############################################################################################################################
###############################################################################################################################
##### user.days model selection . family preselection retained nbinom2
###############################################################################################################################
#################################################################################################################################
#model 16
save(zi.m34.userdays.nb2, file="~/MPAFlickr/finalmodel_userday.Rdata")

############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_userdays_photo_Feb2021.Rdata")


library(glmmTMB)

zi.m1.userdays<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m2.userdays<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m3.userdays<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m4.userdays<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m5.userdays<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

zi.m6.userdays<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

zi.m7.userdays<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

zi.m8.userdays<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

tab<-AIC(zi.m1.userdays,zi.m2.userdays,zi.m3.userdays,zi.m4.userdays,zi.m5.userdays,zi.m6.userdays,zi.m7.userdays,zi.m8.userdays)
save(tab,file="~/MPAFlickr/aic_userdays_m1to8.Rdata")


############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_userdays_photo_Feb2021.Rdata")

library(glmmTMB)

zi.m9.userdays<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m10.userdays<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m11.userdays<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m12.userdays<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m13.userdays<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.m14.userdays<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.m15.userdays<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.m16.userdays<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),   ###this is zi34
data=userdays.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

tab<-AIC(zi.m9.userdays,zi.m10.userdays,zi.m11.userdays,zi.m12.userdays,zi.m13.userdays,zi.m14.userdays,zi.m15.userdays,zi.m16.userdays)
save(tab,file="~/MPAFlickr/aic_userdays_m9to16.Rdata")

################################################################################################################################
################################################################################################################################

################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
####### photos per users
################################################################################################################################
################################################################################################################################


 load("~/MPAFlickr/alldata_user_photo_Feb2021_corrected.Rdata")


library(glmmTMB)

zi.m1.user<-glmmTMB(photouser.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=tweedie)

zi.m2.user<-glmmTMB(photouser.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=tweedie)

zi.m3.user<-glmmTMB(photouser.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=tweedie)

zi.m4.user<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=tweedie)

zi.m5.user<-glmmTMB(photouser.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=tweedie)

zi.m6.user<-glmmTMB(photouser.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=tweedie)

zi.m7.user<-glmmTMB(photouser.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=tweedie)

zi.m8.user<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=tweedie)

tab<-AIC(zi.m1.user,zi.m2.user,zi.m3.user,zi.m4.user,zi.m5.user,zi.m6.user,zi.m7.user,zi.m8.user)
save(tab,file="~/MPAFlickr/aic_photouser_m1to8.Rdata")


############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)

zi.m9.user<-glmmTMB(photouser.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=tweedie)

zi.m10.user<-glmmTMB(photouser.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=tweedie)

zi.m11.user<-glmmTMB(photouser.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=tweedie)

zi.m12.user<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=tweedie)

zi.m13.user<-glmmTMB(photouser.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=tweedie)

zi.m14.user<-glmmTMB(photouser.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=tweedie)

zi.m15.user<-glmmTMB(photouser.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=tweedie)

zi.m16.user<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),   ###this is zi34
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=tweedie)

##gamma
zi.m17.user<-glmmTMB(photouser.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
family=Gamma(link="log"))

zi.m18.user<-glmmTMB(photouser.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
family=Gamma(link="log"))

zi.m19.user<-glmmTMB(photouser.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
family=Gamma(link="log"))

zi.m20.user<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
family=Gamma(link="log"))

tab<-AIC(zi.m9.user,zi.m10.user,zi.m11.user,zi.m12.user,zi.m13.user,zi.m14.user,zi.m15.user,zi.m16.user,zi.m17.user,zi.m18.user,zi.m19.user,zi.m20.user)
save(tab,file="~/MPAFlickr/aic_photouser_m9to20.Rdata")

################################################################################################################################
################################################################################################################################
##gamma for photo count when users are present


load("~/MPAFlickr/alldata_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)

photousersubset<-subset(user.df.trt.ready,user.count>0)
photousersubset$logphotouser.count<-log10(photousersubset$photouser.count)

#but we want to retain destinations that still have both mpa and control (at least one)
dest.tab<-table(photousersubset$WDPA_PID,photousersubset$treatment)

keep.df<-data.frame(WDPA_PID=row.names(dest.tab),keep=1*(dest.tab[,1]>0)*(dest.tab[,2]>0))

finalsubset<-photousersubset[photousersubset$WDPA_PID%in%keep.df$WDPA_PID[keep.df$keep==1],]


#overrepresentation of ones

zi.m16.user<-glmmTMB(logphotouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),   ###this is zi34
data=finalsubset,
ziformula=~log_area+treatment,
family=tweedie)

zi.m16b.user<-glmmTMB(logphotouser.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),   ###this is zi34
data=photousersubset,
ziformula=~log_area+treatment,
family=ziGamma(link="log"))


zi.m17.user<-glmmTMB(photouser.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=photousersubset,
family=Gamma(link="log"))

zi.m18.user<-glmmTMB(photouser.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=photousersubset,
family=Gamma(link="log"))

zi.m19.user<-glmmTMB(photouser.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=photousersubset,
family=Gamma(link="log"))

zi.m20.user<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=photousersubset,
family=Gamma(link="log"))

zi.m20b.user<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=photousersubset,
family=beta_family)

zi.m16.user<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),   ###this is zi34
data=photousersubset,
ziformula=~log_area+treatment,
family=tweedie)

AIC(zi.m16.user,zi.m17.user,zi.m18.user,zi.m19.user,zi.m20.user)

tab<-AIC(zi.m17.user,zi.m18.user,zi.m19.user,zi.m20.user)

################################################################################################################################
################################################################################################################################

################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
####### number of users
################################################################################################################################
################################################################################################################################
#model 16
save(zi.m34.user.nb2, file="~/MPAFlickr/finalmodel_user.Rdata")
################################################################################################################################
################################################################################################################################

############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_user_photo_Feb2021_corrected.Rdata")


library(glmmTMB)

zi.m1.user<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m2.user<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m3.user<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=nbinom2)

zi.m4.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~1,
family=nbinom2)


zi.m5.user<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

zi.m6.user<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

zi.m7.user<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=nbinom2)

zi.m8.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area,
family=nbinom2)
tab<-AIC(zi.m1.user,zi.m2.user,zi.m3.user,zi.m4.user,zi.m5.user,zi.m6.user,zi.m7.user,zi.m8.user)
save(tab,file="~/MPAFlickr/aic_user_m1to8.Rdata")


############################################################################################################################
#############################################################################################################################

load("~/MPAFlickr/alldata_user_photo_Feb2021_corrected.Rdata")

library(glmmTMB)

zi.m9.user<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m10.user<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m11.user<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m12.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~treatment,
family=nbinom2)

zi.m13.user<-glmmTMB(user.count~1+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.m14.user<-glmmTMB(user.count~log_area+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.m15.user<-glmmTMB(user.count~treatment+(1|WDPA_PID)+(1|year)+(1|continent),
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

zi.m16.user<-glmmTMB(user.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),   ###this is zi34
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=nbinom2)

tab<-AIC(zi.m9.user,zi.m10.user,zi.m11.user,zi.m12.user,zi.m13.user,zi.m14.user,zi.m15.user,zi.m16.user)
save(tab,file="~/MPAFlickr/aic_user_m9to16.Rdata")

############################################################################################################################
#############################################################################################################################

save(zi.m16.userdays, file="~/MPAFlickr/bestmodel_userdays.Rdata")

zi.m16.user<-glmmTMB(photouser.count~log_area+treatment+(1|WDPA_PID)+(1|year)+(1|continent),   ###this is zi34
data=user.df.trt.ready,
ziformula=~log_area+treatment,
family=tweedie)
save(zi.m16.user, file="~/MPAFlickr/bestmodel_photouser.Rdata")

save(zi.m16.user, file="~/MPAFlickr/bestmodel_user.Rdata")



load("~/MPAFlickr/bestmodel_user.Rdata")
library(glmmTMB)
library(sjPlot)
library(tiff)

tiff("~/MPAFlickr/usermodel.tif",units="cm",width=16,height=8,res=200)
plot_model(zi.m16.user,title="Number of users",axis.labels =c("Treatment (MPA/control)","log10(Area)"))
dev.off()

load("~/MPAFlickr/bestmodel_userdays.Rdata")

tiff("~/MPAFlickr/userdaysmodel.tif",units="cm",width=16,height=8,res=200)
plot_model(zi.m16.userdays,title="Number of user.days",axis.labels =c("Treatment (MPA/control)","log10(Area)"))
dev.off()


load("~/MPAFlickr/bestmodel_photouser.Rdata")

tiff("~/MPAFlickr/photousermodel.tif",units="cm",width=16,height=8,res=200)
plot_model(zi.m16.user,title="Number of photos per user",axis.labels =c("Treatment (MPA/control)","log10(Area)"))
dev.off()




##############################################################################################################################################
################################################################################################################################################
##################################################################################################################################################
######## decoupling user days



load("~/MPAFlickr/allmpawusers.Rdata")
MPA_photos_all$days<-format(MPA_photos_all$datetaken,"%Y-%m-%d")
MPA_photos_all$year<-format(MPA_photos_all$datetaken,"%Y")

MPA_photos_all<-subset(MPA_photos_all,year!="2007"&year!="2009")


library(dplyr)


big.table<-MPA_photos_all %>%
group_by(orientation,WDPA_PID,owner) %>%
 count(days)

small.table<-big.table %>%
group_by(orientation,WDPA_PID,owner) %>%
 summarize(days=n())

unfolded_mpa<-as.data.frame(small.table)

save(unfolded_mpa,file="~/MPAFlickr/TabDaysMPA.Rdata")

load("~/MPAFlickr/allcontrolwusers.Rdata")
control_photos_all$days<-format(control_photos_all$datetaken,"%Y-%m-%d")
control_photos_all$year<-format(control_photos_all$datetaken,"%Y")

control_photos_all<-subset(control_photos_all,year!="2007"&year!="2009")



big.table<-control_photos_all %>%
group_by(orientation,WDPA_PID,owner) %>%
 count(days)

small.table<-big.table %>%
group_by(orientation,WDPA_PID,owner) %>%
 summarize(days=n())

unfolded_control<-as.data.frame(small.table)

save(unfolded_control,file="~/MPAFlickr/TabDaysControl.Rdata")

load("~/MPAFlickr/TabUserDaysControl.Rdata")
load("~/MPAFlickr/TabUserDaysMPA.Rdata")
unfolded_mpa$treatment<-"MPA"
unfolded_control$treatment<-"control"

unfolded_all<-rbind(unfolded_mpa,unfolded_control)

destinations<-unique(unfolded_control$WDPA_PID)
owners<-unique(unfolded_control$owner)

library(data.table)


library(dplyr)

control.df<-unfolded_control%>%group_by(WDPA_PID,owner) %>%summarise(days=ifelse(n()>1,median(days),days))
control.df$treatment<-"control"

controldays<-as.data.frame(control.df)
controldays$days<-as.integer(controldays$days)

days.paired.df<-rbind(unfolded_mpa[2:5],controldays)
save(days.paired.df,file="~/MPAFlickr/allDaysPaired.Rdata")

library(glmmTMB)

model.paired.days<-glmmTMB(days~treatment+(1|WDPA_PID)+(1|owner),
data=days.paired.df,
family=nbinom2)

save(model.paired.days,days.paired.df,file="~/MPAFlickr/model_allDaysPaired.Rdata")

crosstab<-table(days.paired.df$WDPA_PID,days.paired.df$owner,days.paired.df$treatment)
#too big

days.paired.df$keep<-0
users<-unique(days.paired.df$owner)
destinations<-unique(days.paired.df$WDPA_PID)

days.paired.df$keep<-0
destinations<-unique(days.paired.df$WDPA_PID)

for (j in 1:length(destinations)) {
trialtab<-table(days.paired.df$owner[days.paired.df$WDPA_PID==destinations[j]],days.paired.df$treatment[days.paired.df$WDPA_PID==destinations[j]])
if(dim(trialtab)[2]==2) {
user.keep<-names(which(trialtab[,1]*trialtab[,2]>0))
if (length(user.keep)>0) {
days.paired.df$keep[days.paired.df$WDPA_PID==destinations[j]][days.paired.df$owner[days.paired.df$WDPA_PID==destinations[j]]%in%user.keep]<-1
}
}
print(j)
flush.console()
}

days.paired.df.sub<-subset(days.paired.df,keep==1)
rm(days.paired.df)


