#####################likes and comments

###135 million observations, way too big for anything to maintain random effects (we need to work on that, GPU deployment of BFGS)

load("~/MPAFlickr/allphotoswusersViewLike.Rdata")
#so taking a bootstrapping approach

boot<-100

boot.n<-sample(1:dim(allphotos)[1],100000,replace=FALSE)
boot.glmm<-glmmTMB(count_faves~treatment+(1|WDPA_PID)+(1|owner)+offset(log(count_views+1)),data=allphotos[boot.n,],family=nbinom2)
boot.coef<-summary(boot.glmm)$coefficients$cond

boot<-1000
for (i in 1:boot) {

boot.n<-sample(1:dim(allphotos)[1],100000,replace=FALSE)
boot.glmm<-glmmTMB(count_faves~treatment+(1|WDPA_PID)+(1|owner)+offset(log(count_views+1)),data=allphotos[boot.n,],family=nbinom2)
boot.coef<-rbind(boot.coef,summary(boot.glmm)$coefficients$cond)

print(i)
flush.console()

}

save(boot.coef,file="~/MPAFlickr/bootglmmFaves.Rdata")

#####################################################################################
##### comments

load("~/MPAFlickr/allphotoswusersViewLike.Rdata")
#so taking a bootstrapping approach
library(glmmTMB)

boot<-100

boot.n<-sample(1:dim(allphotos)[1],100000,replace=FALSE)
boot.glmm<-glmmTMB(count_comments~treatment+(1|WDPA_PID)+(1|owner)+offset(log(count_views+1)),data=allphotos[boot.n,],family=nbinom2)
boot.coef.com<-summary(boot.glmm)$coefficients$cond

boot<-1000
for (i in 1:boot) {

boot.n<-sample(1:dim(allphotos)[1],100000,replace=FALSE)
boot.glmm<-glmmTMB(count_comments~treatment+(1|WDPA_PID)+(1|owner)+offset(log(count_views+1)),data=allphotos[boot.n,],family=nbinom2)
boot.coef.com<-rbind(boot.coef.com,summary(boot.glmm)$coefficients$cond)

print(i)
flush.console()

}

save(boot.coef.com,file="~/MPAFlickr/bootglmmComments.Rdata")

#mean offset  mean(log(allphotos$count_views+1)) 4.401086

hist(boot.coef.com[row.names(boot.coef.com)=="treatmentMPA",1])
hist(boot.coef.com[row.names(boot.coef.com)=="(Intercept)",1])
hist(log10(boot.coef.com[row.names(boot.coef.com)=="treatmentMPA",4]))
sum(log10(boot.coef.com[row.names(boot.coef.com)=="treatmentMPA",4])>-1.3)



hist(boot.coef[row.names(boot.coef)=="treatmentMPA",1])
hist(boot.coef[row.names(boot.coef)=="(Intercept)",1])
hist(log10(boot.coef[row.names(boot.coef)=="treatmentMPA",4]))

#mean comment control
MPA.b<-mean(boot.coef.com[row.names(boot.coef.com)=="treatmentMPA",1])
inter.b<-mean(boot.coef.com[row.names(boot.coef.com)=="(Intercept)",1])
offset.m<-mean(log(allphotos$count_views+1))
offset.med<-median(log(allphotos$count_views+1))
offset.max<-max(log(allphotos$count_views+1))

exp(inter.b+offset.m)

#mean comment MPA
exp(inter.b+MPA.b+offset.m)
exp(inter.b+MPA.b+offset.max)/exp(inter.b+offset.max)


#mean like control
MPA.b<-mean(boot.coef[row.names(boot.coef)=="treatmentMPA",1])
inter.b<-mean(boot.coef[row.names(boot.coef)=="(Intercept)",1])
offset.m<-mean(log(allphotos$count_views+1))
offset.med<-median(log(allphotos$count_views+1))
offset.max<-max(log(allphotos$count_views+1))

exp(inter.b+offset.m)

#mean like MPA
exp(inter.b+MPA.b+offset.m)
exp(inter.b+MPA.b+offset.max)/exp(inter.b+offset.max)


load("~/MPAFlickr/bootglmmComments.Rdata")
boot.coef.com<-as.data.frame(boot.coef.com)
boot.coef.com$ratio<-0
boot.iter<-seq(1,1093,2)

for (i in boot.iter) {

boot.coef.com$ratio[i]<-exp(boot.coef.com[i,1]+boot.coef.com[i+1,1])/exp(boot.coef.com[i,1])
boot.coef.com$ratio[i+1]<-boot.coef.com$ratio[i]

}
hist(boot.coef.com$ratio[boot.iter])

#######
boot.coef<-as.data.frame(boot.coef)
boot.coef$ratio<-0
boot.iter<-seq(1,1203,2)

for (i in boot.iter) {

boot.coef$ratio[i]<-exp(boot.coef[i,1]+boot.coef[i+1,1])/exp(boot.coef[i,1])
boot.coef$ratio[i+1]<-boot.coef$ratio[i]

}
hist(boot.coef$ratio[boot.iter])

#########################################################
boot.coef<-boot.coef.com
boot.coef<-as.data.frame(boot.coef)
boot.coef$ratio<-0
boot.iter<-seq(1,1093,2)

for (i in boot.iter) {

boot.coef$ratio[i]<-exp(boot.coef[i,1]+boot.coef[i+1,1])/exp(boot.coef[i,1])
boot.coef$ratio[i+1]<-boot.coef$ratio[i]

}
hist(boot.coef$ratio[boot.iter])


