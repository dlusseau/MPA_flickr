#glmm total count
table(total_photos_subset$treatment,total_photos_subset$MPA_ID)

remove<-names(which(table(total_photos_subset$treatment,total_photos_subset$MPA_ID)[1,]==0))

flickr.mpa<-total_photos_subset[!(total_photos_subset$MPA_ID%in%remove),]

flickr.mpa$MPA_ID<-factor(flickr.mpa$MPA_ID,levels=unique(flickr.mpa$MPA_ID))

table(flickr.mpa$treatment,flickr.mpa$MPA_ID)

flickr.mpa$treatment<-factor(flickr.mpa$treatment)

flickr.mpa$Longitude<- as.numeric(flickr.mpa$Longitude)
flickr.mpa$Latitude<- as.numeric(flickr.mpa$Latitude)

flickr.mpa$sarea<-scale(flickr.mpa$area)
library(glmmTMB)
library(bbmle) ## for AICtab
library(ggplot2)

zi.m0<-glmmTMB(count~sarea+treatment+(1|MPA_ID)+(1|year),
data=flickr.mpa,
ziformula=~1,
family=nbinom1)
#AIC = 1151268

zi.m1<-glmmTMB(count~sarea+treatment+(1|MPA_ID)+(1|year),
data=flickr.mpa,
ziformula=~treatment,
family=nbinom1)
#AIC = 1151110

zi.m2<-glmmTMB(count~sarea+(1|MPA_ID)+(1|year),
data=flickr.mpa,
ziformula=~1,
family=nbinom1)
#AIC = 1180955

zi.m3<-glmmTMB(count~sarea+(1|MPA_ID)+(1|year),
data=flickr.mpa,
ziformula=~treatment,
family=nbinom1)
#AIC = 1157556

zi.m4<-glmmTMB(count~sarea+treatment+(1|MPA_ID),
data=flickr.mpa,
ziformula=~1,
family=nbinom1)
#AIC = 1152037

zi.m5<-glmmTMB(count~sarea+treatment+(1|MPA_ID),
data=flickr.mpa,
ziformula=~treatment,
family=nbinom1)
#AIC = 1151902

zi.m6<-glmmTMB(count~sarea+treatment+(1|MPA_ID)+(1|year)+(1|Longitude),
data=flickr.mpa,
ziformula=~1,
family=nbinom1)
#AIC = 1151161

zi.m7<-glmmTMB(count~sarea+treatment+(1|MPA_ID)+(1|year)+(1|Longitude),
data=flickr.mpa,
ziformula=~treatment,
family=nbinom1)
#AIC = 1151077

zi.m8<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|Longitude),
data=flickr.mpa,
ziformula=~1,
family=nbinom1)
#AIC = 1150213

zi.m9<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|Longitude),
data=flickr.mpa,
ziformula=~treatment,
family=nbinom1)
#did not converge

zi.m10<-glmmTMB(count~log10(sarea)+treatment+(1|MPA_ID)+(1|year),
data=flickr.mpa,
ziformula=~treatment,
family=nbinom1)
#did not converge

zi.m11<-glmmTMB(count~sarea+treatment+(Longitude|MPA_ID)+(1|year),
data=flickr.mpa,
ziformula=~treatment,
family=nbinom1)
#did not converge

zi.m12<-glmmTMB(count~sarea+treatment+(1|continent/MPA_ID)+(1|year),
data=flickr.mpa,
ziformula=~treatment,
family=nbinom1)
#did not converge

zi.m13<-glmmTMB(count~log_area+treatment+(continent|MPA_ID)+(1|year),
data=flickr.mpa,
ziformula=~1,
family=nbinom1)
#did not converge

zi.m14<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~1,
family=nbinom1)
#AIC = 1149907

zi.m15<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~treatment,
family=nbinom1)
#did not converge

zi.m16<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~log_area,
family=nbinom1)
#did not converge

zi.m17<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~log_area+treatment,
family=nbinom1)
#AIC = 1150639

zi.m18<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~log_area,
dispformula=~treatment,
family=nbinom1)
#did not converge

zi.m19<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~log_area+treatment,
dispformula=~treatment,
family=nbinom1)
#did not converge

zi.m20<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~1,
dispformula=~treatment,
family=nbinom1)
#did not converge



zi.m23<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~1,
family=truncated_nbinom1)
#did not converge

zi.m24<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~treatment,
family=truncated_nbinom1)
#did not converge

zi.m25<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~log_area,
family=truncated_nbinom1)
#did not converge

zi.m26<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~treatment+log_area,
family=truncated_nbinom1)
#AIC = 1170075

zi.m27<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~1,
family=truncated_poisson)
#AIC = 100193604

zi.m28<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~treatment,
family=truncated_poisson)
#AIC = 100165718

zi.m29<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~log_area,
family=truncated_poisson)
#AIC = 100187143

zi.m30<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~treatment+log_area,
family=truncated_poisson)
#AIC = 100158412

zi.m31<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~1,
family=truncated_nbinom2)
#AIC = 1135359

zi.m32<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~treatment,
family=truncated_nbinom2)
#AIC = 1107473

zi.m33<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~log_area,
family=truncated_nbinom2)
#AIC = 1128897

zi.m34<-glmmTMB(count~log_area+treatment+(1|MPA_ID)+(1|year)+(1|continent),
data=flickr.mpa,
ziformula=~treatment+log_area,
family=truncated_nbinom2)
#AIC = 1100167



save(zi.m34, file="~/MPAFlickr/final_model2.Rdata")

library(DHARMa)
testDispersion(zi.m34)
zi.m34_simres <- simulateResiduals(zi.m34)
#residuals(zi.m34_simres)
tiff(filename = "resid_m34.tif",width = 21, height = 10, units = "cm", res = 300)
plot.DHARMa(zi.m34_simres) #normality good (qq)
			   #heterogeneity of variance 
dev.off()

plotResiduals(zi.m34_simres, flickr.mpa$treatment, asFactor=T)
plotResiduals(zi.m34_simres, flickr.mpa$log_area, quantreg = T, xlab="Log area (m^2)", ylab="Residuals")
plotResiduals(zi.m34_simres, flickr.mpa$continent, asFactor=T)

disp.sim<-recalculateResiduals(zi.m34_simres, group=flickr.mpa$treatment)
testDispersion(zi.m34_simres)





library(effects)
mpa.ef<-allEffects(zi.m34)
plot(mpa.ef)
plot(mpa.ef, type="response")

ggplot(flickr.mpa,aes(x=sarea,y=count))+
geom_smooth()


ggplot(flickr.mpa,aes(x=Longitude,y=count))+
geom_point(aes(x=Longitude, y=count), groups=flickr.mpa$year)+
geom_smooth()

ggplot(data=flickr.mpa,
       aes(x=sarea, y=count, colour=treatment)) +
       geom_smooth()

ggplot(means, aes(x=year, y=mean, fill=treatment))+
geom_bar(stat="identity", position="dodge")

ggplot(means, aes(x=year,y=sum, colour=treatment))+
geom_point()+
geom_line()+
xlab("Year")+
ylab("Total photos posted")+
scale_x_continuous(breaks=2010:2019)




2010		2011		2012		2013		2014		2015		2016		2017		2018		2019
12462861	11707429	12775298	15246305	12672277	12588732	13028382	14226482	11442004	10068867
4758530		6011847		6666127		7541974		6976695		6479561		5326031		4898568		4128181		3524205
7704331		5695582		6109171		7704331		5695582		6109171		7702351		9327914		7313823		6544662