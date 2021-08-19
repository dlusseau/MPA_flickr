install.packages("cld2")
library(cld2)

load("~/MPAFlickr/photo_details.Rdata")
load("~/MPAFlickr/photo_details_2018.Rdata")
load("~/MPAFlickr/photo_details_2017.Rdata")

photo_details_MPA$language<-detect_language(photo_details_MPA$description)
photo_details_controlE$language<-detect_language(photo_details_controlE$description)
photo_details_controlW$language<-detect_language(photo_details_controlW$description)
photo_details_controlS$language<-detect_language(photo_details_controlS$description)
photo_details_controlN$language<-detect_language(photo_details_controlN$description)

photo_details_MPA<-subset(photo_details_MPA,description!="")
photo_details_controlE<-subset(photo_details_controlE,description!="")
photo_details_controlW<-subset(photo_details_controlW,description!="")
photo_details_controlS<-subset(photo_details_controlS,description!="")
photo_details_controlN<-subset(photo_details_controlN,description!="")

photo_details_MPA$treatment<-"MPA"
photo_details_controlE$treatment<-"control"
photo_details_controlW$treatment<-"control"
photo_details_controlS$treatment<-"control"
photo_details_controlN$treatment<-"control"

photos<-rbind(photos,photo_details_MPA,photo_details_controlE,photo_details_controlW,photo_details_controlS,photo_details_controlN)

rm(photo_details_MPA,photo_details_controlE,photo_details_controlW,photo_details_controlS,photo_details_controlN)

photos<-photos[,c(2,3,7,13,16,17,18,19,58,59,60,65,66,67)]

library(stm)
install.packages("sentimentr")
library(sentimentr)

photos.en<-photos[photos$language=="en",] #english one first

photos.en<-photos.en[is.na(photos.en$language)==FALSE,]


#######stm -text preparation


photos.en$description<-gsub("/","",photos.en$description)
photos.en$description<-gsub("http\\w+ *","",photos.en$description)
photos.en$description<-gsub("tco\\w+ *","",photos.en$description)
photos.en$description<-gsub(" tco\\w+ *","",photos.en$description)
photos.en$description<-gsub("t.co\\w+ *","",photos.en$description)

install.packages("textclean")
library(textclean)

photos.en$description<-replace_emoji(photos.en$description)
photos.en$description<-replace_emoticon(photos.en$description)
photos.en$description<-replace_html(photos.en$description)
photos.en$description<-gsub("\\NA\\>","",photos.en$description)


photos.en$treatment<-factor(photos.en$treatment)

#sent.glm<-glm(sentiment~treatment,data=photos.en)

library(sentimentr)


#######################sentiment analysis

##we get a sentiment value per sentence and take the median of sentence sentiments for the description sentiment estimate.

sentences<-sentimentr::get_sentences(photos.en$description[1:50000]) #for meory use efficiency
sentiments<-sentimentr::sentiment(sentences)

iters<-c(seq(1,length(photos.en$description),50000),length(photos.en$description))

for (i in 3:(length(iters))) {

sentences<-get_sentences(photos.en$description[iters[i-1]:(iters[i]-1)])
sentiments<-rbind(sentiments,sentimentr::sentiment(sentences))

  print(i)
  flush.console()
}


sentences<-get_sentences(photos.en$description[length(iters)])
sentiments<-rbind(sentiments,sentimentr::sentiment(sentences))

########################################################################
##### reconcile sentence sentiments to the photos


start_pos<-which(sentiments$element_id==1)
starting_pos<-array(0,dim=length(start_pos))
starting_pos[1]<-1
for (i in 2:length(start_pos)) {
if (start_pos[i]!=start_pos[i-1]+1) {starting_pos[i]<-1}
}
real_start_pos<-start_pos[starting_pos==1]

real_start_pos<-c(real_start_pos,dim(sentiments)[1])
sentiments$real_element_id<-0
for (i in 1:(length(real_start_pos)-1)) {

sentiments$real_element_id[real_start_pos[i]:(real_start_pos[i+1]-1)]<-sentiments$element_id[real_start_pos[i]:(real_start_pos[i+1]-1)]+(iters[i]-1)


}
sentiments$real_element_id[dim(sentiments)[1]]<-sentiments$element_id[dim(sentiments)[1]]+(iters[length(iters)]-1)

photos.en$sentiment<-NA
for (i in 1:(length(iters)-1)) {

photos.en$sentiment[iters[i]:(iters[i+1]-1)]<-by(sentiments$sentiment[(sentiments$real_element_id<iters[i+1]) &(sentiments$real_element_id>(iters[i]-1))],sentiments$real_element_id[(sentiments$real_element_id<iters[i+1]) &(sentiments$real_element_id>(iters[i]-1))],median)

}

tail(photos.en)
#fix the tail

photos.en$sentiment[dim(photos.en)[1]]<-by(sentiments$sentiment[(sentiments$real_element_id==dim(photos.en)[1])],sentiments$real_element_id[(sentiments$real_element_id==dim(photos.en)[1])],median)

save(photos.en)




##################################
##### stm


text<-textProcessor(photos.en$description,striphtml=TRUE,metadata=as.data.frame(photos.en[,c(1,2,4,5,6,7,10,11,12,14)]))

  
output <- prepDocuments(text$documents, text$vocab, text$meta,lower.thresh=10)
docs <- output$documents
vocab <- output$vocab
meta <-output$meta

meta$treatment<-factor(meta$treatment) # just to be sure

MPA.stm<-stm(docs, vocab,prevalence=~treatment,data=meta,K = 0,init.type = "Spectral",max.em.its=100,emtol=0.0001)

save(meta,docs,vocab, MPA.stm, file="MPAstm2019.Rdata")

prep <- estimateEffect(1:58 ~ treatment, MPA.stm,meta = meta, uncertainty = "Global")

plot(prep, covariate = "treatment", topics = c(1:58),model = MPA.stm, method = "difference", cov.value1 = "control", cov.value2 = "MPA", labeltype = "custom", custom.labels = c(1:58))

#eg
labelTopics(MPA.stm,c(38,10))








