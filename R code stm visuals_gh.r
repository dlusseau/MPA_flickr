######################
### R code to generate  visuals for the MPA flickr STM

install.packages("LDAvis")
library(LDAvis)
load("~/MPAFlickr/MPAstm2020_clean.Rdata")
library(stm)

prep <- estimateEffect(1:65 ~ treatment, MPA.stm,meta = meta, uncertainty = "Global")

plot(prep, covariate = "treatment", topics = c(1:65),model = MPA.stm, method = "difference", cov.value1 = "control", cov.value2 = "MPA", labeltype = "custom", custom.labels = c(1:67), xlab="Control-MPA")

try<-plot(prep, covariate = "treatment", topics = c(1:65),model = MPA.stm, method = "difference", cov.value1 = "control", cov.value2 = "MPA")

prevalence.p<-unlist(try$means)

save(prevalence.p,file="~/MPAFlickr/stm_prevalence_prob.Rdata")

MPA.stm.lda<-toLDAvis(mod=MPA.stm, docs=docs)

findTopic(MPA.stm,"seagrass")


word.doc.prob<-exp(MPA.stm$beta$logbeta[[1]])


topic.specificity<-apply(word.doc.prob,2,which.max)
#find which topic in which each word is most probable

topics.ns<-c(4,8,17,22,26,32,43,53,65)  #from prep adjusted (Bonferroni p values for multiple testing: 0.05/65 = 0.0008)

topics.retain<-c(1:65)
topics.vocab<-as.character(topics.retain)
topics.retain<-topics.retain[-topics.ns]

for (i in topics.retain) {

topics.vocab[i]<-paste(vocab[which(topic.specificity==i)][sort(word.doc.prob[i,which(topic.specificity==i)],decreasing=TRUE,index.return=TRUE)$ix][1:5],collapse=",")

#sort the vocab of word assigned to topic by how likely they are in the topic

}

vocab.marinecesasset.curated<-c(51242,50799,50485,50097,50099,50021,50017,49820,49736,49709,49375,49364,49359,49349,908,914,1404,1406,1488,1552,1564,1599,1944,2272,3229,3553,3802,3884,3887,4583,4597,4609,4674,9958,9957,9108,9021,8932,7933,7878,7211,11403,11243,11244,10933,10264,16496,16467,16428,16367,15101,19993,19623,19920,19592,19413,17387,17334,21521,24251,24252,23079,27998,27929,27331,29926,31263,31084,29928,34378,34371,34310,33714,32416,38859,37803,36676,36674,41152,41107,40586,40524,40517,40506,40501,40499,40495,40492,39965,39818,39813,39802,39724,39607,45274,44504,44174,44164,42309,47905,47526,47041,47035,46997,46830,45725,49197)
vocab.marinecesasset.curated<-unique(c(vocab.marinecesasset.curated,match(nature,vocab)))
vocab.built.curated<-c(50419,50136,50106,49767,49747,49254,23,681,1486,1948,1953,2052,2362,2363,2610,2823,2915,3627,4257,4662,5017,5745,9789,9772,9214,8561,7817,7506,6939,6938,6196,12800,12247,11564,11565,11039,10826,16903,16893,16873,15539,13269,19555,19527,18841,17852,21618,21555,20127,28196,26334,31617,30335,29505,38759,37262,37249,35815,41917,41275,40548,40268,39602,44833,44470,43258,42745,45427)
vocab.built.curated<-unique(c(vocab.built.curated,match(infrastructure,vocab) ))
vocab.marineactivities.curated<-c(51209,50684,50683,50430,50125,49740,49711,333,450,12480,12477,12493,16378,15395,24699,24172,23526,44467,44121)



topic.nature<-apply(word.doc.prob[,vocab.marinecesasset.curated],2,which.max)
topic.built<-apply(word.doc.prob[,vocab.built.curated],2,which.max)
topic.activities<-apply(word.doc.prob[,vocab.marineactivities.curated],2,which.max)

topic.words.nature<-rep("",65)
topic.words.built<-rep("",65)
topic.words.activities<-rep("",65)

for (i in 1:65) {
if (length(which(topic.activities==i))>0) {
topic.words.activities[i]<-paste(vocab[vocab.marineactivities.curated[which(topic.activities==i)]],collapse=",")
}
}

for (i in 1:65) {
if (length(which(topic.nature==i))>0) {
topic.words.nature[i]<-paste(vocab[vocab.marinecesasset.curated[which(topic.nature==i)]],collapse=",")
}

if (length(which(topic.built==i))>0) {
topic.words.built[i]<-paste(vocab[vocab.built.curated[which(topic.built==i)]],collapse=",")
}

}


topics.labels.built<-c(
"",
"",
"",
"",
"theater",
"lighthouse",
"",
"",
"dinghy,harbour",
"aquarium,gondola,seaport",
"city",
"warehouse,architecture,building,skyscraper",
"containership,port",
"road",
"brickhouse,campfire,nightclub",
"",
"aqueduct",
"footbridge",
"agriculture,motorboat,town",
"restaurant",
"boardwalk,swimmingpool",
"",
"abbay,basilica,fountain",
"anchorage,campground,car",
"artist",
"",
"",
"",
"",
"castle,guardhouse",
"workboat,windfarm,berth,deck,ship,sail,tanker",
"atelier",
"",
"",
"cemetery,minaret,school,street",
"",
"",
"",
"",
"facilities",
"",
"",
"",
"church,dormitory",
"garden,hovercraft",
"",
"",
"",
"dumpster",
"deckchair,guesthouse,speedboat",
"",
"wharf,cyclepath",
"artisan",
"railroad",
"rail,station,train,bus",
"",
"winery",
"wheelchair",
"footpath,hotel,marina",
"bistro,coffe,cuisine",
"plane",
"",
"",
"",
""
)

topics.labels.nature<-c(
"",
"",
"",
"bioluminescence,birdsong",
"",
"",
"",
"",
"fish",
"wrass,anemone,barracuda,corallin,grouper,seagrass,sea,sailfish,urchin,turtle",
"",
"current",
"",
"",
"turbot",
"estuary,lungfish",
"",
"horseshoe crab,seacliff,turquoise",
"flamingo",
"",
"beach,coral,mangrove,ocean,sand",
"",
"whale,cetacean",
"wildlife,waterfall",
"cephalopod,undersea",
"dolphin",
"beauty,kelp,swordfish,sunshine",
"",
"",
"",
"",
"",
"",
"",
"",
"peace,peaceful",
"",
"wildfowl,wetland,waterfowl,amphibian,bittern,flatfish,mammal,seal,seabird,saltmarsh",
"",
"",
"zooplankton,whirlpool,waterbird,albatross,bird,gull,mollusk,seaduck,sandeel,tern",
"fjord",
"beautiful,panorama",
"",
"waterlily,amphipod,aquamarine,frigatebird,pelican",
"sardine",
"",
"nature,seaview",
"",
"angelfish,kelpfish,island,rockfish,reef,puffin,pufferfish,shark,tidepool,walrus",
"cardinalfish",
"windblown",
"seabreeze",
"",
"",
"clownfish,wild",
"penguin",
"wind,cove,halibut,sunrise",
"coastline,clifftop,peninsula",
"anchovy,aroma",
"",
"",
"",
"",
""
)


topics.labels<-data.frame(topic=1:65,nature=topics.labels.nature,built=topics.labels.built)
write.csv(topics.labels,file="~/MPAFlickr/topics_labels.csv")


library(tiff)
tiff(file="~/MPAFlickr/STM_plot.tiff",pointsize=6,width=20, height=14, res=300, units="cm")
plot(prep, xlim=c(-0.015,0.025),covariate = "treatment", topics = c(1:65),model = MPA.stm, method = "difference", cov.value1 = "control", cov.value2 = "MPA", labeltype = "custom", custom.labels = c(1:4,"theater","lighthouse","airport","diving","fish","seagrass,sea,turtle","city","architecture, building","port","road","campfire, nightclub","estuary","aqueduct","seacliff","town","restaurant","beach, mangrove, coral", 22, "whale", "campground", "artist, undersea", "dolphin","beauty, kelp", 28, 29, "castle", "fishing, windfarm, sail", 32, 33, 34, "street, minaret", "peace, peaceful", 37, "seal, saltmarsh, wetland", 39, 40, "gull, mollusc, bird", "fjord","beautiful, panorama", "church", "garden, pelican","sardine", 47, "nature, seaview", "dumpster", "shark, reef", "cardinalfish", "windblown, cyclepath","seabreeze","railroad", "rail, station, train, bus", "wild", "winery", "wind, cove", "coastline, peninsula", "anchovy, aroma, bistro","plane", 62:65), xlab="Control-MPA")
dev.off()
