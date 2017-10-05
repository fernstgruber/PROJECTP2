load("/home/fabs/PROJECTP2/data2017/boden_SGU.RData")
boden_SGU <- boden_SGU[boden_SGU$KLASSE != "",]
boden_SGU <- boden_SGU[boden_SGU$TYP != "",]
boden_SGU <- boden_SGU[boden_SGU$SUBTYP != "",]
boden_SGU[boden_SGU$ID=="9_U","SUBTYP"] <- "Typischer Ranker"
boden_SGU[boden_SGU$ID=="58_T","SUBTYP"] <- "Typischer Ranker"
boden_SGU[boden_SGU$ID=="33_U","SUBTYP"] <- "Typischer Ranker" 
boden_SGU[boden_SGU$ID=="22_U","SUBTYP"] <- "Typischer Ranker"
boden_SGU[boden_SGU$SUBTYP=="Ranker","SUBTYP"] <- "Typischer Ranker"
boden_SGU <- boden_SGU[-c(289),]
boden_SGU <- droplevels(boden_SGU)
summary(boden_SGU$KLASSE)
Klassen <- levels(boden_SGU$KLASSE)
Klassen
Typen <- levels(boden_SGU$TYP)
Typen
Subtypen <- levels(boden_SGU$SUBTYP)
Subtypen
summary(boden_SGU$SUBTYP)
bodenpunkte <- unique(boden_SGU$ID)
levels(boden_SGU$KLASSE)
boden_SGU$KLASSE <- factor(boden_SGU$KLASSE,levels=levels(boden_SGU$KLASSE)[c(7,6,1,3,2,5,8,4)])
summary(boden_SGU$KLASSE)
levels(boden_SGU$TYP)
boden_SGU$TYP <- factor(boden_SGU$TYP,levels=levels(boden_SGU$TYP)[c(4,3,12,7,10,11,1,9,15,6,2,16,8,13,14,5)])
summary(boden_SGU$TYP)
levels(boden_SGU$SUBTYP)
boden_SGU$SUBTYP <- factor(boden_SGU$SUBTYP,levels=levels(boden_SGU$SUBTYP)[c(3,8,7,25,18,20,14,19,16,22,17,29,2,28,24,6,1,21,26,13,11,27,15,4,9,5,10,23,12)])
summary(boden_SGU$SUBTYP)
save(boden_SGU, file="/home/fabs/PROJECTP2/data2017/boden_SGU_Oct5.RData")

###DANN TESTs
require(rgdal)
bodenpunkte <- readOGR("/home/fabs/PROJECTP2/data2017/Profilpunktemitboden.shp",layer="Profilpunktemitboden")
length(unique(bodenpunkte$ID))
length(unique(boden_SGU$ID))
bodenpunkte$ID %in% boden_SGU$ID


library(RCurl)
geolegendeng <- read.table(text=getURL("https://raw.githubusercontent.com/fernstgruber/p2/master/data2017/geolegendeng.txt"),sep="\t",header=T)
myfunctions <- getURL("https://raw.githubusercontent.com/fernstgruber/Rstuff/master/fabiansandrossitersfunctions.R", ssl.verifypeer = FALSE)
eval(parse(text = myfunctions))
require(rgdal)
bodenmittopo <- sqliteGRASS(location = "EPPAN_vhr",mapset = "paper3data",vector = "paper3data_Profilpunktemitboden_UTM")
save(bodenmittopo, file="/home/fabs/PROJECTP2/data2017/bodenmittopo_Oct5.RData")
