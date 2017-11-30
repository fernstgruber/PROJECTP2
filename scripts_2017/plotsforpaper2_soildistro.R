require(e1071)
require(RCurl)
require(repmis)
require(randomForest)
library(knitr)
#proj2path="/home/fabs/PROJECTP2/"
#paper2datapath ="/home/fabs/Data/paper2data/"
proj2path="/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/"
paper2datapath ="/media/fabs/Volume/01_PAPERZEUG/paper2data/"
load(paste(proj2path,"data2017/boden_SGU_Oct5.RData",sep=""))
load(paste(proj2path,"data2017/pointIDs.RData",sep=""))
myfunctions <- getURL("https://raw.githubusercontent.com/fernstgruber/Rstuff/master/fabiansandrossitersfunctions.R", ssl.verifypeer = FALSE)
eval(parse(text = myfunctions))
legend <- read.table(paste(proj2path,"data2017/SGU_legend_new.txt",sep=""),sep="\t",header=T)
load(paste(proj2path,"data2017/modeldata_sGUkartiert.RData",sep=""))
modeldataoktober$SGU_gk <- factor(modeldataoktober$SGU_gk,levels=levels(legend$SGU))
modeldataoktober$SGU_kartiert <- factor(modeldataoktober$SGU_kartiert,levels=levels(legend$SGU))
load(paste(proj2path,"data2017/pointIDs.RData",sep=""))
#svg("/home/fabs/PROJECTP2/figure/studyarea_statistics/barplot_bodentypen.svg")
barplot(table(boden_SGU$TYP),las=2,cex.names=0.5,ylim=c(0,200))
#dev.off()

bodendata <-boden_SGU[boden_SGU$ID %in% modelpointIDs,]
SB_geo<- bodendata[bodendata$SGU_gk == "SB",]
SB_geo <- droplevels(SB_geo)
SB_kart <- bodendata[bodendata$SGU_kartiert == "SB",]
SB_kart <- droplevels(SB_kart)
SB_beides <- bodendata[(bodendata$SGU_gk == "SB" & bodendata$SGU_kartiert == "SB"),]
SB_beides <- droplevels(SB_beides)
svg(paste(proj2path,"/figure/studyarea_statistics/bodentypen_SB_geo.svg",sep=""))
barplot(table(SB_geo$TYP),las=2,cex.names=0.5,ylim=c(0,40))
dev.off()
svg(paste(proj2path,"/figure/studyarea_statistics/bodentypen_SB_kart.svg",sep=""))
barplot(table(SB_kart$TYP),las=2,cex.names=0.5,ylim=c(0,40))
dev.off()
svg(paste(proj2path,"/figure/studyarea_statistics/bodentypen_SB_beides.svg",sep=""))
barplot(table(SB_beides$TYP),las=2,cex.names=0.5,ylim=c(0,40))
dev.off()
TG_geo<- bodendata[bodendata$SGU_gk == "TG",]
TG_geo <- droplevels(TG_geo)
TG_kart <- bodendata[bodendata$SGU_kartiert == "TG",]
TG_kart <- droplevels(TG_kart)
TG_beides <- bodendata[(bodendata$SGU_gk == "TG" & bodendata$SGU_kartiert == "TG"),]
TG_beides <- droplevels(TG_beides)
svg(paste(proj2path,"/figure/studyarea_statistics/bodentypen_TG_geo.svg",sep=""))
barplot(table(TG_geo$TYP),las=2,cex.names=0.5,ylim=c(0,110))
dev.off()
svg(paste(proj2path,"/figure/studyarea_statistics/bodentypen_TG_kart.svg",sep=""))
barplot(table(TG_kart$TYP),las=2,cex.names=0.5,ylim=c(0,110))
dev.off()
svg(paste(proj2path,"/figure/studyarea_statistics/bodentypen_TG_beides.svg",sep=""))
barplot(table(TG_beides$TYP),las=2,cex.names=0.5,ylim=c(0,110))
dev.off()
SD_geo<- bodendata[bodendata$SGU_gk == "SD",]
SD_geo <- droplevels(SD_geo)
SD_kart <- bodendata[bodendata$SGU_kartiert == "SD",]
SD_kart <- droplevels(SD_kart)
SD_beides <- bodendata[(bodendata$SGU_gk == "SD" & bodendata$SGU_kartiert == "SD"),]
SD_beides <- droplevels(SD_beides)
svg(paste(proj2path,"/figure/studyarea_statistics/bodentypen_SD_geo.svg",sep=""))
barplot(table(SD_geo$TYP),las=2,cex.names=0.5,ylim=c(0,45))
dev.off()
svg(paste(proj2path,"/figure/studyarea_statistics/bodentypen_SD_kart.svg",sep=""))
barplot(table(SD_kart$TYP),las=2,cex.names=0.5,ylim=c(0,45))
dev.off()
svg(paste(proj2path,"/figure/studyarea_statistics/bodentypen_SD_beides.svg",sep=""))
barplot(table(SD_beides$TYP),las=2,cex.names=0.5,ylim=c(0,45))
dev.off()
AD_geo<- bodendata[bodendata$SGU_gk == "AD",]
AD_geo <- droplevels(AD_geo)
AD_kart <- bodendata[bodendata$SGU_kartiert == "AD",]
AD_kart <- droplevels(AD_kart)
AD_beides <- bodendata[(bodendata$SGU_gk == "AD" & bodendata$SGU_kartiert == "AD"),]
AD_beides <- droplevels(AD_beides)
svg(paste(proj2path,"/figure/studyarea_statistics/bodentypen_AD_geo.svg",sep=""))
barplot(table(AD_geo$TYP),las=2,cex.names=0.5,ylim=c(0,10))
dev.off()
svg(paste(proj2path,"/figure/studyarea_statistics/bodentypen_AD_kart.svg",sep=""))
barplot(table(AD_kart$TYP),las=2,cex.names=0.5,ylim=c(0,10))
dev.off()
svg(paste(proj2path,"/figure/studyarea_statistics/bodentypen_AD_beides.svg",sep=""))
barplot(table(AD_beides$TYP),las=2,cex.names=0.5,ylim=c(0,10))
dev.off()
