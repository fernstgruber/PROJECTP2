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

#svg("/home/fabs/PROJECTP2/figure/studyarea_statistics/barplot_bodentypen.svg")
barplot(table(boden_SGU$TYP),las=2,cex.names=0.5,ylim=c(0,200))
#dev.off()


########
#Welche Punkte werden verwendet?
######
parameter = "TRI_hr_ws48"
ylim=c(0,100)

cl1="TG"
dependent="correct"
relevantmodeldata <- modeldataoktober[(modeldataoktober$SGU_kartiert == as.character(cl1)),]
relevantmodeldata$correct <- ifelse(relevantmodeldata$SGU_kartiert == relevantmodeldata$SGU_gk,1,0)
relevantmodeldata$correct<-as.factor(relevantmodeldata$correct)
#svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_TG_internal_TRI48.svg",sep=""))
boxplot(data=relevantmodeldata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
#dev.off()

cl1="SB"
dependent="correct"
relevantmodeldata <- modeldataoktober[(modeldataoktober$SGU_kartiert == as.character(cl1)),]
relevantmodeldata$correct <- ifelse(relevantmodeldata$SGU_kartiert == relevantmodeldata$SGU_gk,1,0)
relevantmodeldata$correct<-as.factor(relevantmodeldata$correct)
#svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_SB_internal_TRI48.svg",sep=""))
boxplot(data=relevantmodeldata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
#dev.off()

cl1="SD"
dependent="correct"
relevantmodeldata <- modeldataoktober[(modeldataoktober$SGU_kartiert == as.character(cl1)),]
relevantmodeldata$correct <- ifelse(relevantmodeldata$SGU_kartiert == relevantmodeldata$SGU_gk,1,0)
relevantmodeldata$correct<-as.factor(relevantmodeldata$correct)
#svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_SD_internal_TRI48.svg",sep=""))
boxplot(data=relevantmodeldata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
#dev.off()



dependent="SGU_kartiert"
classes <- c("TG","SB","SD")
classdata <- modeldataoktober[modeldataoktober$SGU_kartiert %in% classes,c(dependent,parameter)]
classdata <- droplevels(classdata)
summary(classdata)
#svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_TG_SB_SD_kartiert_TRI48.svg",sep=""))
boxplot(data=classdata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
#dev.off()

dependent="SGU"
north <- sqliteGRASS_delilah(location="EPPAN_vhr",mapset = "p2_samplevectors",vector = "dtm_hr_TRI_100m_grid_SGU_north")
south <- sqliteGRASS_delilah(location="EPPAN_vhr",mapset = "p2_samplevectors",vector = "dtm_hr_TRI_100m_grid_SGU_south")
mapdata <- rbind(north,south)
rm(north,south)
names(mapdata)
summary(mapdata)
mapdata$SGU <- as.factor(mapdata$geolegen_1)
mapclassdata <- mapdata[mapdata$SGU %in% classes,c(dependent,parameter)]
mapclassdata <-droplevels(mapclassdata)
#svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_TG_SB_SD_geol_TRI48.svg",sep=""))
boxplot(data=mapclassdata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
#dev.off()
####################################################
######
parameter = "vectorruggedness_hr_ws43"
ylim=c(0,0.22)

cl1="TG"
dependent="correct"
relevantmodeldata <- modeldataoktober[(modeldataoktober$SGU_kartiert == as.character(cl1)),]
relevantmodeldata$correct <- ifelse(relevantmodeldata$SGU_kartiert == relevantmodeldata$SGU_gk,1,0)
relevantmodeldata$correct<-as.factor(relevantmodeldata$correct)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_TG_internal_VR43.svg",sep=""))
boxplot(data=relevantmodeldata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()

cl1="SB"
dependent="correct"
relevantmodeldata <- modeldataoktober[(modeldataoktober$SGU_kartiert == as.character(cl1)),]
relevantmodeldata$correct <- ifelse(relevantmodeldata$SGU_kartiert == relevantmodeldata$SGU_gk,1,0)
relevantmodeldata$correct<-as.factor(relevantmodeldata$correct)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_SB_internal_VR43.svg",sep=""))
boxplot(data=relevantmodeldata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()

cl1="SD"
dependent="correct"
relevantmodeldata <- modeldataoktober[(modeldataoktober$SGU_kartiert == as.character(cl1)),]
relevantmodeldata$correct <- ifelse(relevantmodeldata$SGU_kartiert == relevantmodeldata$SGU_gk,1,0)
relevantmodeldata$correct<-as.factor(relevantmodeldata$correct)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_SD_internal_VR43.svg",sep=""))
boxplot(data=relevantmodeldata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()



dependent="SGU_kartiert"
classes <- c("TG","SB","SD")
classdata <- modeldataoktober[modeldataoktober$SGU_kartiert %in% classes,c(dependent,parameter)]
classdata <- droplevels(classdata)
summary(classdata)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_TG_SB_SD_kartiert_VR43.svg",sep=""))
boxplot(data=classdata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()

dependent="SGU"
north <- sqliteGRASS_delilah(location="EPPAN_vhr",mapset = "p2_samplevectors",vector = "dtm_hr_vectorruggedness_100m_grid_SGU_north")
south <- sqliteGRASS_delilah(location="EPPAN_vhr",mapset = "p2_samplevectors",vector = "dtm_hr_vectorruggedness_100m_grid_SGU_south")
mapdata <- rbind(north,south)
rm(north,south)
names(mapdata)
summary(mapdata)
mapdata$SGU <- as.factor(mapdata$geolegen_1)
mapclassdata <- mapdata[mapdata$SGU %in% classes,c(dependent,parameter)]
mapclassdata <-droplevels(mapclassdata)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_TG_SB_SD_geol_VR43.svg",sep=""))
boxplot(data=mapclassdata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()


####################################################
######
parameter = "vectorruggedness_hr_ws57"
ylim=c(0,0.2)

cl1="TG"
dependent="correct"
relevantmodeldata <- modeldataoktober[(modeldataoktober$SGU_kartiert == as.character(cl1)),]
relevantmodeldata$correct <- ifelse(relevantmodeldata$SGU_kartiert == relevantmodeldata$SGU_gk,1,0)
relevantmodeldata$correct<-as.factor(relevantmodeldata$correct)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_TG_internal_VR57.svg",sep=""))
boxplot(data=relevantmodeldata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()

cl1="SB"
dependent="correct"
relevantmodeldata <- modeldataoktober[(modeldataoktober$SGU_kartiert == as.character(cl1)),]
relevantmodeldata$correct <- ifelse(relevantmodeldata$SGU_kartiert == relevantmodeldata$SGU_gk,1,0)
relevantmodeldata$correct<-as.factor(relevantmodeldata$correct)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_SB_internal_VR57.svg",sep=""))
boxplot(data=relevantmodeldata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()

cl1="SD"
dependent="correct"
relevantmodeldata <- modeldataoktober[(modeldataoktober$SGU_kartiert == as.character(cl1)),]
relevantmodeldata$correct <- ifelse(relevantmodeldata$SGU_kartiert == relevantmodeldata$SGU_gk,1,0)
relevantmodeldata$correct<-as.factor(relevantmodeldata$correct)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_SD_internal_VR57.svg",sep=""))
boxplot(data=relevantmodeldata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()



dependent="SGU_kartiert"
classes <- c("TG","SB","SD")
classdata <- modeldataoktober[modeldataoktober$SGU_kartiert %in% classes,c(dependent,parameter)]
classdata <- droplevels(classdata)
summary(classdata)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_TG_SB_SD_kartiert_VR57.svg",sep=""))
boxplot(data=classdata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()

dependent="SGU"
north <- sqliteGRASS_delilah(location="EPPAN_vhr",mapset = "p2_samplevectors",vector = "dtm_hr_vectorruggedness_100m_grid_SGU_north")
south <- sqliteGRASS_delilah(location="EPPAN_vhr",mapset = "p2_samplevectors",vector = "dtm_hr_vectorruggedness_100m_grid_SGU_south")
mapdata <- rbind(north,south)
rm(north,south)
names(mapdata)
summary(mapdata)
mapdata$SGU <- as.factor(mapdata$geolegen_1)
mapclassdata <- mapdata[mapdata$SGU %in% classes,c(dependent,parameter)]
mapclassdata <-droplevels(mapclassdata)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_TG_SB_SD_geol_VR57.svg",sep=""))
boxplot(data=mapclassdata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()


####################################################
######
ylim=c(0,120)

cl1="TG"
dependent="correct"
relevantmodeldata <- modeldataoktober[(modeldataoktober$SGU_kartiert == as.character(cl1)),]
relevantmodeldata$correct <- ifelse(relevantmodeldata$SGU_kartiert == relevantmodeldata$SGU_gk,1,0)
relevantmodeldata$correct<-as.factor(relevantmodeldata$correct)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_TG_internal_VR57.svg",sep=""))
boxplot(data=relevantmodeldata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()

cl1="SB"
dependent="correct"
relevantmodeldata <- modeldataoktober[(modeldataoktober$SGU_kartiert == as.character(cl1)),]
relevantmodeldata$correct <- ifelse(relevantmodeldata$SGU_kartiert == relevantmodeldata$SGU_gk,1,0)
relevantmodeldata$correct<-as.factor(relevantmodeldata$correct)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_SB_internal_VR57.svg",sep=""))
boxplot(data=relevantmodeldata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()

cl1="SD"
dependent="correct"
relevantmodeldata <- modeldataoktober[(modeldataoktober$SGU_kartiert == as.character(cl1)),]
relevantmodeldata$correct <- ifelse(relevantmodeldata$SGU_kartiert == relevantmodeldata$SGU_gk,1,0)
relevantmodeldata$correct<-as.factor(relevantmodeldata$correct)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_SD_internal_VR57.svg",sep=""))
boxplot(data=relevantmodeldata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()



dependent="SGU_kartiert"
classes <- c("TG","SB","SD")
classdata <- modeldataoktober[modeldataoktober$SGU_kartiert %in% classes,c(dependent,parameter)]
classdata <- droplevels(classdata)
summary(classdata)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_TG_SB_SD_kartiert_VR57.svg",sep=""))
boxplot(data=classdata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()

dependent="SGU"
north <- sqliteGRASS_delilah(location="EPPAN_vhr",mapset = "p2_samplevectors",vector = "dtm_hr_TRI_100m_grid_SGU_north")
south <- sqliteGRASS_delilah(location="EPPAN_vhr",mapset = "p2_samplevectors",vector = "dtm_hr_TRI_100m_grid_SGU_south")
mapdata <- rbind(north,south)
rm(north,south)
names(mapdata)
summary(mapdata)
mapdata$SGU <- as.factor(mapdata$geolegen_1)
mapclassdata <- mapdata[mapdata$SGU %in% classes,c(dependent,parameter)]
mapclassdata <-droplevels(mapclassdata)
svg(paste(proj2path,"/figure/studyarea_statistics/boxplot_TG_SB_SD_geol_VR57.svg",sep=""))
boxplot(data=mapclassdata[c(as.character(parameter),dependent)],as.formula(paste(as.character(parameter),"~ ", dependent,sep=" ")),outline=T,main=as.character(parameter),ylim=ylim)  
dev.off()