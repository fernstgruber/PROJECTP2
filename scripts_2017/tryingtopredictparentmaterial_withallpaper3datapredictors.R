#ACHTUNG sind die SGU_predcodes eh Faktoren??

require(e1071)
require(randomForest)
require(rgdal)
require(rgrass7)

##predictors <- c("SGUcode","TRI_hr_ws38")
##predictors <- c("SGUcode","TRI_hr_ws9")
##predictors <- c("SGUcode","TRI_hr_ws9","geom_hr_L50m_fl10_rplipmps_UE_hr_20cells_hr")
##predictors <- c("SGUcode","TRI_hr_ws38","geom_hr_L50m_fl10_rplipmps_UE_hr_20cells_hr")
#predictors <- c("SGUcode","DiurnalAnisotropicHeating_10m","vectorruggedness_hr_ws41","VerticalDistancetoChannelNetwork_10m")
proj2path="/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/"
gisBase="/usr/local/src/grass70_release/dist.x86_64-unknown-linux-gnu"
gisDbase =  "/media/fabs/Volume/Data/GRASSDATA/"
location="EPPAN_vhr"
mapset="paper3data_predictparentmaterial"
#########################################################################
initGRASS(gisBase = gisBase,gisDbase = gisDbase,location=location,mapset=mapset,override = TRUE)
#rastlist <- execGRASS("g.list",type="rast",pattern="*")
#rastlist <- attributes(rastlist)$resOut
#predictors <- c("SGUcode",rastlist[-c(6,7,11:31,49,63:66,78:82,88:89)])
#load(paste(proj2path,"data2017/modeldata_sGUkartiert.RData",sep=""))
legend <- read.table(paste(proj2path,"data2017/SGU_legend_new.txt",sep=""),sep="\t",header=T)
names(legend) <- c("SGU","SGUcode")
#dependent="SGU_kartiert"
#modelcols <- c(dependent,predictors)
SGU_gk <-readRAST("SGU")
data <- SGU_gk@data
#for (i in predictors[2:length(predictors)]){
#  temp <- readRAST(i)@data
#  data[[i]] <- temp[[i]]
#}
#names(data) <- predictors
#data$UID <- 1:nrow(data)
#modeldataoktober <- merge(modeldataoktober,legend,by.x="SGU_gk",by.y="SGU")
#modeldata <- modeldataoktober[modelcols[modelcols %in% names(modeldataoktober)]]
#modeldata$SGUcode <- factor(modeldata$SGUcode,levels=1:15)
#data$SGUcode <- factor(data$SGUcode,levels=1:15)
#f <- paste(dependent,"~.")
#fit <- do.call("randomForest",list(as.formula(f),modeldata))
#save(fit,data,file="fit_paper3datapredictors.RData")
#load(file="/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/temporlarge/fit_paper3datapredictors.RData")
#n4 <- as.integer(nrow(data) / 4)
#data1 <- data[1:n4,]
#save(data1,file="data1forpreds.RData")
#data1[["preds"]] <- predict(fit,newdata=data1)
#save(data1,file="data1withpreds.RData")
#data2 <- data[(n4+1):(n4*2),]
#save(data2,file="data2forpreds.RData")
#data2[["preds"]] <- predict(fit,newdata=data2)
#save(data2,file="data2withpreds.RData")
#data3 <- data[(n4*2+1):(n4*3),]
#save(data3,file="data3forpreds.RData")
#data3[["preds"]] <- predict(fit,newdata=data3)
#save(data3,file="data3withpreds.RData")
#data4 <- data[(n4*3+1):nrow(data),]
#save(data4,file="data4forpreds.RData")
#data4[["preds"]] <- predict(fit,newdata=data4)
#save(data4,file="data4withpreds.RData")
#load("/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/temporlarge/data1withpreds.RData")
#load("/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/temporlarge/data2withpreds.RData")
#data12 <- rbind(data1,data2)
#rm(data1,data2)
#load("/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/temporlarge/data3withpreds.RData")
#data12 <- rbind(data12,data3)
#load("SAVE.RData")
#load("/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/temporlarge/data4withpreds.RData")
#datawithpreds <- data12[c("UID","preds")]
#rm(data12)
#data4withpreds <- data4[c("UID","preds")]
#rm(data4)
#datawithpreds <- rbind(datawithpreds,data4withpreds)
#rm(data4withpreds)
#summary(datawithpreds)
#save(datawithpreds,file="/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/temporlarge/datawithpreds_largepredictorset.RData")
load("/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/temporlarge/datawithpreds_largepredictorset.RData")
SGU_modell <- SGU_gk
names(legend) <- c("SGU","SGU_predcodes")
data <- merge(datawithpreds,legend,by.x="preds",by.y="SGU",all.x=T)
data <-data[order(data$UID,decreasing = F),]
SGU_modell@data <- data
summary(SGU_modell)
outname=paste("SGUcode_largepredictorset")
writeRAST(SGU_modell["SGU_predcodes"],vname = outname)
execGRASS("r.to.vect",input=outname,output=outname,type="area")
execGRASS("v.out.ogr",input=outname,output=paste(outname,".shp",sep=""))

