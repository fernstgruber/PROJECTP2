#ACHTUNG sind die SGU_predcodes eh Faktoren??

require(e1071)
require(randomForest)
require(rgdal)
require(rgrass7)

predictors <- c("SGUcode","MinimalCurvature_10m","geom_hr_L50m_fl1_rplipshape_UE_hr_60cells_hr")
proj2path="/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/"
gisBase="/usr/local/src/grass70_release/dist.x86_64-unknown-linux-gnu"
gisDbase =  "/media/fabs/Volume/Data/GRASSDATA/"
location="EPPAN_vhr"
mapset="paper3data_predictparentmaterial"
#########################################################################
initGRASS(gisBase = gisBase,gisDbase = gisDbase,location=location,mapset=mapset,override = TRUE)
load(paste(proj2path,"data2017/modeldata_sGUkartiert.RData",sep=""))
modeldataoktober[modeldataoktober$SGU_kartiert == "SD","SDornot"] <- 1
modeldataoktober[modeldataoktober$SGU_kartiert != "SD","SDornot"] <- 0
modeldataoktober$SDornot <- as.factor(modeldataoktober$SDornot)
legend <- read.table(paste(proj2path,"data2017/SGU_legend_new.txt",sep=""),sep="\t",header=T)
names(legend) <- c("SGU","SGUcode")
dependent="SDornot"
modelcols <- c(dependent,predictors)
SGU_gk <-readRAST("SGU")
data <- SGU_gk@data
for (i in predictors[2:length(predictors)]){
  temp <- readRAST(i)@data
  data[[i]] <- temp[[i]]
}
names(data) <- predictors
data$UID <- 1:nrow(data)
modeldataoktober <- merge(modeldataoktober,legend,by.x="SGU_gk",by.y="SGU")
modeldata <- modeldataoktober[c(modelcols)]
modeldata$SGUcode <- factor(modeldata$SGUcode,levels=1:15)
data$SGUcode <- factor(data$SGUcode,levels=1:15)
f <- paste(dependent,"~.")
fit <- do.call("randomForest",list(as.formula(f),modeldata))
data[["preds"]] <- predict(fit,newdata=data)
SGU_modell <- SGU_gk
#names(legend) <- c("SGU","SGU_predcodes")
#data <- merge(data,legend,by.x="preds",by.y="SGU",all.x=T)
data$SD <- as.numeric(data$preds)
data <-data[order(data$UID,decreasing = F),]
SGU_modell@data <- data
#summary(SGU_modell)
outname=paste("Slopedeposit_",predictors,collapse="_",sep="")
writeRAST(SGU_modell["SD"],vname = outname)
execGRASS("r.to.vect",input=outname,output=outname,type="area")
execGRASS("v.out.ogr",input=outname,output=paste(outname,".shp",sep=""))

