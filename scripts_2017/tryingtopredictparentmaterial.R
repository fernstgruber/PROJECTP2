require(e1071)
require(randomForest)
require(rgdal)
require(rgrass7)
load("/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/data2017/modeldata_sGUkartiert.RData")
legend <- read.table("/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/data2017/SGU_legend_new.txt",sep="\t",header=T)
names(legend) <- c("SGU","SGUcode")
dependent="SGU_kartiert"
gisBase="/usr/local/src/grass70_release/dist.x86_64-unknown-linux-gnu"
gisDbase =  "/media/fabs/Volume/Data/GRASSDATA/"
location="EPPAN_vhr"
mapset="paper3data_predictparentmaterial"
initGRASS(gisBase = gisBase,gisDbase = gisDbase,location=location,mapset=mapset,override = TRUE)

predictors <- c("SGUcode","vectorruggedness_hr_ws51","TRI_hr_ws9")
modelcols <- c(dependent,predictors)
SGU_gk <-readRAST("SGU")
data <- SGU_gk@data
for (i in predictors[2:length(predictors)]){
  temp <- readRAST(i)@data
  data[[i]] <- temp[[i]]
}
names(data) <- predictors
data$UID <- 1:nrow(data)
names(data)
modeldataoktober <- merge(modeldataoktober,legend,by.x="SGU_gk",by.y="SGU")
modeldata <- modeldataoktober[c(modelcols)]
modeldata <- merge(modeldata,legend,by.x="SGU_gk",by.y=)
names(modeldata)
modeldata$SGUcode <- factor(modeldata$SGUcode,levels=1:15)
data$SGUcode <- factor(data$SGUcode,levels=1:15)
str(data)
str(modeldata)

nadata <- na.omit(data)
summary(nadata)
#remove Mrd!!


f <- paste(dependent,"~.")
fit <- do.call("randomForest",list(as.formula(f),modeldata))
preds <- predict(fit,newdata=data)
preds <- predict(fit,newdata=data[,predictors])
preds <- predict(fit, newdata=na.omit(data))
preds <- predict(fit,newdata=as.matrix(data))
preds <- predict(fit,newdata=nadata[predictors])

function(modeldata,dependent,predictors,legend,doreturn=FALSE,outname,input){
  require(e1071)
  require(rgdal)
  modeldata_new <- merge(modeldata,legend,all.x=T)
  dependent_new <- names(legend)[1]
  modeldata_new[[dependent_new]] <-droplevels(modeldata_new[[dependent_new]]) 
  mymodeldata <- modeldata_new[c(dependent_new,predictors)]
  f <- paste(dependent_new,"~.")
  fit <- do.call("svm",list(as.formula(f),mymodeldata,cross=10,kernel="radial"))
  modellevels <- levels(mymodeldata[[predictors]])
  oldrast <- readGDAL(input)
  newdata <-oldrast@data
  newdata$UID <- 1:nrow(newdata)
  newdata[[predictors]] <-as.factor(newdata$band1) 
  newlevels <- levels(newdata[[predictors]])
  bothlevels<- modellevels[modellevels %in% newlevels]
  predictdata<- newdata[newdata[[predictors]] %in% bothlevels,]
  predictdata<-droplevels(predictdata)
  predictdata$pred <- predict(fit,predictdata)
  predictdata <- merge(predictdata,legend,by.x="pred",by.y=dependent_new,all.x=TRUE)
  newdata <- merge(newdata,predictdata,by="UID",all.x=T)
  newdata <- newdata[order(newdata$UID,decreasing = F),]
  oldrast@data <- newdata
  writeGDAL(oldrast["code"],fname=paste(outname,"_",dependent,".tif",sep=""))
  cverror = 1-(fit$tot.accuracy)/100
  print(paste("10fold cv-error: ",cverror," for predictors",paste(predictors,collapse=" AND ")))
  preds <- predict(fit,mymodeldata)
  CM <- table(mymodeldata[[dependent_new]],preds)
  print(CM)
  summary.kappa(kappa(CM))
  print(paste("#########  Cramer's V = ",Cramer(CM)))
  if(doreturn) {  return(preds)}
}
