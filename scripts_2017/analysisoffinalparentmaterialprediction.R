require(e1071)
require(randomForest)
require(rgdal)
require(rgrass7)
###################################################################################################################################################

###################################################################################################################################################

proj2path="/home/fabs/PROJECTP2/"
predictors <- c("SGUcode","TRI_hr_ws31","vectorruggedness_hr_ws57")
load(paste(proj2path,"data2017/modeldata_sGUkartiert.RData",sep=""))
legend <- read.table(paste(proj2path,"data2017/SGU_legend_new.txt",sep=""),sep="\t",header=T)
names(legend) <- c("SGU","SGUcode")
dependent="SGU_kartiert"
modelcols <- c(dependent,predictors)
modeldataoktober <- merge(modeldataoktober,legend,by.x="SGU_gk",by.y="SGU")
modeldata <- modeldataoktober[c(modelcols)]
modeldata$SGUcode <- factor(modeldata$SGUcode,levels=1:15)
f <- paste(dependent,"~.")
fit <- do.call("randomForest",list(as.formula(f),modeldata,cross=10))
fit$confusion
fit$err.rate[nrow(fit$err.rate),1]
modeldata[["preds"]] <- predict(fit,newdata=modeldata)
table(modeldata$preds,modeldata$SGU_kartiert)
proj2path="/home/fabs/PROJECTP2/"
predictors <- c("SGUcode","TRI_hr_ws31","vectorruggedness_hr_ws57")
load(paste(proj2path,"data2017/modeldata_sGUkartiert.RData",sep=""))
legend <- read.table(paste(proj2path,"data2017/SGU_legend_new.txt",sep=""),sep="\t",header=T)
names(legend) <- c("SGU","SGUcode")
dependent="SGU_kartiert"
modelcols <- c(dependent,predictors)
modeldataoktober <- merge(modeldataoktober,legend,by.x="SGU_gk",by.y="SGU")
modeldata <- modeldataoktober[c(modelcols)]
modeldata$SGUcode <- factor(modeldata$SGUcode,levels=1:15)
f <- paste(dependent,"~.")
fit <- do.call("randomForest",list(as.formula(f),modeldata,cross=10))
fit$confusion
fit$err.rate[nrow(fit$err.rate),1]
modeldata[["preds"]] <- predict(fit,newdata=modeldata)
table(modeldata$preds,modeldata$SGU_kartiert)

