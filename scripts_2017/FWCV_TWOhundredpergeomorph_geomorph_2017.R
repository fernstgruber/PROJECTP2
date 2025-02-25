require(randomForest)
require(e1071)
require(RCurl)
require(repmis)
myfunctions <- getURL("https://raw.githubusercontent.com/fernstgruber/Rstuff/master/fabiansandrossitersfunctions.R", ssl.verifypeer = FALSE)
eval(parse(text = myfunctions))
source_data("https://github.com/fernstgruber/p2/blob/master/data2017/geotopodata_twohundredpergeomorph_2017.RData?raw=true")
setwd("/media/fabs/Volume/01_PAPERZEUG/newstuff_p2/")
origmodeldata <- modeldata
origmodeldata$Festgestein <- ifelse(is.na(origmodeldata$Festgestein),yes = 0,no = 1)
origmodeldata$Festgestein <- as.factor(origmodeldata$Festgestein)
dependent=geospalten[1]
preds <- allpreds
fullmodelcols <- c(dependent,preds)
paramsetnames
paramsets[[1]] <- paramsets[[1]][-c(156,162:163,196)]
paramsets[[5]] <- paramsets[[5]][-c(80:82,84:93,95:102)]
paramsets[[2]] <- paramsets[[2]][-c(132:133)]
paramsets[[3]] <- paramsets[[3]][-c(2,15)]
paramsets[[4]] <- paramsets[[4]][-c(5,8,10,13,15,18,20,23,25,28,30,33,35,38,40,43,45,48,50,53,55,58,60,63,65,68,70,72,73,76,78,81,83,86,88,91,93,96,98,101,103,106,108,
                                    111,113,116,118,121,123,126,128,131,133,136,138,141,143,146,148,151,153,156,158,161,163,164,165,168,170,173,175,178,180,183,185)]
paramsets[[4]] <- paramsets[[4]][-c(44)]
#########################################################################################
psets <- c(7)
classes <-  levels(origmodeldata[[dependent]])
#save(classes,paramsets,modeldata,paramsetnames,file="classesandparamsets.RData")
paramsetnames = paramsetnames[psets]
paramsets = paramsets[psets]

n=1

#p=paramsets[2]
for (p in paramsets){
predset_name <- paramsetnames[n]
preds <- unlist(p)
badones <-vector()
  for(pp in preds){
    if(nrow(origmodeldata[is.na(origmodeldata[[pp]]),]) > 10) {
      badones <-c(badones,pp)
    }
  }
predset=preds[!(preds %in% badones)]
mymodeldata <- na.omit(origmodeldata[c(dependent,predset)])
folds = sample(rep(1:5, length = nrow(mymodeldata)))

tt=1:10 #number of best parameters in combination
mydir=paste("svm_fw_5fold_10p_",dependent,"_",predset_name,"_200pg_v2",sep="")
dir.create(mydir)
#############################################################################################################################
#############################################################################################################################
for(k in 1:5){
  kmodeldata=mymodeldata[folds != k,]
  ktestdata =  mymodeldata[folds == k,]
  keepers <- vector()
  pred_df_orig <- data.frame(preds = as.character(predset))
  pred_df_orig$index <- 1:nrow(pred_df_orig)
  pred_df <- pred_df_orig
  result_df <- data.frame(tt)
  predictions_metrics <- data.frame(index=as.character(unique(pred_df$preds)))
  predset_new <- predset
  t=1
  for(t in tt){
    predictions_metrics <- predictions_metrics
    predset_new <-predset_new[!(predset_new %in% keepers)]
    seed=sample(1:1000,1)
    g=predset_new[1]
    for(g in predset_new){
      starttime <- proc.time()
      set.seed(seed)
      modelcols <- c(dependent,g,keepers)
      modeldata <- kmodeldata[names(kmodeldata) %in% modelcols]
      f <- paste(dependent,"~.")
      fit <- do.call("svm",list(as.formula(f),modeldata,cross=5))
      cverror = 1-(fit$tot.accuracy)/100
      predictions_metrics[predictions_metrics$index== eval(g),"cverror"] <- cverror
      endtime <- proc.time()
      time <- endtime - starttime
      print(paste(g, " with cverror error.rate of ",cverror, "and time =",time[3]))
      #######################################################################
    }
    predictions_metrics <- predictions_metrics[order(predictions_metrics$cverror),]
    
    minindex <- predictions_metrics[predictions_metrics$cverror == min(predictions_metrics$cverror),"index"][1]
    print(paste("###########################################################################################################################################################remove the metric ",minindex,"##############################################################################################################################################################",sep=""))
    result_df[t,"metric"] <- minindex
    result_df[t,"cverror"] <- predictions_metrics[predictions_metrics$index == minindex,"cverror"]
    keepers[t] <-as.character(minindex)
    modelcols <- c(dependent,keepers)
    modeldata <- modeldata <- kmodeldata[names(kmodeldata) %in% modelcols]
    modeldata <- na.omit(modeldata)
    set.seed(seed)
    fit <- do.call("svm",list(as.formula(f),modeldata,cross=5))
    preds=predict(fit,newdata=ktestdata)
    prederror=mean(ktestdata[[dependent]] != preds)
    result_df[t,"geheimerprederror"] <- prederror
    print(paste("geheimerprederror = ",prederror,sep=""))
    testdatatable <- table(ktestdata[[dependent]])
    traindatatable<- table(kmodeldata[[dependent]])
    save(predictions_metrics,result_df,fit,keepers,traindatatable,testdatatable,file=paste(mydir,"/k",k,"_round_",t,".RData",sep=""))
    predictions_metrics <- predictions_metrics[predictions_metrics$index != as.character(minindex),]
    
  }
}
n=n+1
}


