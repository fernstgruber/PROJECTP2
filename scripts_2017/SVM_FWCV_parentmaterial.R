require(e1071)
require(RCurl)
require(repmis)
require(randomForest)
myfunctions <- getURL("https://raw.githubusercontent.com/fernstgruber/Rstuff/master/fabiansandrossitersfunctions.R", ssl.verifypeer = FALSE)
eval(parse(text = myfunctions))
load("/home/fabs/Data/paper2data/profiledata.RData")
allpreds <- c(localterrain,regionalterrain,roughness,heights)
paramsets <- list(localterrain,regionalterrain,roughness,heights,allpreds)
paramsetnames <- c("localterrain","regionalterrain","roughness","heights","allpreds")
dependent="SGU_kartiert"
allpreds <- c(localterrain,regionalterrain,roughness,heights)
allpreds <- c(allpreds[allpreds %in% names(profiledata)])
nadata <- na.omit(profiledata)
problempunkte <- profiledata[!(profiledata$ID %in% nadata$ID),]
profiledata <- profiledata[profiledata$ID != "12884", ]
badones <-vector()
for(pp in allpreds){
  if(nrow(profiledata[is.na(profiledata[[pp]]),]) > 0) {
    badones <-c(badones,pp)
  }
}
allpreds=allpreds[!(allpreds %in% badones)]
paramsets[[5]] <- allpreds
origmodeldata <- profiledata[names(profiledata) %in% c(dependent,"SGU_gk",allpreds)]

#########################################################################################
psets <- c(1:4)
classes <-  levels(origmodeldata[[dependent]])
#save(classes,paramsets,modeldata,paramsetnames,file="classesandparamsets.RData")
paramsetnames = paramsetnames[psets]
paramsets = paramsets[psets]

n=1
p=paramsets[1]
for (p in paramsets){
  predset_name <- paramsetnames[n]
  preds <- unlist(p)
  preds <- preds[preds %in% allpreds]
  predset= c(preds,"SGU_gk")
  mymodeldata <- origmodeldata[c(dependent,predset)]
  folds = sample(rep(1:5, length = nrow(mymodeldata)))
  
  tt=1:5 #number of best parameters in combination
  mydir=paste("svm_fw_5fold_5p_",dependent,"_",predset_name,"",sep="")
  dir.create(mydir)
  #############################################################################################################################
  #############################################################################################################################
  k=1
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


