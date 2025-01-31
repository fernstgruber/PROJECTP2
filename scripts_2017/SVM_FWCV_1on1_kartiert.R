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
localterrain <- localterrain[localterrain %in% allpreds]
paramsets[[1]] <- localterrain
roughness <- roughness[roughness %in% allpreds]
paramsets[[3]] <- roughness
origmodeldata <- profiledata[names(profiledata) %in% c(dependent,"SGU_gk",allpreds)]
#########################################################################################
psets <- c(3:5)
classes <-  levels(origmodeldata[[dependent]])
classes <- classes[!(classes %in% c("MrD"))]
#save(classes,paramsets,modeldata,paramsetnames,file="classesandparamsets.RData")
paramsetnames = paramsetnames[psets]
paramsets = paramsets[psets]

n=1

p=paramsets[1]
for (p in paramsets){
  predset_name <- paramsetnames[n]
  preds <- unlist(p)
  
  predset=preds
  mymodeldata <- na.omit(origmodeldata[c(dependent,predset)])
  folds = sample(rep(1:5, length = nrow(mymodeldata)))
  
  predset=preds
  tt=1:5 #number of best parameters in combination
  mydir=paste("SVM_1on1_5fold","_SGUkartiert",predset_name,"p5",sep="")
  dir.create(mydir)
  cl=classes[1]
for(cl in classes){
  newclasses <- classes[!(classes %in% cl)]
  newclass=newclasses[1]
  for(newclass in newclasses){
    modelclasses <- c(cl,newclass)
    mynewmodeldata <- mymodeldata[mymodeldata[[dependent]] %in% modelclasses,]
    mynewmodeldata[[dependent]] <- droplevels(mynewmodeldata[[dependent]]) 
    mynewdir = paste(mydir,"/",cl,"_vs_",newclass,sep="")
    dir.create(mynewdir)
    folds = sample(rep(1:5, length = nrow(mynewmodeldata)))
    trial=1001
    k=1
    for(k in 1:5){
      kmodeldata=mynewmodeldata[folds != k,]
      ktestdata =  mynewmodeldata[folds == k,]
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
        g=predset_new[1]
        gcount=1
        for(g in predset_new){
          print(gcount)
          starttime <- proc.time()
          set.seed(trial)
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
          gcount=gcount +1
        }
        predictions_metrics <- predictions_metrics[order(predictions_metrics$cverror),]
        
        minindex <- predictions_metrics[predictions_metrics$cverror == min(predictions_metrics$cverror),"index"][1]
        print(paste("###########################################################################################################################################################remove the metric ",minindex,"##############################################################################################################################################################",sep=""))
        result_df[t,"metric"] <- minindex
        result_df[t,"cverror"] <- predictions_metrics[predictions_metrics$index == minindex,"cverror"]
        keepers[t] <-as.character(minindex)
        modelcols <- c(dependent,keepers)
        modeldata <- kmodeldata[names(kmodeldata) %in% modelcols]
        modeldata <- na.omit(modeldata)
        set.seed(trial)
        fit <- do.call("svm",list(as.formula(f),modeldata,cross=5))
        preds=predict(fit,newdata=ktestdata)
        prederror=mean(ktestdata[[dependent]] != preds)
        result_df[t,"geheimerprederror"] <- prederror
        print(paste("geheimerprederror = ",prederror,sep=""))
        testdatatable <- table(ktestdata[[dependent]])
        traindatatable<- table(kmodeldata[[dependent]])
        save(predictions_metrics,result_df,fit,keepers,traindatatable,testdatatable,file=paste(mynewdir,"/k",k,"_round_",t,".RData",sep=""))
        predictions_metrics <- predictions_metrics[predictions_metrics$index != as.character(minindex),]
        
      }
    }
  }
 }
#############################################################################################################################
#############################################################################################################################

n=n+1
}


