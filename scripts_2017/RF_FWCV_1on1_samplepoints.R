require(e1071)
require(RCurl)
require(repmis)
require(randomForest)
myfunctions <- getURL("https://raw.githubusercontent.com/fernstgruber/Rstuff/master/fabiansandrossitersfunctions.R", ssl.verifypeer = FALSE)
eval(parse(text = myfunctions))
load("/media/fabs/Volume/01_PAPERZEUG/paper2data/sampledata_100mgrid.RData")
allpreds <- c(localterrain,regionalterrain,roughness,heights)
paramsets <- list(localterrain,regionalterrain,roughness,heights,allpreds)
paramsetnames <- c("localterrain","regionalterrain","roughness","heights","allpreds")
dependent="geolegen_1"  
sampledata <- na.omit(sampledata) 
origmodeldata <- sampledata[names(sampledata) %in% c(dependent,allpreds)]
origmodeldata[[dependent]] <- as.factor(origmodeldata[[dependent]])
badones <- vector()

for (p in allpreds){
  if (summary(origmodeldata[[p]])[5] == 0.0 ) {
    badones <- c(badones,p)
  }
}
roughness <- roughness[!(roughness %in% badones)]
allpreds <- c(localterrain,regionalterrain,roughness,heights)
paramsets <- list(localterrain,regionalterrain,roughness,heights,allpreds)
#########################################################################################
psets <- c(3,1,5)
classes <-  levels(origmodeldata[[dependent]])
classes <- classes[!(classes %in% c("Ant","WB","MrD"))]
origclasses <- classes
analysisclasses <- c("TG","SD","CSR","DC")
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
tt=1:3 #number of best parameters in combination
mydir=paste("RanFor_1on1_5fold","_sampledataSGUgk",predset_name,"p5",sep="")
dir.create(mydir)
cl=classes[1]
for(cl in analysisclasses){
  newclasses <- origclasses[!(origclasses %in% cl)]
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
          fit <- do.call("randomForest",list(as.formula(f),modeldata))
          cverror = fit$err.rate[nrow(fit$err.rate),1]
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
        fit <- do.call("randomForest",list(as.formula(f),modeldata))
        preds=predict(fit,newdata=ktestdata)
        prederror=mean(ktestdata[[dependent]] != preds)
        result_df[t,"geheimerprederror"] <- prederror
        print(paste("geheimerprederror = ",prederror,sep=""))
        testdatatable <- table(ktestdata[[dependent]])
        traindatatable<- table(kmodeldata[[dependent]])
        importance <- as.data.frame(fit$importance)
        save(predictions_metrics,result_df,fit,keepers,traindatatable,importance,testdatatable,file=paste(mynewdir,"/k",k,"_round_",t,".RData",sep=""))
        predictions_metrics <- predictions_metrics[predictions_metrics$index != as.character(minindex),]
        
      }
    }
  }
 }
#############################################################################################################################
#############################################################################################################################

n=n+1
}

