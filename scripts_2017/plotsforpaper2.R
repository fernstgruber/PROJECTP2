require(e1071)
require(RCurl)
require(repmis)
require(randomForest)
library(knitr)
proj2path="/home/fabs/PROJECTP2/"
paper2datapath ="/home/fabs/Data/paper2data/"
#proj2path="/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/"
#paper2datapath ="/media/fabs/Volume/01_PAPERZEUG/paper2data/"
load(paste(proj2path,"data2017/boden_SGU_Oct5.RData",sep=""))
load(paste(proj2path,"data2017/pointIDs.RData",sep=""))
myfunctions <- getURL("https://raw.githubusercontent.com/fernstgruber/Rstuff/master/fabiansandrossitersfunctions.R", ssl.verifypeer = FALSE)
eval(parse(text = myfunctions))
legend <- read.table(paste(proj2path,"data2017/SGU_legend_new.txt",sep=""),sep="\t",header=T)
load(paste(proj2path,"data2017/modeldata_sGUkartiert.RData",sep=""))
modelprofiles <- unique(modeldataoktober$I)
svg("/home/fabs/PROJECTP2/figure/studyarea_statistics/barplot_bodentypen.svg")
barplot(table(boden_SGU$TYP),las=2,cex.names=0.5,ylim=c(0,200))
dev.off()


########
#Welche Punkte werden verwendet?



