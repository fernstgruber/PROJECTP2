require(e1071)
require(RCurl)
require(repmis)
require(randomForest)
library(knitr)
load("/home/fabs/PROJECTP2/data2017/boden_SGU_Oct5.RData")
proj2path="/home/fabs/PROJECTP2/"
paper2datapath ="/home/fabs/Data/paper2data/"
#proj2path="/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/"
#paper2datapath ="/media/fabs/Volume/01_PAPERZEUG/paper2data/"
myfunctions <- getURL("https://raw.githubusercontent.com/fernstgruber/Rstuff/master/fabiansandrossitersfunctions.R", ssl.verifypeer = FALSE)
eval(parse(text = myfunctions))
legend <- read.table(paste(proj2path,"data2017/SGU_legend_new.txt",sep=""),sep="\t",header=T)
load(paste(proj2path,"data2017/modeldata_sGUkartiert.RData",sep=""))
modelprofiles <- unique(modeldataoktober$I)
svg("/home/fabs/PROJECTP2/figure/studyarea_statistics/barplot_bodentypen.svg")
barplot(table(boden_SGU$TYP),las=2,cex.names=0.5,ylim=c(0,200))
dev.off()








#svg("nrcorrectbarplot_Defredmak.svg")
barplot(defredmakstack,col=c("#d7191c","#ec6e43","#fdb96e","#fee7a4","#e7f5b7","#b7e1a7","#2b83ba"),las=2,border=F,ylim=c(0,600))
legend("topright",pch=15,legend=as.character(0:6),col=c("#d7191c","#ec6e43","#fdb96e","#fee7a4","#e7f5b7","#b7e1a7","#2b83ba"))
#dev.off()