
require(rgrass7)
library(RCurl)
library(repmis)
require(xtable)
geolegendeng <- read.table(text=getURL("https://raw.githubusercontent.com/fernstgruber/p2/master/data2017/geolegendeng.txt"),sep="\t",header=T)
myfunctions <- getURL("https://raw.githubusercontent.com/fernstgruber/Rstuff/master/fabiansandrossitersfunctions.R", ssl.verifypeer = FALSE)
eval(parse(text = myfunctions))
varlist <- c("ChannelNetworkBaseLevel","Channel_Network_Base_Level","Convexity","Longitudinal_Curvature",
             "Maximum_Height_hr","Normalized_Height","TPI_i0m_o500m","TRI_hr_ws26","Texture",
             "VerticalDistancetoChannelNetwork","aspect_ws15_hr","bodenbedeckung","crosc_DTM_50m_avg_ws3",
             "crosc_DTM_50m_avg_ws7","geom_dtm_10m_hyd_fl5_L30","geom_hr_L3_fl1_r.li.simpson_UE_hr_40cells",
             "hoehenstufen","longc_DTM_50m_avg_ws7","profc_DTM_50m_avg_ws7","slope_DTM_50m_avg_ws3",
             "slope_DTM_50m_avg_ws7","slope_ws15","slope_ws15_hr")
numericlist <-  c("ChannelNetworkBaseLevel","Channel_Network_Base_Level","Convexity","Longitudinal_Curvature",
                  "Maximum_Height_hr","Normalized_Height","TPI_i0m_o500m","TRI_hr_ws26","Texture",
                  "VerticalDistancetoChannelNetwork","aspect_ws15_hr","crosc_DTM_50m_avg_ws3",
                  "crosc_DTM_50m_avg_ws7","geom_dtm_10m_hyd_fl5_L30","geom_hr_L3_fl1_r.li.simpson_UE_hr_40cells",
                  "longc_DTM_50m_avg_ws7","profc_DTM_50m_avg_ws7","slope_DTM_50m_avg_ws3",
                  "slope_DTM_50m_avg_ws7","slope_ws15","slope_ws15_hr")
CNBLlist <-  c("ChannelNetworkBaseLevel","Channel_Network_Base_Level")
load(file="/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/temporlarge/sgu_df_base_ordered.RData")

gisBase="/usr/local/src/grass70_release/dist.x86_64-unknown-linux-gnu"
gisDbase =  "/media/fabs/Volume/Data/GRASSDATA/"
location="EPPAN_vhr"
mapset="paper3data_masktest"
initGRASS(gisBase = gisBase,gisDbase = gisDbase,location=location,mapset=mapset,override = TRUE)
for (num in numericlist[17:21]){
  print(num)
  sgu_df[[num]] <- readRAST(num)@data[[num]]
  boxplot(data=sgu_df,as.formula(paste(num,"~","Abbrev.")),las=2)
  
}
save(sgu_df,file="/media/fabs/Volume/01_PAPERZEUG/PROJECTP2/temporlarge/SGU_df_17bis21.RData")
