require(randomForest)
require(RCurl)
require(repmis)
myfunctions <- getURL("https://raw.githubusercontent.com/fernstgruber/Rstuff/master/fabiansandrossitersfunctions.R", ssl.verifypeer = FALSE)
eval(parse(text = myfunctions))
source_data("https://github.com/fernstgruber/p2/blob/master/data2017/geotopodata_twohundredpergeomorph_2017.RData?raw=true")
rm(modeldata)
dependent=""
preds <- allpreds
####################paramsets##
paramsetnames
paramsets[[1]] <- paramsets[[1]][-c(156,162:163,196)]
paramsets[[5]] <- paramsets[[5]][-c(80:82,84:93,95:102)]
paramsets[[2]] <- paramsets[[2]][-c(132:133)]
paramsets[[3]] <- paramsets[[3]][-c(2,15)]
paramsets[[4]] <- paramsets[[4]][-c(5,8,10,13,15,18,20,23,25,28,30,33,35,38,40,43,45,48,50,53,55,58,60,63,65,68,70,72,73,76,78,81,83,86,88,91,93,96,98,101,103,106,108,
                                    111,113,116,118,121,123,126,128,131,133,136,138,141,143,146,148,151,153,156,158,161,163,164,165,168,170,173,175,178,180,183,185)]
paramsets[[4]] <- paramsets[[4]][-c(44)]
####testing the 100m grid
require(rgdal)
grid <- readOGR(dsn="/media/fabs/Volume/01_PAPERZEUG/paper2data/100m_grid_SGU.shp", layer="100m_grid_SGU")
length(unique(grid$ID))
table(as.factor(grid$ID))
#################get new data################
#basedata
south <- sqliteGRASS_delilah(location = "SUEDTIROL_DTM_NEU", mapset= "paper2_samplevectors",vector = "res10m_dtmtransfer_100m_grid_SGU_south")
south <- south[,c(2,8)]
south$NorS <- "south"
south$nID <- paste(1:nrow(south),"S",sep="")
north <- sqliteGRASS_delilah(location = "SUEDTIROL_DTM_NEU", mapset= "paper2_samplevectors",vector = "res10m_dtmtransfer_100m_grid_SGU_north")
north <- north[,c(2,8)]
north$NorS <- "north"
north$nID <- paste(1:nrow(north),"N",sep="")
basedata<- rbind(south,north)
profilesites <- sqliteGRASS_delilah(location = "SUEDTIROL_DTM_NEU", mapset= "Profilpunkte",vector = "res10m_dtmtransfer_Profilpunktemitboden_UTM")
base_profilesites <- profilesites[,c(4,37:41,45,55:56)]
names(base_profilesites) <- c("ID","bodenorig","Class","Type", "Subtype","variation","CARG","SGU_kartiert","SGU_gk")
#res10m localterrain
cols <- c(13:22,28:43,45:57)
south <- sqliteGRASS_delilah(location = "SUEDTIROL_DTM_NEU", mapset= "paper2_samplevectors",vector = "res10m_dtmtransfer_100m_grid_SGU_south")
south <- south[,c(2,cols)]
south$nID <- paste(1:nrow(south),"S",sep="")
north <- sqliteGRASS_delilah(location = "SUEDTIROL_DTM_NEU", mapset= "paper2_samplevectors",vector = "res10m_dtmtransfer_100m_grid_SGU_north")
north <- north[,c(2,cols)]
north$nID <- paste(1:nrow(north),"N",sep="")
paramscale_10m <- rbind(north,south)
names(paramscale_10m) <- paste(names(paramscale_10m),"_10m",sep="")
profilesites <- sqliteGRASS_delilah(location = "SUEDTIROL_DTM_NEU", mapset= "Profilpunkte",vector = "res10m_dtmtransfer_Profilpunktemitboden_UTM")
profilesites <-profilesites[,c(4,cols+46)]
profilesites_paramscale_10m <- profilesites
names(profilesites_paramscale_10m) <- paste(names(profilesites_paramscale_10m),"_10m",sep="")
#res10m saga
cols <- c(13:14,18,21,23:24,27,30:32,34,36:42,44:46,49:52)
north <- sqliteGRASS_delilah(location = "SUEDTIROL_DTM_NEU", mapset= "paper2_samplevectors",vector = "res10m_saga_100m_grid_SGU_north")
north <- north[,c(2,cols)]
north$nID <- paste(1:nrow(north),"N",sep="")
south <- sqliteGRASS_delilah(location = "SUEDTIROL_DTM_NEU", mapset= "paper2_samplevectors",vector = "res10m_saga_100m_grid_SGU_south")
south <- south[,c(2,cols)]
south$nID <- paste(1:nrow(south),"S",sep="")
saga_10m <- rbind(north,south)
names(saga_10m) <- paste(names(saga_10m),"_10m",sep="")
profilesites <- sqliteGRASS_delilah(location = "SUEDTIROL_DTM_NEU", mapset= "Profilpunkte",vector = "res10m_saga_Profilpunktemitboden_UTM")
profilesites <-profilesites[,c(4,cols+46)]
profilesites_saga_10m <- profilesites
names(profilesites_saga_10m) <- paste(names(profilesites_saga_10m),"_10m",sep="")
#res50m saga
cols <- c(12:15,17:32,34:38,41,44:45,48:87)
north <- sqliteGRASS_delilah(location = "SUEDTIROL_DTM_NEU", mapset= "paper2_samplevectors",vector = "res50m_mitsaga_100m_grid_SGU_north")
north <- north[,c(2,cols)]
north$nID <- paste(1:nrow(north),"N",sep="")
south <- sqliteGRASS_delilah(location = "SUEDTIROL_DTM_NEU", mapset= "paper2_samplevectors",vector = "res50m_mitsaga_100m_grid_SGU_south")
south <- south[,c(2,cols)]
south$nID <- paste(1:nrow(south),"S",sep="")
saga_50m <- rbind(north,south)
names(saga_50m) <- paste(names(saga_50m),"_50m",sep="")
profilesites <- sqliteGRASS_delilah(location = "SUEDTIROL_DTM_NEU", mapset= "Profilpunkte",vector = "res50m_mitsaga_Profilpunktemitboden_UTM")
profilesites <-profilesites[,c(4,cols+46)]
profilesites_saga_50m <- profilesites
names(profilesites_saga_50m) <- paste(names(profilesites_saga_50m),"_50m",sep="")
#####################
#hr_localterrain
cols <- c(21:31,35:94)
south <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "dtm_hr_100m_grid_SGU_south")
south <- south[,c(2,cols)]
south$nID <- paste(1:nrow(south),"S",sep="")
north <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "dtm_hr_100m_grid_SGU_north")
north <- north[,c(2,cols)]
north$nID <- paste(1:nrow(north),"N",sep="")
paramscale_hr <- rbind(north,south)
names(paramscale_hr) <- paste(names(paramscale_hr),"_hr",sep="")
profilesites <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "profilpunkte",vector = "dtm_hr_Profilpunktemitboden_UTM")
profilesites <-profilesites[,c(4,cols+46)]
profilesites_paramscale_hr<- profilesites
names(profilesites_paramscale_hr) <- paste(names(profilesites_paramscale_hr),"_hr",sep="")
############
#####################
#hr_rli
cols <- c(11:35,41:85,91:135,141:185,191:210)
south <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "dtm_hr_rli_100m_grid_SGU_south")
south <- south[,c(2,cols)]
south$nID <- paste(1:nrow(south),"S",sep="")
north <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "dtm_hr_rli_100m_grid_SGU_north")
north <- north[,c(2,cols)]
north$nID <- paste(1:nrow(north),"N",sep="")
rli_hr <- rbind(north,south)
names(rli_hr) <- paste(names(rli_hr),"_hr",sep="")
profilesites <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "profilpunkte",vector = "dtm_hr_rli_Profilpunktemitboden_UTM")
profilesites <-profilesites[,c(4,cols+46)]
profilesites_rli_hr<- profilesites
names(profilesites_rli_hr) <- paste(names(profilesites_rli_hr),"_hr",sep="")
############
#####################
#hr_roughnessvector
cols <- c(15:44,46:75)
south <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "dtm_hr_roughnessvector_100m_grid_SGU_south")
south <- south[,c(2,cols)]
south$nID <- paste(1:nrow(south),"S",sep="")
north <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "dtm_hr_roughnessvector_100m_grid_SGU_north")
north <- north[,c(2,cols)]
north$nID <- paste(1:nrow(north),"N",sep="")
roughnessvector <- rbind(north,south)
names(roughnessvector) <- paste(names(roughnessvector),"_hr",sep="")
profilesites <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "profilpunkte",vector = "dtm_hr_roughnessvector_Profilpunktemitboden_UTM")
profilesites <-profilesites[,c(4,cols+46)]
profilesites_roughnessvector<- profilesites
names(profilesites_roughnessvector) <- paste(names(profilesites_roughnessvector),"_hr",sep="")
############
#####################
#hr_Textur
cols <- c(12:53)
south <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "dtm_hr_Texture_100m_grid_SGU_south")
south <- south[,c(2,cols)]
south$nID <- paste(1:nrow(south),"S",sep="")
north <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "dtm_hr_Texture_100m_grid_SGU_north")
north <- north[,c(2,cols)]
north$nID <- paste(1:nrow(north),"N",sep="")
texture <- rbind(north,south)
names(texture) <- paste(names(texture),sep="")
profilesites <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "profilpunkte",vector = "dtm_hr_Texture_Profilpunktemitboden_UTM")
profilesites <-profilesites[,c(4,cols+46)]
profilesites_texture<- profilesites
names(profilesites_texture) <- paste(names(profilesites_texture),sep="")
############
#####################
#hr_TRI
cols <- c(11:65)
south <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "dtm_hr_TRI_100m_grid_SGU_south")
south <- south[,c(2,cols)]
south$nID <- paste(1:nrow(south),"S",sep="")
north <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "dtm_hr_TRI_100m_grid_SGU_north")
north <- north[,c(2,cols)]
north$nID <- paste(1:nrow(north),"N",sep="")
tri <- rbind(north,south)
names(tri) <- paste(names(tri),sep="")
profilesites <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "profilpunkte",vector = "dtm_hr_TRI_Profilpunktemitboden_UTM")
profilesites <-profilesites[,c(4,cols+46)]
profilesites_tri<- profilesites
names(profilesites_tri) <- paste(names(profilesites_tri),sep="")
############
#hr_vectorruggedness
cols <- c(12:40)
south <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "dtm_hr_vectorruggedness_100m_grid_SGU_south")
south <- south[,c(2,cols)]
south$nID <- paste(1:nrow(south),"S",sep="")
north <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "dtm_hr_vectorruggedness_100m_grid_SGU_north")
north <- north[,c(2,cols)]
north$nID <- paste(1:nrow(north),"N",sep="")
vectorruggedness <- rbind(north,south)
names(vectorruggedness) <- paste(names(vectorruggedness),sep="")
profilesites <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "profilpunkte",vector = "dtm_hr_vectorruggedness_Profilpunktemitboden_UTM")
profilesites <-profilesites[,c(4,cols+46)]
profilesites_vectorruggedness<- profilesites
names(profilesites_vectorruggedness) <- paste(names(profilesites_vectorruggedness),sep="")
#hr_saga
cols <- c(12:46)
south <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "hr_saga_100m_grid_SGU_south")
south <- south[,c(2,cols)]
south$nID <- paste(1:nrow(south),"S",sep="")
north <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "p2_samplevectors",vector = "hr_saga_100m_grid_SGU_north")
north <- north[,c(2,cols)]
north$nID <- paste(1:nrow(north),"N",sep="")
saga_hr <- rbind(north,south)
names(saga_hr) <- paste(names(saga_hr),sep="")
profilesites <- sqliteGRASS_delilah(location = "EPPAN_vhr", mapset= "profilpunkte",vector = "hr_saga_Profilpunktemitboden_UTM")
profilesites <-profilesites[,c(4,cols+46)]
profilesites_saga_hr<- profilesites
names(profilesites_saga_hr) <- paste(names(profilesites_saga_hr),sep="")
############

sampledata <- merge(basedata,paramscale_10m,by.x="nID",by.y="nID_10m")
sampledata <- merge(sampledata ,saga_10m,by.x="nID",by.y="nID_10m")
sampledata <- merge(sampledata ,saga_50m,by.x="nID",by.y="nID_50m")
sampledata <- merge(sampledata ,paramscale_hr,by.x="nID",by.y="nID_hr")
sampledata <- merge(sampledata ,rli_hr,by.x="nID",by.y="nID_hr")
sampledata <- merge(sampledata ,roughnessvector,by.x="nID",by.y="nID_hr")
sampledata <- merge(sampledata ,texture,by.x="nID",by.y="nID")
sampledata <- merge(sampledata ,tri,by.x="nID",by.y="nID")
sampledata <- merge(sampledata ,vectorruggedness,by.x="nID",by.y="nID")
sampledata <- merge(sampledata ,saga_hr,by.x="nID",by.y="nID")
sampledata <- sampledata[,!(names(sampledata) %in% c("ID.x","ID.y", "ID_hr.y" ,"ID_hr.x" ,"ID_10m.y" ,"ID_10m.x","ID_hr" ))]



fullmodelcols <- c(dependent,preds)
