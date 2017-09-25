require(lubridate)
require(rgdal)
require(sp)
require(maptools)
require(rgeos)
require(ggplot2)
require(ggmap)

####import GI components####
parcels<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/LandParcels.2017.csv')
names(parcels)
properties<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/Properties.2017.csv')
names(properties)
IDConnector<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/IDConnector.2017.csv')
names(IDConnector)

##import parcels shp
parcels_shp<-readOGR(dsn="C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/LandParcels.2017", layer="LandParcels.2017")
plot(parcels_shp)
proj4string(parcels_shp)

##import BG shp, translate projection to NAD83 to be consistent with parcels; note, not on the BARI Team Data Library
BG<-readOGR(dsn="C:/Users/dobrien/Documents/Research/Boston-Radcliffe/Teaching/Undergraduate Team/NRT", layer="BG_NRT")
plot(BG)
proj4string(BG)
BG.2 <- spTransform(BG, CRS("+init=epsg:4326"))


####import test file####
CRM<-read.csv("C:/Users/dobrien/Google Drive/BARI Research Team Data Library/CRM 2015/Data/Main Database 2010-2015.csv")
CRM<-CRM[!duplicated(CRM$CASE_ENQUIRY_ID),]
CRM_2014<-CRM[year(CRM$OPEN_DT)==2014,]

##There are three possible geo-identifiers.
##prop_ID (and variants) is historical and for units in the Street and Address Management (SAM) System.
##Parcel_num (and variants) is for properties in tax assessments.
##Land_Parcel_ID is for parcels (which contain one or more properties).
##Some files will only have prop_ID, though this is not standard to the GI v. 2017.
##The IDConnector file makes it possible to link these in (though it has some duplicated property_IDs that need to be removed).
##Some files with prop_ID have a prefix letter for address/intersection that has to be stripped to link properly (as seen here).

##link, aggregate CRM to parcels
CRM_2014<-merge(CRM_2014[c(1:12,22:30)],properties[c(1,16:24)],by='parcel_num',all.x=TRUE)
CRM_parcels<-aggregate(cbind(Housing,UncivilUse,PrivateNeglect)~Land_Parcel_ID,data=CRM_2014,FUN=sum)
CRM_parcels<-merge(CRM_parcels,parcels[c(1,19:21)],by='Land_Parcel_ID',all.y=TRUE)
CRM_temp<-CRM_parcels[c(1:4)]
CRM_temp[is.na(CRM_temp)]<-0
CRM_parcels<-merge(CRM_parcels[c(1,5:7)],CRM_temp,by='Land_Parcel_ID',all.x=TRUE)
summary(CRM_parcels)

####set up mapping example of Bowdoin-Geneva neighborhood####

BG_google<-get_map(location=c(left = -71.080, bottom = 42.3, right = -71.055, top = 42.312))
BG_map<-ggmap(BG_google)
BG_map

base<-BG_map + geom_path(aes(x=long, y=lat), color = 'black', data=BG.2)
base

##Spatial Join: isolates x-y of each CRM case and links to BG##
geocoded_locations<-SpatialPoints(parcels[(!is.na(parcels$X) & !is.na(parcels$Y)),6:7])
proj4string(geocoded_locations)<-proj4string(BG.2)
summary(geocoded_locations)
sp_join<-over(geocoded_locations,BG.2)
summary(sp_join)
names(sp_join)
row.names(parcels)

parcels_bg<-cbind(parcels[(!is.na(parcels$X) & !is.na(parcels$Y)),],sp_join)
parcels_bg<-parcels_bg[!is.na(parcels_bg$OBJECTID),] ##OBJECTID indicates that it linked to something in the shape file, in this case the map of Bowdoin Geneva

plot(parcels_shp[parcels_shp$Ln_P_ID %in% parcels_bg$Land_Parcel_ID,])
parcels_shp_fort<-fortify(parcels_shp, region = "Ln_P_ID") ##this will take a long time to run
parcels_shp_fort<-merge(parcels_shp_fort[1:7],CRM_parcels[c(1,5:7)],by.x='id',by.y='Land_Parcel_ID')
parcels_shp_fort<-parcels_shp_fort[order(parcels_shp_fort$order),]

base_bg<-BG_map + geom_path(aes(x=long, y=lat, group=group), color = 'black', data=BG.2) + geom_polygon(aes(x=long, y=lat, group=group, fill=PrivateNeglect), data=parcels_shp_fort[parcels_shp_fort$id %in% parcels_bg$Land_Parcel_ID & parcels_shp_fort$PrivateNeglect>0,]) + geom_path(aes(x=long, y=lat, group=group), color='blue', data=parcels_shp_fort[parcels_shp_fort$id %in% parcels_bg$Land_Parcel_ID,])
base_bg