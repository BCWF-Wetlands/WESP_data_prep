# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#Clip wetlands by AOI
EcoP_L <- readRDS(file='tmp/EcoP_L')

#Read in and clean up geometry
WetlandsIn<-EcoP_L[[WetlandArea[1]]]
clgeo_IsValid(as(WetlandsIn,'Spatial'), verbose = FALSE) #FALSE
#unique(st_is_valid(WetlandsIn)) #TRUE

#WetlandsAll<-clgeo_Clean(as(WetlandsIn,'Spatial'), errors.only = NULL, verbose = FALSE) %>%
#  st_as_sf()
#clgeo_IsValid(as(WetlandsAll,'Spatial'), verbose = FALSE) #

#First pull out the valid geometry to speed up repairs, double check it with
#clgeo_IsValid and save
WetlandsIn_Valid<-WetlandsIn %>%
  dplyr::filter(st_is_valid(WetlandsIn)==TRUE)
clgeo_IsValid(as(WetlandsIn_Valid,'Spatial'), verbose = FALSE) #TRUE
write_sf(WetlandsIn_Valid, file.path(spatialOutDir,"WetlandsIn_Valid.gpkg"))

#Pull out dirty geometry to see what needs to be fixed
WetlandsIn_Invalid<-WetlandsIn %>%
  dplyr::filter(st_is_valid(WetlandsIn)==FALSE)
#check attributes
Wetlandsa_Invalid_no_geo <- Wetlandsa_Invalid %>%
  st_drop_geometry()
#Repair geometry, check with clgeo_IsValid and save
Wetlandsa_Invalid_fix <- Wetlandsa_Invalid %>%
  st_make_valid()
clgeo_IsValid(as(Wetlandsa_Invalid_fix,'Spatial'), verbose = FALSE) #TRUE
write_sf(Wetlandsa_Invalid_fix, file.path(spatialOutDir,"Wetlandsa_Invalid_fix.gpkg"))

#combine fixed geometry and test for validitiy one last time
Wetlandsa<- rbind(Wetlandsa_Valid,Wetlandsa_Invalid_fix) %>%
  mutate(CalibSite=0) %>%
  dplyr::select(WTLND_ID, Sampled, YearSampled,area_Ha,
                dist_to_road, Rd_Insct,
                stream_intersect,river_intersect,mmwb_intersect,lake_intersect,
                split_by_stream,stream_start,stream_end,max_stream_order, granitic_bedrock,
                BEC_BCWF=BEC,
                Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                Land_Cover, Land_Disturbance,
                fire_history, fire_year,
                parcelmap_private,
                DUC_Class, Wet_Numb,OF18,CalibSite,X50m_Buff)
clgeo_IsValid(as(Wetlandsa,'Spatial'), verbose = FALSE) #TRUE

#Pull out Boreal Plains Eco Province
Wetlandsb<-EcoP_L[[WetlandArea[2]]] %>%
  dplyr::select(WTLND_ID, Sampled, YearSampled, area_Ha,
              dist_to_road, Rd_Insct,
              stream_intersect,river_intersect,mmwb_intersect,lake_intersect,
              split_by_stream,stream_start,stream_end,max_stream_order, granitic_bedrock,
              BEC_BCWF=BEC,
              Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
              Land_Cover, Land_Disturbance,
              fire_history, fire_year,
              parcelmap_private,
              DUC_Class, Wet_Numb,OF18,CalibSite,X50m_Buff)
clgeo_IsValid(as(Wetlandsb,'Spatial'), verbose = FALSE) #TRUE

#Make Final wetlands data set
WetlandsAll<- rbind(Wetlandsa,Wetlandsb) %>%
  mutate(area_Ha=as.numeric(area_Ha))
clgeo_IsValid(as(WetlandsAll,'Spatial'), verbose = FALSE) #TRUE

wetCheck<-WetlandsAll %>%
  dplyr::filter(is.na(BEC)) #30 blank cases in SIM, 0 in Plains

#Need to set wet_id to link back to data after st_intersects
Wetlands1 <- WetlandsAll %>% #25986 wetlands
  #mutate(area_Ha=as.numeric(area_Ha)) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  rename(BEC_BCWF=BEC) #%>%
#mutate(AreaHa=as.numeric(st_area(Wetlands1)*0.0001)) %>%
  #mutate(areadif=area_Ha-AreaHa) %>% #checking original area calculation
  #eliminate 30 overlapping NA wetlands
  #dplyr::filter(!(is.na(BEC_BCWF))) #%>%
  #dplyr::select(area_Ha, AreaHa, areadif) %>%
  #st_drop_geometry()

#Wetlands1$Sampled[is.na(Wetlands1$Sampled)]<-0
Wetlands1$Sampled<-0
#Wetlands1$YearSampled[is.na(Wetlands1$YearSampled)]<-0
Wetlands1$YearSampled<-0
st_crs(Wetlands1)<-crs(bc)

#Pull out wetlands >1000 ha as a special case - eg Tagia
#Hard to type them with disturbance, land cover or even BEC due to size
#Suggest randomly sampling large wetlands - 15 in the Taiga - sample 5 randomly
Large_Wetlands<- Wetlands1 %>%
  dplyr::filter(area_Ha>=1000)
#Make Wetlands all the rest
Wetlands2 <- Wetlands1 %>%
  mutate(LargeWetland=if_else(WTLND_ID %in% Large_Wetlands$WTLND_ID, ">=1000", "<1000"))
 # dplyr::filter(!(WTLND_ID %in% Large_Wetlands$WTLND_ID))

#Generate a Wetlands point coverage
wetlandsXY <- st_centroid(Wetlands2)
wetpt <- st_coordinates(wetlandsXY)
wetpt <- wetlandsXY %>%
  cbind(wetpt) %>%
  st_drop_geometry()

wetland.pt <- st_as_sf(wetpt, coords= c("X","Y"), crs = 3005)

wetland.pt <- wetland.pt %>%
  mutate(wet_id=as.numeric(rownames(wetland.pt)))
#st_crs(wetland.pt)<-3005

#Drop wetlands whose centroids are outside of study area - there are 13 in SIM mostly on Alberta border
WetlandInAOI <- wetland.pt %>%
  st_intersection(AOI)

Wetlands3 <- Wetlands2 %>% #25981 wetlands
  dplyr::filter(WTLND_ID %in% WetlandInAOI$WTLND_ID)
Wetlands4 <- Wetlands3 %>%
  mutate(wet_id=as.numeric(rownames(Wetlands3))) %>%
  mutate(LargeWetland=if_else(WTLND_ID %in% Large_Wetlands$WTLND_ID, ">=1000", "<1000"))

st_crs(Wetlands)<-crs(bc)

#Read Wetland centroids identifying sampled wetlands
#Check how many in SIM then read and clip to AOI
BCWF_centroids<-st_read(file.path(spatialOutDirP,"BCWF_centroids.gpkg")) %>%
  dplyr::filter(grepl(paste(c("BP","TP"), collapse="|"), WTLND_ID, ignore.case = TRUE)) #51 sites
BCWF_centroids<-st_read(file.path(spatialOutDirP,"BCWF_centroids.gpkg")) %>%
  st_intersection(AOI) %>%
  mutate(Centroid_WTLND_ID=WTLND_ID) %>%
  mutate(Centroid=1)#48 sites
st_crs(BCWF_centroids)<-crs(bc)

#only 18 matches appears that centroids and wetlands have different WTLND_IDs for same wetland
WetSampled_OF<-Wetlands4 %>%
  st_drop_geometry() %>%
  #dplyr::filter(Sampled==1) %>%
  dplyr::select(WTLND_ID, YearSampled, Sampled)
WetSampled <- WetSampled_OF %>%
  full_join(BCWF_centroids, by=c("WTLND_ID"="Centroid_WTLND_ID")) %>%
  dplyr::select(WTLND_ID, Centroid, YearSampled, Sampled) %>%
  dplyr::filter(Centroid==1)
#Get full join - appears centroid and Plains Wetlands are consistent- did
#overlay and all joins WTLND_ID and BCWF_centroids were consistent.

#Further exploratioon to ensuren WTLND_IDs are consistent between data sets
BCWF_centroidsB <- BCWF_centroids %>%
  st_buffer(20)
#Fold in centroids into Wetlands polygons data by point in polygon
#wetland.pt<-st_read(file.path(spatialOutDir,"wetland.pt.gpkg"))
Wetlands5 <- Wetlands4 %>% #wetland.pt %>% # 49 matches with wetland.pts,
  # 39 with polys, 42 withh 20m buffer on BCWF_centroids, safest
  #st_buffer(100) %>%
  st_intersection(BCWF_centroidsB) %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID, Centroid_WTLND_ID, Centroid, YearSampled, Sampled) #%>%
  #full_join(WetSampled, by=c('WTLND_ID'))

CentWetsMissing<-BCWF_centroids %>%
  dplyr::filter(!(Centroid_WTLND_ID %in% Wetlands5$Centroid_WTLND_ID))
write_sf(CentWetsMissing, file.path(spatialOutDir,"CentWetsMissing.gpkg"))
WriteXLS(Wetlands5,file.path(dataOutDir,paste(WetlandArea,'_CentroidsCompare.xlsx',sep='')))

#identify WTLND_IDs that were sampled
#SampledWets<-Wetlands5$WTLND_ID #43
#reconcile location of missing wets: 'SIM_168','SIM_26061','SIM_11109' with WTLND_ID in Wetland data base
#MissingWets<-c('SIM_168','SIM_26061','SIM_11109','SIM_11098') #note 11109 was sampled 3 times - so actually 5 records
#WetsSampled<-c(SampledWets,MissingWets)

Wetlands<-Wetlands4 %>%
  mutate(Sampled=if_else(WTLND_ID %in% WetSampled$WTLND_ID, 1, 0)) %>%
  mutate(YearSampled=if_else(WTLND_ID %in% WetSampled$WTLND_ID, 2021, 0))

#only 46 since one has no data and was dropped from the Wetlands data set
# and one wetland had 2 samples
WetSampled_Test<-Wetlands %>%
  st_drop_geometry() %>%
  dplyr::filter(Sampled==1)

write_sf(Wetlands, file.path(spatialOutDir,"Wetlands.gpkg"))

wetland.pt<-wetland.pt %>% #25986
       dplyr::filter(WTLND_ID %in% Wetlands$WTLND_ID) %>%#25981
       mutate(Sampled=if_else(WTLND_ID %in% WetSampled$WTLND_ID, 1, 0)) %>%
       mutate(YearSampled=if_else(WTLND_ID %in% WetSampled$WTLND_ID, 2021, 0))

write_sf(wetland.pt, file.path(spatialOutDir,"wetland.pt.gpkg"))

#Wetlands Buffer
WetlandsB<-st_buffer(Wetlands, dist=100) %>%
  st_collection_extract("POLYGON")

#WetlandsB<-st_read(file.path(spatialOutDir,"WetlandsB.gpkg"))
#WetlandsB<-WetlandsB %>%
#  mutate(Sampled=if_else(WTLND_ID %in% WetSampled$WTLND_ID, 1, 0)) %>%
#  mutate(YearSampled=if_else(WTLND_ID %in% WetSampled$WTLND_ID, 2021, 0))

write_sf(WetlandsB, file.path(spatialOutDir,"WetlandsB.gpkg"))

#FWA_Streams
#StreasmP <-file.path(spatialOutDirP,"Streams.gpkg")
#Streams<-StreasmP %>%
#  st_intersection(AOI)
#write_sf(Streams, file.path(spatialOutDir,"Streams.gpkg"))

#FWA_Rivers
Rivers <- read_sf(file.path(spatialOutDirP,"Rivers.gpkg")) %>%
  st_intersection(AOI)
write_sf(Rivers, file.path(spatialOutDir,"Rivers.gpkg"))

#FWA_Lakes
Lakes <- read_sf(file.path(spatialOutDirP,"Lakes.gpkg")) %>%
  st_intersection(AOI)
write_sf(Lakes, file.path(spatialOutDir,"Lakes.gpkg"))

bec_sf <- st_read(file.path(DataDir,'PROVdata/BEC.gpkg')) #%>%
  #st_intersection(AOI)
write_sf(bec_sf, file.path(spatialOutDir,"bec_sf.gpkg"))

#Need updated DEM
#DEM<-  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','DEM.tif')) %>%
 # crop(AOI)
#saveRDS(DEM, file = 'tmp/AOI/DEM')

#Nation Boundary
SFN_TT<-st_read(file.path(spatialOutDirP,"SFN_TT.gpkg")) %>%
  st_intersection(AOI)
write_sf(SFN_TT, file.path(spatialOutDir,"SFN_TT.gpkg"))

#Ownership
F_OWN<-read_sf(file.path(DataDir,'PROVdata/F_OWN/F_OWN_polygon.shp')) %>%
  st_buffer(0) %>%
  st_intersection(AOI)
write_sf(F_OWN, file.path(spatialOutDir,"F_OWN.gpkg"))

#pull out private land and summarize
F_OWN_P<-F_OWN %>%
  mutate(private=if_else(OWN %in% c(40,41,52,53), 1, 0)) %>%
  dplyr::group_by(private) %>%
  dplyr::summarize(AreaHa=sum(AREA_SQM)*0.0001)
write_sf(F_OWN_P, file.path(spatialOutDir,"F_OWN_P.gpkg"))

F_OWN_PR<- fasterize(F_OWN_P,ProvRast,field='private')
F_OWN_PR[is.na(F_OWN_PR)]<-0
writeRaster(F_OWN_PR,filename=file.path(spatialOutDirP,"F_OWN_PR.tif"), format="GTiff", overwrite=TRUE)

#ws

ws <-get_layer("wsc_drainages", class = "sf") %>%
  st_intersection(AOI)
write_sf(ws, file.path(spatialOutDir,"ws.gpkg"))

gc()


#####################
#Clip DEM
#DEM_AOI<-  DEM_BC %>%
#  mask(ws_AOI) %>% #mask first so only watershed, not just bounding box that you get from crop
#  crop(ws_AOI)

#crs(DEM_AOI)<-crs(bc)
#writeRaster(DEM_AOI, filename=file.path(spatialOutDir,'DEM_AOI'), format="GTiff", overwrite=TRUE)

#AOI_R <- cded_raster(ws_AOIR)


#or use full ESI area
#ESI<-readRDS(file = 'tmp/ESI')
#select AOI
#AOI<-ESI
