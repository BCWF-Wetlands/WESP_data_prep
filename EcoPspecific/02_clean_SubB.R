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

#Read in  and clean up geometry
WetlandsIn<-read_sf(file.path(DataDir,'WESPdata/Wetlands/Sub_Boreal/Sub_Boreal.gpkg'))
clgeo_IsValid(as(WetlandsIn,'Spatial'), verbose = FALSE) #FALSE
unique(st_is_valid(WetlandsIn)) #TRUE

WetlandsAll<-clgeo_Clean(as(WetlandsIn,'Spatial'), errors.only = NULL, verbose = FALSE) %>%
  st_as_sf() %>%
  st_set_crs(3005)
clgeo_IsValid(as(WetlandsAll,'Spatial'), verbose = FALSE) #

saveRDS(WetlandsIn, file='tmp/WetlandsIn')
saveRDS(WetlandsAll, file='tmp/WetlandsAll')

#Run clean routines to fix dirty geometry and read in clean geometry if needed
#source('02_clean_geometry')
#Start of clean geometry section
WetlandsAll<-readRDS(file='tmp/WetlandsAll') %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) #%>%
  #drop small 'wetlands'?? on inspection appear to be mostly valid
  #dplyr::filter(area_Ha<0.25) %>%
  mutate(wet_id=seq.int(nrow(.))) %>%
  #dplyr::rename(BEC_BCWF=BEC) %>%
  #dplyr::filter(is.na(BEC))
write_sf(WetlandsAll, file.path(spatialOutDir,"WetlandsAll.gpkg"))

#Run routines to incorporate FWCP wetlands
#source('02_clean_FWCP')
WetlandsAll_wPEM <- st_read(file.path(spatialOutDir,"WetlandsAll_wPEM.gpkg"))

#Read in FWCP wetlands and append to sub boreal data set
#tt<-rbind(WetlandsAll,fwcp_splineB)

#Pull out wetlands >1000 ha as a special case - eg Tagia
#Hard to type them with disturbance, land cover or even BEC due to size
#Suggest randomly sampling large wetlands - 15 in the Taiga - sample 5 randomly
Large_Wetlands<- WetlandsAll_wPEM %>%
  dplyr::filter(area_Ha>=1000)

#Make Wetlands all the rest
Wetlands1 <- WetlandsAll_wPEM %>%
  mutate(LargeWetland=if_else(WTLND_ID %in% Large_Wetlands$WTLND_ID, ">=1000", "<1000"))

Wetlands1$Sampled<-0
#Wetlands1$YearSampled[is.na(Wetlands1$YearSampled)]<-0
Wetlands1$YearSampled<-0
st_crs(Wetlands1)<-crs(bc)

#Integrate ESI samples that overlap area
write_sf(Wetlands1, file.path(spatialOutDir,"Wetlands1.gpkg"))
#source (02_clean_SubB_ESI.R)
WetlandsESI <- st_read(file.path(spatialOutDir,"WetlandsESI.gpkg"))

#Generate centroid version of data
wetlandsXY <- st_centroid(WetlandsESI)
wetpt <- st_coordinates(wetlandsXY)
wetpt <- wetlandsXY %>%
  cbind(wetpt) %>%
  st_drop_geometry()

wetland.pt <- st_as_sf(wetpt, coords= c("X","Y"), crs = 3005)

wetland.pt <- wetland.pt %>%
  mutate(wet_id=as.numeric(rownames(wetland.pt)))
st_crs(wetland.pt)<-3005

#clip points to AOI to make sure wetland are in AOI
wetland.pt<-wetland.pt %>% #7201 in 7143 out
  st_intersection(AOI)

Wetlands<-WetlandsESI %>% #Select 7143 wetlands
  dplyr::filter((WTLND_ID %in% wetland.pt$WTLND_ID) | (Sampled==1)) %>%
  mutate(wet_id=seq.int(nrow(.)))


#Check that all Sampled sites are still included
numSample<-Wetlands %>%
  dplyr::filter(Sampled==1) #63

#Write out data
write_sf(wetland.pt, file.path(spatialOutDir,"wetland.pt.gpkg"))
write_sf(Wetlands, file.path(spatialOutDir,"Wetlands.gpkg"))

#Wetlands Buffer
WetlandsB<-st_buffer(Wetlands, dist=100) %>%
  st_collection_extract("POLYGON")

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
writeRaster(F_OWN_PR,filename=file.path(spatialOutDir,"F_OWN_PR.tif"), format="GTiff", overwrite=TRUE)

#ws

ws <-get_layer("wsc_drainages", class = "sf") %>%
  st_intersection(AOI)

gc()

