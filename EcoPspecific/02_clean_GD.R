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

#Read in EcoProvince data, check validity of geometry and repair if needed
WetlandsIn<-EcoP_L[[WetlandArea[1]]]

WetlandsAll<-clgeo_Clean(as(WetlandsIn,'Spatial'), errors.only = NULL, verbose = FALSE) %>%
  st_as_sf() %>%
  mutate(area_Ha=as.numeric(area_Ha))
  st_crs(WetlandsAll)<-3005
clgeo_IsValid(as(WetlandsAll,'Spatial'), verbose = FALSE) #

saveRDS(WetlandsIn, file='tmp/WetlandsIn')
saveRDS(WetlandsAll, file='tmp/WetlandsAll')

#Run clean routines to fix dirty geometry and read in clean geometry if needed
#source('02_clean_geometry')

#Start of clean geometry section
WetlandsAll<-readRDS(file='tmp/WetlandsAll') %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  dplyr::rename(BEC_BCWF=BEC)

wetCheck<-WetlandsAll %>%
  dplyr::filter(is.na(BEC_BCWF))
#1 blank cases in GD, which was Sampled according to the centroid file - GD_28685

#Pull out wetlands >1000 ha as a special case - eg Tagia
#Hard to type them with disturbance, land cover or even BEC due to size
#Suggest randomly sampling large wetlands - 15 in the Taiga - sample 5 randomly
Large_Wetlands<- WetlandsAll %>%
  dplyr::filter(area_Ha>=1000)

#Make Wetlands all the rest
Wetlands1 <- WetlandsAll %>%
  mutate(wet_id=as.numeric(rownames(WetlandsAll))) %>%
  mutate(LargeWetland=if_else(WTLND_ID %in% Large_Wetlands$WTLND_ID, ">=1000", "<1000"))

#Read in centroid data to get what was sampled in 2021
BCWF_centroids<-st_read(file.path(spatialOutDirP,"BCWF_centroids.gpkg")) %>%
  dplyr::filter(grepl("GD", WTLND_ID, ignore.case = TRUE)) #63 sites

#Check that al the GD sites are in the GD
BCWF_centroids<-st_read(file.path(spatialOutDirP,"BCWF_centroids.gpkg")) %>%
  st_intersection(AOI) %>%
  dplyr::rename(Centroid_WTLND_ID=WTLND_ID) %>%
  mutate(Centroid=1)#also 63 sites
st_crs(BCWF_centroids)<-crs(bcmaps::bc_bound())
BCWF_centroids_data<-st_drop_geometry(BCWF_centroids)

#Join to Wetlands and re-label the OF Sampled and YearSampled
Wetlands2 <- Wetlands1 %>%
  left_join(BCWF_centroids_data, by=c("WTLND_ID"="Centroid_WTLND_ID")) %>%
  mutate(Sampled_OF=Sampled) %>%
  mutate(YearSampled_OF=YearSampled) %>%
  mutate(Sampled=Centroid) %>%
  mutate(YearSampled=if_else(Sampled==1, 2021,0))

numSample<-Wetlands2 %>%
  dplyr::filter(Sampled==1) #63
#Site GD_28685 was sampled but not in Wetlands file???
write_sf(numSample, file.path(spatialOutDir,"numSample.gpkg"))
saveRDS(Wetlands2, file='tmp/Wetlands2')

#Check OF and centroid data
#mapview(numSample)+mapview(AOI)
#source('02_clean_Compare_OF_Centroids')
#Wetlands2<-readRDS(file='tmp/Wetlands2')

#Generate centroid version of data
wetlandsXY <- st_centroid(Wetlands2)
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

Wetlands<-Wetlands2 %>% #Select 7143 wetlands
 dplyr::filter((WTLND_ID %in% wetland.pt$WTLND_ID) | (Sampled==1))
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
writeRaster(F_OWN_PR,filename=file.path(spatialOutDirP,"F_OWN_PR.tif"), format="GTiff", overwrite=TRUE)

#ws

ws <-get_layer("wsc_drainages", class = "sf") %>%
  st_intersection(AOI)

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
