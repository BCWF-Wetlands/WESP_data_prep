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

#Pacific Birds Estuary polygons
PECP_Estuary<-read_sf(file.path(spatialOutDirP,"PECP_Estuary.gpkg"))
PECP_EstuaryG<-PECP_Estuary %>%
  mutate(Source_id=as.numeric(rownames(PECP_Estuary))) %>%
  mutate(Sampled=0) %>%
  mutate(Source='PECP') %>%
  mutate(Type=paste0('rank_',IMP_CL2019)) %>% #rank
  dplyr::select(Source_id,Source,Sampled, Type) #%>%
  #st_cast("MULTIPOLYGON") %>%
  #st_cast("POLYGON", do_split = FALSE)
NextID<-max(PECP_EstuaryG$Source_id)
write_sf(PECP_EstuaryG, file.path(spatialOutDir,"PECP_EstuaryG.gpkg"))

#Shoreline Data
Shoreline<-read_sf(file.path(spatialOutDirP,"Shoreline.gpkg")) %>%
  #st_intersection(AOI) %>%
  #pull out relevant units from shoreine
  dplyr::filter(RP_TYP_NM %in% c("Estuary, Marsh or Lagoon","Mud Flat","Sand Flat"))
ShorelineG<-Shoreline %>%
  mutate(Source_id=(NextID+as.numeric(rownames(Shoreline)))) %>%
  mutate(Sampled=0) %>%
  mutate(Source='Shoreline') %>%
  mutate(Type=RP_TYP_NM) %>%
  dplyr::select(Source_id,Source,Sampled, Type) #%>%
#st_cast("MULTIPOLYGON") %>%
  #st_cast("POLYGON", do_split = FALSE)
NextID<-max(ShorelineG$Source_id)
write_sf(ShorelineG, file.path(spatialOutDir,"ShorelineG.gpkg"))

#BCWF Estuary 2021 samples
BCWF_Estuary_2021<-read_sf(file.path(spatialOutDirP,"BCWF_Estuary_2021.gpkg"))
BCWF_Estuary_2021G<-BCWF_Estuary_2021 %>%
  mutate(Source_id=(NextID+as.numeric(rownames(BCWF_Estuary_2021)))) %>%
  mutate(Sampled=1) %>%
  mutate(Source='BCWF') %>%
  mutate(Type=Name) %>%
  dplyr::select(Source_id,Source,Sampled, Type) #%>%
#st_cast("MULTIPOLYGON") %>%
  #st_cast("POLYGON", do_split = FALSE)
write_sf(BCWF_Estuary_2021G, file.path(spatialOutDir,"BCWF_Estuary_2021G.gpkg"))

No_Samples<-BCWF_Estuary_2021G %>%
  dplyr::filter(Sampled==1)

#mapview(ShorelineG) + mapview(PECP_EstuaryG) + mapview(BCWF_Estuary_2021G)

#Generate an Estuary data set
#PECP_Estuary, Shoreline, BCWF_Estuary_2021
#Order poygons, 1) BCWF, 2) PECP, 3) Shoreline
#First clip PECP from Shoreline
ShorelineGC<-ShorelineG %>%
  st_difference(st_union(PECP_EstuaryG))
st_geometry_type(ShorelineGC, by_geometry = TRUE)

#Combine the clipped Shoreline with full PECP_Estuary - pick up shoreline polys not
# in PECP data
PECP_Shore<-rbind(PECP_EstuaryG,ShorelineGC)
#st_geometry_type(PECP_Shore, by_geometry = TRUE)
mapview(PECP_Shore)+mapview(ShorelineGC)+mapview(PECP_EstuaryG)+mapview(ShorelineG)
write_sf(PECP_Shore, file.path(spatialOutDir,"PECP_Shore.gpkg"))

#Next clip PECP and Shore from BCWF
BCWF_PECP_ShoreC<-PECP_Shore %>%
  st_difference(st_union(BCWF_Estuary_2021G))
#st_geometry_type(BCWF_PECP_ShoreC, by_geometry = TRUE)

#Combine the clipped PECP and Shoreline with full BCWF_Estuary -
# pick up shoreline and PECP polys not in BCWF data
BCWF_PECP_Shore<-rbind(BCWF_Estuary_2021G,BCWF_PECP_ShoreC) %>%
  #remove small linestring and polys
  dplyr::filter(as.numeric(st_area(.)/10000)>1)  %>%
  #Cast so polygon throughout
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON", do_split = TRUE) %>%
  mutate(est_id=as.numeric(rownames(.))) %>%
  mutate(area_Ha=as.numeric(st_area(.)/10000)) %>%
  dplyr::filter(area_Ha>1) #213

#Check Geometry - all polygon
st_geometry_type(BCWF_PECP_Shore, by_geometry = TRUE)
mapview(BCWF_PECP_Shore)
write_sf(BCWF_PECP_Shore, file.path(spatialOutDir,"BCWF_PECP_Shore.gpkg"))

#Remove slivers and clean up data set
#Put a 10m buffer around each small estuary, then remove adjacent to larger unit
# typically shore line units extend beyond Pacific Birds, defer to the birds since
# more rigorous investigation of estuary boundaries, but keep entire BCWF Estuary
Estuary_drop1<-BCWF_PECP_Shore %>%
  dplyr::filter(area_Ha<=5) %>%
  st_buffer(10) %>%
  mutate(drop_id=as.numeric(rownames(.))) #128

Estuary_keep<-BCWF_PECP_Shore %>%
  dplyr::filter(!est_id %in% Estuary_drop1$est_id)

Est_to_drop<-Estuary_drop1 %>%
  #Id small estuaries buffers that intersect(adjacent to) large units
  mutate(est_id=est_id, Keep=lengths(st_intersects(.,Estuary_keep)) > 0) %>%
  st_drop_geometry() %>%
  dplyr::filter(Keep==TRUE) %>%
  dplyr::select(est_id) %>%
  group_by(est_id) %>% filter(! duplicated(est_id)) #42

Estuary<- BCWF_PECP_Shore %>%
  st_intersection(AOI) %>%
  dplyr::filter(!est_id %in% Est_to_drop$est_id  | Sampled==1)

No_Samples<-Estuary %>%
  dplyr::filter(Sampled==1)

write_sf(Estuary, file.path(spatialOutDir,"Estuary.gpkg"))

Large_Wetlands<- Estuary %>%
  dplyr::filter(area_Ha>=1000)

Wetlands2<-Estuary %>%
  mutate(WTLND_ID=est_id) %>%
  mutate(wet_id=as.numeric(rownames(.))) %>%
  mutate(LargeWetland=if_else(WTLND_ID %in% Large_Wetlands$est_id, ">=1000", "<1000")) %>%
  mutate(YearSampled=if_else(Sampled==1, 2021, 0))

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

#Water
Rivers<-read_sf(file.path(spatialOutDir,"Rivers.gpkg"))
Lakes<-read_sf(file.path(spatialOutDir,"Lakes.gpkg"))
#BEC
bec_sf<-read_sf(file.path(spatialOutDir,"bec_sf.gpkg"))

#ws
ws <-get_layer("wsc_drainages", class = "sf") %>%
  st_intersection(AOI)

#Ownership
F_OWN<-read_sf(file.path(spatialOutDirP,"F_OWN.gpkg")) %>%
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
