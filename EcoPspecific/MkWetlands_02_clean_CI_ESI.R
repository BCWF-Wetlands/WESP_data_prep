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

#WetlandsESI <- st_read(file.path(spatialOutDir,"WetlandsESI.gpkg"))

#Read Wetland BCWF centroids identifying sampled wetlands
#Check how many in SB then read and clip to AOI
#BCWF_centroids<-st_read(file.path(spatialOutDirP,"BCWF_centroids.gpkg")) %>%
#  dplyr::filter(grepl("SB", WTLND_ID, ignore.case = TRUE)) #0 sites also by inspection

#Read in ESI wetlands ESIWetlandsSampled
ESI_data<-st_read(file.path(DataDir,'ESIdata/ESIWetlandsSampled.gpkg'))

#Wetlands<-st_read(file.path(spatialOutDir,"Wetlands.gpkg"))
ESIdata_CI<-read_sf(file.path(DataDir,'ESIdata/ESIWetlandsSampled.gpkg')) %>%
  st_filter(AOI_EP, .predicates = st_intersects) %>%
  mutate(ESI_id=as.numeric(rownames(.))) %>%
  dplyr::rename(ESI_Territory=Territory)
  st_crs(ESIdata_CI)<-crs(bcmaps::bc_bound())
write_sf(ESIdata_CI, file.path(spatialOutDir,"ESIdata_CI.gpkg"))

#Read in CI wetlands
WetlandsAll.1<-read_sf(file.path(spatialOutDir,"WetlandsAll.gpkg"))

WetsToDrop<-WetlandsAll.1 %>%
  st_filter(ESIdata_CI, .predicates = st_intersects)
WetlandsAll.2 <- WetlandsAll.1 %>%
  dplyr::filter(!WTLND_ID %in% WetsToDrop$WTLND_ID) %>%
  dplyr::select(-c("wetland","n","area_Ha","wet_id","WTLND_ID"))
ESI_CI_inter<- ESIdata_CI %>%
  st_filter(WetlandsAllin, .predicates = st_intersects) %>%
  dplyr::select(-c('Wetland_Co','ESI_Territory','ESI_id'))
WetlandsAll<-WetlandsAll.2 %>%
  rbind(.,ESI_CI_inter) %>%
  mutate(wet_id = row_number()) %>%
  mutate(WTLND_ID=paste0(WetlandAreaShort,'_',wet_id)) %>%
  replace(is.na(.), 0)

write_sf(WetlandsAll, file.path(spatialOutDir,"WetlandsAll.gpkg"))
