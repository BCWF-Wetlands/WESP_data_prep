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

#Check BCWF_centroids
Wetlands_In<-st_read(file.path(spatialOutDir,"WetlandsAll.gpkg"))

#Read Wetland centroids identifying sampled wetlands
#Check how many in EcProvince then read and clip to AOI
BCWF_centroids<-st_read(file.path(spatialOutDirP,"BCWF_centroids.gpkg")) %>%
  dplyr::filter(grepl("SIM", WTLND_ID, ignore.case = TRUE)) #48 sites
BCWF_centroids<-st_read(file.path(spatialOutDirP,"BCWF_centroids.gpkg")) %>%
  st_intersection(AOI) %>%
  rename(Centroid_WTLND_ID=WTLND_ID) %>%
  mutate(Centroid=1)#48 sites
st_crs(BCWF_centroids)<-crs(bc)

#only 18 matches appears that centroids and wetlands have different WTLND_IDs for same wetland
WetSampled_OF<-Wetlands_In %>%
  st_drop_geometry() %>%
  dplyr::filter(Sampled==1) %>%
  dplyr::select(WTLND_ID, YearSampled, Sampled)
WetSampled <- WetSampled_OF %>%
  full_join(BCWF_centroids, by=c("WTLND_ID"="Centroid_WTLND_ID")) %>%
  dplyr::select(WTLND_ID, Centroid, YearSampled, Sampled)

#Fold in centroids into Wetlands polygons data by point in polygon
Wetlands5 <- wetland.pt %>%
  #st_buffer(100) %>%
  st_intersection(BCWF_centroids) %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID, Centroid_WTLND_ID, Centroid, YearSampled, Sampled) #%>%
  #full_join(WetSampled, by=c('WTLND_ID'))

CentWetsMissing<-BCWF_centroids %>%
  dplyr::filter(!(Centroid_WTLND_ID %in% Wetlands5$Centroid_WTLND_ID))
write_sf(CentWetsMissing, file.path(spatialOutDir,"CentWetsMissing.gpkg"))
WriteXLS(Wetlands5,file.path(dataOutDir,paste(WetlandArea,'_CentroidsCompare.xlsx',sep='')))

#identify WTLND_IDs that were sampled
SampledWets<-Wetlands5$WTLND_ID #43
#reconcile location of missing wets: 'SIM_168','SIM_26061','SIM_11109' with WTLND_ID in Wetland data base
MissingWets<-c('SIM_168','SIM_26061','SIM_11109','SIM_11098') #note 11109 was sampled 3 times - so actually 5 records
WetsSampled<-c(SampledWets,MissingWets)

Wetlands_BCWF<-Wetlands4 %>%
  mutate(Sampled=if_else(WTLND_ID %in% WetsSampled, 1, 0)) %>%
  mutate(YearSampled=if_else(WTLND_ID %in% WetsSampled, 2021, 0))

#only 46 since one has no data and was dropped from the Wetlands data set
# and one wetland had 2 samples
WetSampled_Test<-Wetlands %>%
  st_drop_geometry() %>%
  dplyr::filter(Sampled==1)

write_sf(Wetlands_BCWF, file.path(spatialOutDir,"WetlandsAll.gpkg"))
