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
#Wetlands<-st_read(file.path(spatialOutDir,"Wetlands.gpkg"))
ESIdata_SB<-read_sf(file.path(DataDir,'ESIdata/ESIWetlandsSampled.gpkg')) %>%
  st_intersection(AOI) %>%
  mutate(ESI_id=as.numeric(rownames(.))) %>%
  dplyr::rename(ESI_Territory=Territory)
  st_crs(ESIdata_SB)<-crs(bc)
write_sf(ESIdata_SB, file.path(spatialOutDir,"ESIdata_SB.gpkg"))

#if previously processed, but needed for updated wetlands
ESIdata_SB<-st_read(file.path(spatialOutDir,"ESIdata_SB.gpkg"))

ESIdata_SB_ng <- ESIdata_SB %>% st_drop_geometry()

#
ESIdata <- ESIdata_SB %>%
  st_intersects(Wetlands_for_ESI) %>%
  tibble::enframe(name = 'ESI_id', value = 'wet_id') %>%
  tidyr::unnest(wet_id) %>%
  left_join(ESIdata_SB_ng, by=c("ESI_id"))

WetlandsESI<-Wetlands_for_ESI %>%
  left_join(ESIdata, by=c("wet_id")) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

WetSampled_Test<-WetlandsESI %>%
  st_drop_geometry() %>%
  dplyr::filter(Sampled==1)

write_sf(WetlandsESI, file.path(spatialOutDir,"WetlandsESI.gpkg"))
WetlandsESI<-st_read(file.path(spatialOutDir,"WetlandsESI.gpkg"))
table(WetlandsESI$YearSampled)

#missing 0 with updated wetland layer, was 4 with BCWF wetlands
ESImissing <- ESIdata_SB %>%
  dplyr::filter(!(ESI_id %in% ESIdata$ESI_id))
write_sf(ESImissing, file.path(spatialOutDir,"ESImissing.gpkg"))
#4 small wetlands not in BCWF wetland coverage
# could be processed as part of Office work and still be part of the 100'


tt<-Wetlands_for_ESI %>%
  dplyr::filter(wet_id==12776)
