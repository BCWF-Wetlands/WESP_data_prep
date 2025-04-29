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

#Provincial Human Disturbance Layers - compiled for CE
#Needs refinement to differentiate rural/urban and old vs young cutblocks, rangeland, etc.

#Integrate ESI samples that overlap area
#first re-assign WetlandsAll and update wet_id so st_intersect works properly
Wetlands_for_ESI<-st_read(file.path(spatialOutDir,"WetlandsAll.gpkg")) %>%
  #mutate(wet_id=seq.int(nrow(.)))
  mutate(wet_id = row_number())

#Check the id
tail(Wetlands_for_ESI)
source('02_clean_subB_ESI.R')
WetlandsESI <- st_read(file.path(spatialOutDir,"WetlandsESI.gpkg")) %>%
  mutate(wet_id=seq.int(nrow(.))) %>%
  st_set_crs(crs(bc))
tail(WetlandsESI)

WetlandsESI.check<-WetlandsESI %>%
  st_drop_geometry() %>%
  dplyr::filter(Sampled>0)

#Generate centroid version of data
wetlandsXY <- st_centroid(WetlandsESI)
wetpt <- st_coordinates(wetlandsXY)
wetpt <- wetlandsXY %>%
  cbind(wetpt) %>%
  st_drop_geometry()

WetlandsESI.pt <- st_as_sf(wetpt, coords= c("X","Y"), crs = 3005)
st_crs(WetlandsESI.pt)<-3005

#WetlandsESI.pt <- WetlandsESI.pt %>%
#  mutate(wet_id=as.numeric(rownames(WetlandsESI.pt)))
#st_crs(WetlandsESI.pt)<-3005

#clip points to AOI to make sure wetland are in AOI
WetlandsESI.pt<-WetlandsESI.pt %>% #7201 in 7143 out
  st_intersection(AOI)

WetlandsESI.1<-WetlandsESI %>% #Select 7143 wetlands
  dplyr::filter((wet_id %in% wetland.pt$wet_id) | (Sampled==1)) %>%
  mutate(wet_id=seq.int(nrow(.)))

#Check that all Sampled sites are still included
numSample<-WetlandsESI.1 %>%
  dplyr::filter(Sampled==1)

#Write out data
write_sf(WetlandsESI.pt, file.path(spatialOutDir,"WetlandsESI.pt.gpkg"))
write_sf(WetlandsESI.1, file.path(spatialOutDir,"WetlandsESI.1.gpkg"))

source('clean_SubB_PEM.R')

WetlandsAll_wPEM<- st_read(file.path(spatialOutDir,"WetlandsAll_wPEM.gpkg"))


