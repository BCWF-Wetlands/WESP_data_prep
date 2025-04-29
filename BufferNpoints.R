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

#BuffersNpoints.R
Wetlands<-Wetlands1
#Generate centroid version of data
wetlandsXY <- st_centroid(Wetlands)
wetpt <- st_coordinates(wetlandsXY)
wetpt <- wetlandsXY %>%
  cbind(wetpt) %>%
  st_drop_geometry()

wetland.pt <- st_as_sf(wetpt, coords= c("X","Y"), crs = 3005)

wetland.pt1 <- wetland.pt
st_crs(wetland.pt)<-3005

#clip points to AOI to make sure wetland are entirely in AOI
wetland.pt<-wetland.pt1 %>%
  st_intersection(AOI)

wetChewc<-Wetlands %>%
  dplyr::filter(Sampled==1)

#If there are already sampled sites that need to be included
#Field2023DataCheck<-Field2023Data %>%
#  dplyr::filter(Sampled==1 & YearSampled<2023)
#drop wetlands on border if have not been sampled
#Wetlands <- Wetlands.1 %>% #42709
#  dplyr::filter((WTLND_ID %in% wetland.pt$WTLND_ID) | Sampled==1) %>%
#  mutate(wet_id=as.numeric(rownames(.)))
#clgeo_IsValid(as(Wetlands,'Spatial'), verbose = FALSE)
#sp.clean <- clgeo_Clean(as(Wetlands,'Spatial'))
#clgeo_IsValid(sp.clean, verbose = FALSE)
#Wetlands<-st_as_sf(sp.clean)

#Generate final centroid version of data with proper wet_id
wetlandsXY <- st_centroid(Wetlands)
wetpt <- st_coordinates(wetlandsXY)
wetpt <- wetlandsXY %>%
  cbind(wetpt) %>%
  st_drop_geometry()

wetland.pt <- st_as_sf(wetpt, coords= c("X","Y"), crs = 3005)

#Wetlands Buffer - this can take a while
WetlandsB<-st_buffer(Wetlands, dist=100) %>%
  st_collection_extract("POLYGON")

