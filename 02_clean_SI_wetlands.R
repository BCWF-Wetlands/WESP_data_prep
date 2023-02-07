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

#WetlandsAll<-st_read(file.path(spatialOutDirDesign,paste0("SampleStrata_",WetlandAreaShort,".gpkg"))) #%>%

#wetlands can come in 3 types:
#1. Wetlands newly made and have no design attributes
#2. pre-made wetlands with no sampling info
#3. old wetlands with sampling info

WetlandsAllin<-st_read(file.path(spatialOutDir,"WetlandsAll.gpkg"))

OK_gdb<-file.path(SpatialDir,'SI_wetlands/Okanagan_Wetlands.gdb')
OK_list <- st_layers(OK_gdb)
OK_in <- read_sf(OK_gdb, layer = "Okanagan_Wetlands")
OK_in <- vect(OK_gdb, layer = "Okanagan_Wetlands")
saveRDS(OK_in,file='tmp/OK_in')
OK_in<-readRDS(file='tmp/OK_in')
OK_raw<-OK_in %>%
  terra::project(crs(ProvRast)) %>%
  sf::st_as_sf() %>%
  st_intersection(AOI) %>%
  mutate(OK_id=as.numeric(rownames(.)))
write_sf(OK_raw, file.path(spatialOutDir,"OK_raw.gpkg"))

#Intersection of wetlands
OK_intersect <- OK_raw %>%
  st_intersects(WetlandsAll) %>%
  as.data.frame() %>%
  group_by(row.id) %>%
  dplyr::summarize(n=n())


#
OK_AOI <- st_convex_hull(st_union(st_centroid(OK_raw)))
qtm(OK_AOI)
write_sf(OK_AOI, file.path(spatialOutDir,"OK_AOI.gpkg"))


