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

VRI_file=(file.path(spatialOutDirP,'VRI_raw.gpkg'))
if (!file.exists(VRI_file)) {
  #using downloaded Provincial
  #VRI_gdb<-file.path(SpatialDir,'VEG_COMP_LYR_R1_POLY_2021.gdb')
  #VRI_list <- st_layers(VRI_gdb)
  #VRI_in <- read_sf(VRI_gdb, layer = "VEG_COMP_LYR_R1_POLY")
  #Lakes_VRI<-VRI_in %>%
  #  st_intersection(Lakes_TSA)
  #using downloaded clipped file
  VRI_gdb<-file.path(SpatialDir,'VEG_COMP_LYR_R1_POLY_2021.gdb')
  VRI_list <- st_layers(VRI_gdb)
  VRI_in <- read_sf(VRI_gdb, layer = "VEG_COMP_LYR_R1_POLY")
  saveRDS(VRI_in,file='tmp/VRI')
  VRI_in<-readRDS(file='tmp/VRI')
  VRI_raw<-VRI_in %>%
    st_intersection(AOI) %>%
    mutate(VRI_id=as.numeric(rownames(.)))
  write_sf(VRI_raw, file.path(spatialOutDir,"VRI_raw.gpkg"))
} else {
  VRI_raw<-read_sf(file.path(spatialOutDirP,"VRI_raw.gpkg"))
}

#FWA_wetlands
#FWA_wetlands_raw<-bcdc_get_data("WHSE_BASEMAPPING.FWA_WETLANDS_POLY")
FWA_wetlands_raw<-read_sf(file.path(spatialOutDirP,"FWA_wetlands.gpkg"))
FWA_wetlands<-FWA_wetlands_raw %>%
  st_intersection(AOI)
write_sf(FWA_wetlands, file.path(spatialOutDir,"FWA_wetlands.gpkg"))

#FWA_wetlands
#FWA_lakes_raw<-bcdc_get_data("WHSE_BASEMAPPING.FWA_LAKES_POLY")
FWA_lakes_raw<-read_sf(file.path(spatialOutDirP,"Lakes.gpkg"))
#FWA_lakes_raw<-Lakes
FWA_lakes<-FWA_lakes_raw %>%
  st_intersection(AOI) %>%
  mutate(water=1) %>%
  mutate(WaterType='lake') %>%
  select(water,WaterType)
write_sf(FWA_lakes, file.path(spatialOutDir,"FWA_lakes.gpkg"))

#FWA_rivers
#FWA_rivers_raw<-bcdc_get_data("WHSE_BASEMAPPING.FWA_RIVERS_POLY")
FWA_rivers_raw<-read_sf(file.path(spatialOutDirP,"Rivers.gpkg"))

#FWA_rivers_raw<-Rivers
FWA_rivers<-FWA_rivers_raw %>%
  st_intersection(AOI) %>%
  mutate(water=1) %>%
  mutate(WaterType='river') %>%
  select(water,WaterType)
write_sf(FWA_rivers, file.path(spatialOutDir,"FWA_rivers.gpkg"))

Erase_water<-rbind(FWA_lakes,FWA_rivers) %>%
  dplyr::group_by(water) %>%
  dplyr::summarize()  %>%
  st_buffer(dist=0.5)

write_sf(Erase_water, file.path(spatialOutDir,"Erase_water.gpkg"))

##Estuary Section
#Pacific Birds Estuary
#https://pacificbirds.org/2021/02/an-updated-ranking-of-british-columbias-estuaries/
PECP_Estuary<-read_sf(file.path(DataDir,'EstuaryData/PECP_Estuary_Shapefiles_PUBLIC/PECP_estuary_polys_ranked_2019_PUBLIC.shp'))
st_crs(PECP_Estuary) <- 3005
write_sf(PECP_Estuary, file.path(spatialOutDirP,"PECP_Estuary.gpkg"))

#Provincial Shorelines SHZN_SHORE_UNIT_CLASS_POLYS_SV
Shoreline<-read_sf(file.path(DataDir,'PROVdata/Shoreline/SHZN_SHORE_UNIT_CLASS_POLYS_SV/SU_CL_PY_S_polygon.shp'))
st_crs(Shoreline) <- 3005
write_sf(Shoreline, file.path(spatialOutDirP,"Shoreline.gpkg"))












