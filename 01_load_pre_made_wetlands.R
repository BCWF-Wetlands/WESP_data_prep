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

#Make lapply to loop through layers and pull out EcoProvinces
#wetlands_SIM<-read_sf(file.path(DataDir,'WESPdata/Wetlands/WESP_Office_Wetlands.gpkg'), layer='SIM_Base')
Wet_EcoP_L<-st_layers(file.path(DataDir,'WESPdata/Wetlands/WESP_Office_Wetlands.gpkg'))$name
EcoP_L<-list()
EcoP_L<-lapply(Wet_EcoP_L, function(x) read_sf(file.path(DataDir,'WESPdata/Wetlands/WESP_Office_Wetlands.gpkg'), layer=x))
names(EcoP_L) <- Wet_EcoP_L
saveRDS(EcoP_L, file='tmp/EcoP_L')

#Read BCWF Wetland centroids
BCWF_centroids<-read_sf(file.path(DataDir,'WESPdata/Wetlands/Centroids_WESP_OF_Wetlands/Centroids_WESP_OF_Wetlands.shp'))
write_sf(BCWF_centroids, file.path(spatialOutDirP,"BCWF_centroids.gpkg"))

#Read BCWF Wetland Estuary polygons sampled in 2021 - New File Geodatabase.gdb
#NewFile_gdb <- list.files(file.path(DataDir,'WESPdata/Wetlands'), pattern = ".gdb", full.names = TRUE)[1]
#File_list <- st_layers(NewFile_gdb)
# Read as sf
#BCWF_Estuary_2021 <- read_sf(NewFile_gdb, layer = "WESP_Tidal_Shorelines_assessed2021", as_tibble=FALSE) %>%
  #st_zm(drop=TRUE) %>%
#  st_cast('POLYGON') %>%
#  st_as_sf()
BCWF_Estuary_2021 <- read_sf(file.path(DataDir,'WESPdata/BCWF_Estuary_2021.gpkg')) %>%
  st_zm()
st_crs(BCWF_Estuary_2021) <- 3005

write_sf(BCWF_Estuary_2021, file.path(spatialOutDirP,"BCWF_Estuary_2021.gpkg"))

#Wetlands<-read_sf(file.path(DataDir,'WESPdata/Wetlands/WESP_Office_Wetlands.gpkg'),
#                  layer="SIM_Base", fid_column_name='wet_id', promote_to_multi=TRUE)
#Wetlands<-EcoP_L[[WetlandArea]]
#Wetlands_SIM<-read_sf(file.path(DataDir,'WESPdata/Wetlands/SIM_wetlands.shp'),
#                      fid_column_name='wet_id', promote_to_multi=TRUE)
#sf::st_write(Wetlands_SIM, file.path(spatialOutDir,"Wetlands_SIM.gpkg"), delete_layer = TRUE)
#Wetlands1<- Wetlands_SIM %>%
#  dplyr::select(wet_id)
#sf::st_write(Wetlands, file.path(spatialOutDir,"Wetlands.gpkg"), delete_layer = TRUE)



