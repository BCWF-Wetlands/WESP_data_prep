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

VRI_file=(file.path(spatialOutDir,'VRI_raw.gpkg'))
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
  VRI_raw<-read_sf(file.path(spatialOutDir,"VRI_raw.gpkg"))
}

#FWA_wetlands
FWA_wetlands_raw<-bcdc_get_data("WHSE_BASEMAPPING.FWA_WETLANDS_POLY")
FWA_wetlands<-FWA_wetlands_raw %>%
  st_intersection(AOI)
write_sf(FWA_wetlands, file.path(spatialOutDir,"FWA_wetlands.gpkg"))

#FWA_wetlands
FWA_lakes_raw<-bcdc_get_data("WHSE_BASEMAPPING.FWA_LAKES_POLY")
#FWA_lakes_raw<-Lakes
FWA_lakes<-FWA_lakes_raw %>%
  st_intersection(AOI) %>%
  mutate(water=1) %>%
  mutate(WaterType='lake') %>%
  select(water,WaterType)
write_sf(FWA_lakes, file.path(spatialOutDir,"FWA_lakes.gpkg"))

#FWA_rivers
FWA_rivers_raw<-bcdc_get_data("WHSE_BASEMAPPING.FWA_RIVERS_POLY")
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

#############
#FWA_Rivers
Rivers <- read_sf(file.path(BioDDir,"PROVdata/doubleline_rivers_bc/CWB_RIVERS/CWB_RIVERS.shp"))
st_crs(Rivers) <- 3005
write_sf(Rivers, file.path(spatialOutDirP,"Rivers.gpkg"))
#saveRDS(Rivers, file = 'tmp/Rivers')

#FWA_Lakes
Lakes <- read_sf(file.path(BioDDir,"PROVdata/lakes_bc/CWB_LAKES/CWB_LAKES.shp"))
st_crs(Lakes) <- 3005
write_sf(Lakes, file.path(spatialOutDirP,"Lakes.gpkg"))

#######

FWA_wetlands <- read_sf(file.path(BioDDir,"PROVdata/Wetlands/FWWTLNDSPL_polygon.shp"))
st_crs(FWA_wetlands) <- 3005
FWA_wetlands <- FWA_wetlands %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001))

#Download streams from BC Data Catalogue
Streams_F<-file.path(spatialOutDirP,"StreamsP.gpkg")
if (!file.exists(Streams_F)) {
  Streams_L<-st_layers(file.path(GISLibrary,'ProvWaterData/FWA_STREAM_NETWORKS_SP.gdb'))$name[1:246]
  Streams<-list()
  Streams<-lapply(Streams_L, function(x) read_sf(file.path(GISLibrary,'ProvWaterData/FWA_STREAM_NETWORKS_SP.gdb'), layer=x))
  names(Streams) <- Streams_L

  StreamsP <- do.call(rbind, Streams)
  saveRDS(StreamsP,file=Streams_F)
  write_sf(StreamsP, file.path(spatialOutDirP,"StreamsP.gpkg"))
} else {
  StreamsP <- read_sf(file.path(spatialOutDirP,"StreamsP.gpkg"))
}

#FWA_Streams
StreasmP <-readRDS(Streams_F)
FWA_streams<-StreamsP %>%
  st_intersection(AOI)
write_sf(FWA_streams, file.path(spatialOutDir,"FWA_streams.gpkg"))


#st_crs(ws)<-3005
#saveRDS(ws, file = 'tmp/ws')
#write_sf(ws, file.path(spatialOutDirP,"ws.gpkg"))

#Read DEM - cells are odd - need new Provincial DEM
#DEM_BC <- raster(file.path(DataDir,'WESPdata/BC_DEM/Full_Province_DEM.tif'))
#saveRDS(DEM_BC, file = 'tmp/DEM_BC')

#From bcmaps - read fails maybe a band width thing
#AOI_R <- cded_raster(ws_AOIR)
#DEM - use temporarily since 100m x 100m
#DEM<-raster(file.path(DataDir,'PROVdata/DEM/DEM.tif'))

#Current BEC - failing need faster band width

#BECin<-bcdc_get_data("WHSE_FOREST_VEGETATION.BEC_BIOGEOCLIMATIC_POLY")
#BECin<-st_read(file.path(DataDir,'PROVdata/BEC.gpkg'))

#BEC_LUT<-read_csv(file.path(SpatialDir,'v_datadeliver_tiff_bec.csv')) %>%
#  dplyr::rename(BGC_LABEL = becsubzone)

#Roads - use latest CE roads - from the Province - Rob Oostlander
#Skip and use pre-processed
#Rd_gdb <- list.files(file.path(RoadDir, "CE_Roads/2017"), pattern = ".gdb", full.names = TRUE)[1]
#fc_list <- st_layers(Rd_gdb)

# Read as sf
rd_file<-'tmp/roads_sf_in'
if (!file.exists(rd_file)) {
  #Download CE road data
CE_downloadFn("https://nrs.objectstore.gov.bc.ca/bsmheo/BC_CEF_Integrated_Roads_2021.gdb.zip",
              file.path(SpatialDir,"CE_Roads_2021.zip"))
#Unzip and put gdb in local SpatialDir
unzip(file.path(SpatialDir,"CE_Roads_2021.zip"), exdir = SpatialDir)

#Read gdb and select layer for sf_read
Roads_gdb <- list.files(file.path(SpatialDir), pattern = "_Roads_", full.names = TRUE)[1]
st_layers(file.path(Roads_gdb))

roads_sf_in <- read_sf(Roads_gdb, layer = "integrated_roads_2021")
sf::st_crs(roads_sf_in)<-sf::st_crs(ProvRast)
write_sf(roads_sf_in, file.path(spatialOutDirP,"roads_clean.gpkg"))

} else {
  roads_sf_in<-read_sf(file.path(spatialOutDirP,"roads_clean.gpkg"))
}

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



#FWA_wetlands
FWA_wetlands <- read_sf(file.path(DataDir,"PROVdata/Wetlands/FWWTLNDSPL_polygon.shp"))
st_crs(FWA_wetlands) <- 3005
FWA_wetlands <- FWA_wetlands %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001))

tt<-FWA_wetlands %>%
  dplyr::filter(area_Ha<0.0625)
write_sf(FWA_wetlands, file.path(spatialOutDirP,"FWA_wetlands.gpkg"))
#saveRDS(Rivers, file = 'tmp/Rivers')

#FWA_Rivers
Rivers <- read_sf(file.path(DataDir,"PROVdata/doubleline_rivers_bc/CWB_RIVERS/CWB_RIVERS.shp"))
st_crs(Rivers) <- 3005
write_sf(Rivers, file.path(spatialOutDirP,"Rivers.gpkg"))
#saveRDS(Rivers, file = 'tmp/Rivers')

#FWA_Lakes
Lakes <- read_sf(file.path(DataDir,"PROVdata/lakes_bc/CWB_LAKES/CWB_LAKES.shp"))
st_crs(Lakes) <- 3005
write_sf(Lakes, file.path(spatialOutDirP,"Lakes.gpkg"))
#saveRDS(waterbodies, file = 'tmp/waterbodies')

# read in the VRI data
#vri <- read_sf(WetInData, layer = 'VRI_LYRR1_181128')
#st_crs(vri) <- 3005
#saveRDS(vri, file = 'tmp/vri')

#Load Fires
Wildfire_Historical<-read_sf(file.path(DataDir,'PROVdata/Historic_Fire/H_FIRE_PLY/H_FIRE_PLY_polygon.shp'))
write_sf(Wildfire_Historical, file.path(spatialOutDirP,"Wildfire_Historical.gpkg"))
BurnSeverity<-read_sf(file.path(DataDir,'PROVdata/Burn_Severity/BURN_SEVERITY/BURN_SVRTY_polygon.shp'))
write_sf(BurnSeverity, file.path(spatialOutDirP,"BurnSeverity.gpkg"))

Fire<-Wildfire_Historical %>%
  #dplyr::filter(FIRE_YEAR>1999) %>%
  # st_union(Wildfire_2018) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(FIRE_YEAR=as.numeric(FIRE_YEAR)) %>%
  mutate(fire= case_when((FIRE_YEAR > 2016) ~ 5,
                         (FIRE_YEAR<=2015 & FIRE_YEAR >= 1991) ~ 2,
                         (FIRE_YEAR<=1990) ~ 1,
                         TRUE ~ 0))

FireR<- fasterize(Fire,ProvRast,field='fire')
FireR[is.na(FireR)]<-0
writeRaster(FireR,filename=file.path(spatialOutDirP,"FireR.tif"), format="GTiff", overwrite=TRUE)

#Load Nation Boundary
SFN_TT <- read_sf(file.path(DataDir,"WESPdata/FirstNations/SFN_TT/SFN_TT.shp"))
st_crs(SFN_TT) <- 3005
write_sf(SFN_TT, file.path(spatialOutDirP,"SFN_TT.gpkg"))

#Load Nation AOI
McLeodLakeFN_50km <- read_sf(file.path(DataDir,"WESPdata/FirstNations/McLeodLake_50km.gpkg"))
st_crs(McLeodLakeFN_50km) <- 3005
write_sf(McLeodLakeFN_50km, file.path(spatialOutDirP,"McLeodLakeFN_50km.gpkg"))

NakazdliCabins_50km <- read_sf(file.path(DataDir,"WESPdata/FirstNations/NakazdliCabins_50km.gpkg"))
st_crs(NakazdliCabins_50km) <- 3005
write_sf(NakazdliCabins_50km, file.path(spatialOutDirP,"NakazdliCabins_50km.gpkg"))

FtStJames_50K <- read_sf(file.path(DataDir,"WESPdata/FirstNations/FtStJames_50K.gpkg"))
st_crs(FtStJames_50K) <- 3005
write_sf(FtStJames_50K, file.path(spatialOutDirP,"FtStJames_50K.gpkg"))

MoberlyLake_50K <- read_sf(file.path(DataDir,"WESPdata/FirstNations/MoberlyLake_50K.gpkg"))
st_crs(MoberlyLake_50K) <- 3005
write_sf(MoberlyLake_50K, file.path(spatialOutDirP,"MoberlyLake_50K.gpkg"))

TeaSpot <- read_sf(file.path(DataDir,"WESPdata/FirstNations/TeaSpot.gpkg"))
st_crs(TeaSpot) <- 3005
write_sf(TeaSpot, file.path(spatialOutDirP,"TeaSpot.gpkg"))

#F_OWN
F_OWN<-read_sf(file.path(DataDir,'PROVdata/F_OWN/F_OWN_polygon.shp'))
write_sf(F_OWN, file.path(spatialOutDirP,"F_OWN.gpkg"))

#Pacific Birds Estuary
#https://pacificbirds.org/2021/02/an-updated-ranking-of-british-columbias-estuaries/
PECP_Estuary<-read_sf(file.path(DataDir,'EstuaryData/PECP_Estuary_Shapefiles_PUBLIC/PECP_estuary_polys_ranked_2019_PUBLIC.shp'))
st_crs(PECP_Estuary) <- 3005
write_sf(PECP_Estuary, file.path(spatialOutDirP,"PECP_Estuary.gpkg"))

#Provincial Shorelines SHZN_SHORE_UNIT_CLASS_POLYS_SV
Shoreline<-read_sf(file.path(DataDir,'PROVdata/Shoreline/SHZN_SHORE_UNIT_CLASS_POLYS_SV/SU_CL_PY_S_polygon.shp'))
st_crs(Shoreline) <- 3005
write_sf(Shoreline, file.path(spatialOutDirP,"Shoreline.gpkg"))


