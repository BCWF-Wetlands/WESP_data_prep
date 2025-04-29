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

source('01_Load_Fns.R')

#Clip the layers to EcoP
ProvRast<-raster(file.path(spatialOutDirP,'ProvRast.tif'))

#Files that are to large to download Provincial versions
# First get the DEM for EcoP and calculate aspect
DEM_file <- file.path(spatialOutDir,paste0('DEMtp_',WetlandAreaShort,'.tif'))
if (!file.exists(DEM_file)) {
  AOIbuffr<-read_stars(file.path(spatialOutDir,'AOIbuffr.tif'))
  DEM<-bcmaps::cded_stars(aoi = AOIbuffr) #crs=4269
  write_stars(DEM,dsn=file.path(spatialOutDir,paste0('DEM_',WetlandAreaShort,'.tif')))
  DEM.t<-terra::rast(file.path(spatialOutDir,paste0('DEM_',WetlandAreaShort,'.tif')))
  crs(DEM.t, proj=TRUE)
  DEM.tp<-terra::project(DEM.t,crs(bcmaps::bc_bound()))
  writeRaster(DEM.tp, filename=file.path(spatialOutDir,paste0('DEMtp_',WetlandAreaShort,'.tif')), overwrite=TRUE)

} else
DEM.tp<-rast(file.path(spatialOutDir,paste0('DEMtp_',WetlandAreaShort,'.tif')))

# Get OG seral stage, CDC occurence and SARA critical habitat since too large to download provincial version
OG_file <- file.path(spatialOutDir,'Old_GrowthSSP.gpkg')
if (!file.exists(OG_file)) {
  Old_GrowthSSP <- bcdc_query_geodata("WHSE_FOREST_VEGETATION.OGSR_TAP_SERAL_STAGE_SP") %>%
    filter(INTERSECTS(AOIbuff)) %>%
  collect()
st_crs(Old_GrowthSSP)<-3005
write_sf(Old_GrowthSSP,file.path(spatialOutDir,'Old_GrowthSSP.gpkg'),delete_layer=TRUE)

CDC_occur <- bcdc_query_geodata("WHSE_TERRESTRIAL_ECOLOGY.BIOT_OCCR_NON_SENS_AREA_SVW") %>%
  filter(INTERSECTS(AOIbuff)) %>%
  collect()
st_crs(CDC_occur)<-3005
write_sf(CDC_occur,file.path(spatialOutDir,'CDC_occur.gpkg'),delete_layer=TRUE)

SARA <- bcdc_query_geodata("WHSE_WILDLIFE_MANAGEMENT.WCP_CRITICAL_HABITAT_SP") %>%
  filter(INTERSECTS(AOIbuff)) %>%
  collect()
st_crs(SARA)<-3005
write_sf(SARA,file.path(spatialOutDir,'SARA.gpkg'),delete_layer=TRUE)
} else {
Old_GrowthSSP<-st_read(file.path(spatialOutDir,'Old_GrowthSSP.gpkg'))
CDC_occur<-st_read(file.path(spatialOutDir,'CDC_occur.gpkg'))
SARA<-st_read(file.path(spatialOutDir,'SARA.gpkg'))
}

#test if clipping is done, if yes then read from disk
VRI_file <- file.path(spatialOutDir,"VRI.gpkg")
if (!file.exists(VRI_file)) {
  VRIP<-read_sf(file.path(spatialOutDirP,"VRIP.gpkg"))
  #VRI_inP<-read_sf(file.path(spatialOutDirP,"VRIP_QGIS.gpkg"))
  VRI_raw<-VRIP %>%
    st_intersection(AOIbuff) %>%
    mutate(VRI_id=as.numeric(rownames(.)))
  write_sf(VRI_raw, file.path(spatialOutDir,"VRI_raw.gpkg"))

VRI_raw<-st_read(file.path(spatialOutDir,"VRI_raw.gpkg"))

VRI_SIR<-fasterize(VRI_raw,ProvRast,field='SITE_INDEX')
writeRaster(VRI_SIR,file.path(spatialOutDir,'VRI_SIR.tif'),overwrite=TRUE)
} else {
VRI_SIR<-raster(spatialOutDir,'VRI_SIR.tif')
}

LC_file <- file.path(spatialOutDir,"LandCover.tif")
if (!file.exists(LC_file)) {
  #raster clip
  LandCover<-rast(landcoverP) %>%
    terra::crop(AOIbuff)
  writeRaster(LandCover,file.path(spatialOutDir,'LandCover.tif'),overwrite=TRUE)
  LandForm<-rast(file.path(spatialOutDirP,'LandFormP.tif')) %>%
    terra::crop(AOIbuff)
  writeRaster(LandForm,file.path(spatialOutDir,'LandForm.tif'),overwrite=TRUE)
  Disturb<-rast(file.path(spatialOutDirP,'disturbance_sfRP.tif')) %>%
    terra::crop(AOIbuff)
  writeRaster(Disturb,file.path(spatialOutDir,'Disturb.tif'), overwrite=TRUE)
  DisturbU<-unique(Disturb)
  FireR<-rast(file.path(spatialOutDirP,"FireR.tif")) %>%
    terra::crop(AOIbuff)
  writeRaster(FireR, filename=file.path(spatialOutDir,'FireR.tif'), overwrite=TRUE)
  roadsSR<-rast(file.path(spatialOutDirP,'roadsSRP.tif')) %>%
    terra::crop(AOIbuff)
  writeRaster(roadsSR, filename=file.path(spatialOutDir,'roadsSR.tif'), overwrite=TRUE)
  roadsDist<-rast(file.path(spatialOutDirP,'roadsDistP.tif')) %>%
    terra::crop(AOIbuff)
  writeRaster(roadsDist, filename=file.path(spatialOutDir,'roadsDist.tif'), overwrite=TRUE)
  dwell_R<-rast(file.path(spatialOutDirP,'dwellP_R.tif')) %>%
   terra::crop(AOI)
   writeRaster(dwell_R, filename=file.path(spatialOutDir,'dwell_R.tif'), overwrite=TRUE)
  residence_R<-rast(file.path(spatialOutDirP,'residenceP_R.tif')) %>%
    terra::crop(AOIbuff)
  writeRaster(residence_R, filename=file.path(spatialOutDir,'residence_R.tif'), overwrite=TRUE)
  Bedrockr<-rast(file.path(spatialOutDirP,'BedrockrP.tif')) %>%
    terra::crop(AOIbuff)
  writeRaster(Bedrockr, filename=file.path(spatialOutDir,'Bedrockr.tif'), overwrite=TRUE)

  #Generic Vector Clip
  #?census<-ClipFn('censusP')
  FWA_ASS_WSin<-ClipFn('FWA_ASS_WSP')
  roads_sf<-ClipFn('roads_sfP')
  Fire2010<-ClipFn('Fire2010P')
  F_OWN<-ClipFn('F_OWNP')
  BEC<-ClipFn('BECP')
  karst<-ClipFn('karstP')
  communities<-ClipFn('communitiesP')
  BGCprotected<-ClipFn('BGCprotectedP')
  GeoF<-ClipFn('GeoFP')
  Bedrock<-ClipFn('BedrockP')
  Fish_Observe<-ClipFn('Fish_ObserveP')
  Old_Growth<-ClipFn('Old_GrowthP')
  Streams<-ClipFn('StreamsP')
  ConservationLands<-ClipFn('ConservationLandsP')
  MMWB<-ClipFn('MMWBP')

  #FWA_wetlands
  FWA_wetlands<-read_sf(file.path(spatialOutDirP,"FWA_wetlandsP.gpkg")) %>%
    st_intersection(AOIbuff)
  types <- st_geometry_type(FWA_wetlands)
  types_df.wet <- data.frame(types) #POLYGON

  FWA_wetlands <- FWA_wetlands %>%
    mutate(water=1) %>%
    mutate(WaterType='wetland') %>%
    mutate(lake_id=as.numeric(rownames(.))) %>%
    select(water,WaterType)
  write_sf(FWA_wetlands, file.path(spatialOutDir,"FWA_wetlands.gpkg"),delete_layer=TRUE)

  FWA_lakes<-ClipFn('FWA_LakesP') %>%
    mutate(water=1) %>%
    mutate(WaterType='lake') %>%
    mutate(lake_id=as.numeric(rownames(.))) %>%
    select(lake_id,water,WaterType)
  write_sf(FWA_lakes, file.path(spatialOutDir,"FWA_lakes.gpkg"))

  FWA_rivers<-ClipFn('FWA_RiversP') %>%
    mutate(water=1) %>%
    mutate(WaterType='river') %>%
    select(water,WaterType)
  write_sf(FWA_rivers, file.path(spatialOutDir,"FWA_rivers.gpkg"))

  FWA_lakesE<-FWA_lakes %>%
    select(water,WaterType)

  Erase_water<-rbind(FWA_lakesE,FWA_rivers) %>%
    dplyr::group_by(water) %>%
    dplyr::summarize()  %>%
    st_buffer(dist=0.5)
write_sf(Erase_water, file.path(spatialOutDir,"Erase_water.gpkg"))

#Watersheds in EcoProvince - needed for sample design split by stream
#merge small slivers from intersection with AOI to their larger neighbour
AOI_ws_in<-get_layer("wsc_drainages") %>%
  st_intersection(AOI_EP) %>%
  #Bust up the geometry into its constituent parts
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  mutate(areaHa=as.numeric(st_area(.)/10000))
#pull out the large watersheds, then the small watershed pieces
AOI_ws_large<-AOI_ws_in %>%
  dplyr::filter(areaHa>200000)%>%
  mutate(Large_id=as.numeric(rownames(.)))
AOI_ws_small<-AOI_ws_in %>%
  dplyr::filter(areaHa<200000) %>%
  mutate(Small_id=as.numeric(rownames(.)))
#For each small find its largest neighbout
neigb_int<-as.data.frame(st_intersects(AOI_ws_small,AOI_ws_large)) %>%
  dplyr::rename(Small_id=row.id) %>%
  dplyr::rename(Large_id=col.id) %>%
  left_join(st_drop_geometry(AOI_ws_large)) %>%
  group_by(Small_id) %>%
  mutate(new_value = Large_id[which.max(areaHa)]) %>%
  dplyr::summarise(n=n(),Large_id=first(new_value)) %>%
  ungroup()
#Join the neighbour list to the small so they can be combined with thier large neighbour
AOI_ws_small<-AOI_ws_small %>%
  left_join(neigb_int) %>%
  dplyr::select(-c(n,Small_id))
#bind the small and large together then group by the Large_id to absorbe the smaller units
AOI_ws<-rbind(AOI_ws_large,AOI_ws_small) %>%
  group_by(Large_id) %>%
  dplyr::summarise(n=n())
mapview(AOI_ws)
write_sf(AOI_ws, file.path(spatialOutDir,"AOI_ws.gpkg"))
} else

Disturb<-rast(file.path(spatialOutDir,'Disturb.tif'))
LandCover<-rast(file.path(spatialOutDir,'LandCover.tif'))
FireR<-rast(file.path(spatialOutDir,'FireR.tif'))
roadsSR<-rast(file.path(spatialOutDir,'roadsSR.tif'))
LandForm<-rast(file.path(spatialOutDir,'LandForm.tif'))

VRI<-st_read(file.path(spatialOutDir,'VRI.gpkg'))
roads_sf<-st_read(file.path(spatialOutDir,'roads_sf.gpkg'))
Fire2010<-st_read(file.path(spatialOutDir,'Fire2010.gpkg'))
FWA_ASS_WS<-st_read(file.path(spatialOutDir,'FWA_ASS_WS.gpkg'))
F_OWN<-st_read(file.path(spatialOutDir,'F_OWN.gpkg'))
BEC<-st_read(file.path(spatialOutDir,'BEC.gpkg'))
FWA_lakes<-st_read(file.path(spatialOutDir,'FWA_lakes.gpkg'))
FWA_rivers<-st_read(file.path(spatialOutDir,'FWA_rivers.gpkg'))
communities<-st_read(file.path(spatialOutDir,'communities.gpkg'))
karst<-st_read(file.path(spatialOutDir,'karst.gpkg'))
BGCprotected<-st_read(file.path(spatialOutDir,'BGCprotected.gpkg'))
GeoF<-st_read(file.path(spatialOutDir,'GeoF.gpkg'))
Fish_Observe<-st_read(file.path(spatialOutDir,'Fish_Observe.gpkg'))
Old_Growth<-st_read(file.path(spatialOutDir,'Old_Growth.gpkg'))
Old_GrowthSSP<-st_read(file.path(spatialOutDir,'Old_GrowthSSP.gpkg'))
Streams<-st_read(file.path(spatialOutDir,'Streams.gpkg'))
Erase_water<-st_read(file.path(spatialOutDir,'Erase_water.gpkg'))
AOI_ws<-st_read(file.path(spatialOutDir,'AOI_ws.gpkg'))
#message('Breaking')
#break






