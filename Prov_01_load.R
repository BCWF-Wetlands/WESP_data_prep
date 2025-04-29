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

#Load Provincial maps from BC Data wharehouse
# Landcover and Landform are local files and are loaded under Prov_0_Data_run.R

# bring in BC boundary
bc <- bcmaps::bc_bound()
Prov_crs<-crs(bc)
#Prov_crs<-"+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

#Provincial boundary in various formats
BCr_file <- file.path(spatialOutDirP,"BCr.tif")
if (!file.exists(BCr_file)) {
  BC<-bcmaps::bc_bound_hres()
  write_sf(BC, file.path(spatialOutDirP,"BC.gpkg"))
  ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5,
                   ymn=173787.5, ymx=1748187.5,
                   crs=Prov_crs,
                   res = c(100,100), vals = 1)
  ProvRast25<-raster(nrows=62976, ncols=68864, xmn=159587.5, xmx=1881187.5,
                     ymn=173787.5, ymx=1748187.5,
                     crs=Prov_crs,
                     res = c(25,25), vals = 1)
  #crs(ProvRast)<-crs(bcmaps::bc_bound())
  ProvRast_S<-st_as_stars(ProvRast)
  write_stars(ProvRast_S,dsn=file.path(spatialOutDirP,'ProvRast_S.tif'))
  BCr <- fasterize(BC,ProvRast)
  BCr25 <- fasterize(BC,ProvRast25)
  #Linear rasterization of roads works better using the stars package
  BCr_S <-st_as_stars(BCr)
  write_stars(BCr_S,dsn=file.path(spatialOutDirP,'BCr_S.tif'))
  writeRaster(BCr, filename=BCr_file, format="GTiff", overwrite=TRUE)
  writeRaster(BCr25, filename=file.path(spatialOutDirP,"BCr25.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(ProvRast, filename=file.path(spatialOutDirP,'ProvRast'), format="GTiff", overwrite=TRUE)
  writeRaster(ProvRast25, filename=file.path(spatialOutDirP,'ProvRast25'), format="GTiff", overwrite=TRUE)
} else {
  BC<-st_read(file.path(spatialOutDirP,"BC.gpkg"))
  BCr <- raster(BCr_file)
  BCr25 <- raster(file.path(spatialOutDirP,"BCr25.tif"))
  ProvRast<-raster(file.path(spatialOutDirP,'ProvRast.tif'))
  ProvRast25<-raster(file.path(spatialOutDirP,'ProvRast25.tif'))
  BCr_S <- read_stars(file.path(spatialOutDirP,'BCr_S.tif'))
  BC <-readRDS('tmp/BC')
}

#Get watersheds from bc_maps get_layer
ws <- get_layer("wsc_drainages")
st_crs(ws)<-3005
write_sf(ws, file.path(spatialOutDirP,"ws.gpkg"))

#layers that can get fetched from BC Data Catalogue
FOWNP<-get_data_fn('WHSE_FOREST_VEGETATION.F_OWN','F_OWNP')
BECP<-get_data_fn('WHSE_FOREST_VEGETATION.BEC_BIOGEOCLIMATIC_POLY','BEC_raw')

#FWA_wetlands
FWA_wetlandsP<-get_data_fn('WHSE_BASEMAPPING.FWA_WETLANDS_POLY','FWA_wetlandsP')
write_sf(FWA_wetlandsP, file.path(spatialOutDirP,"FWA_wetlandsP.gpkg"))
#FWA_wetlandsP<-read_sf(file.path(spatialOutDirP,"FWA_wetlandsP.gpkg"))
#types <- st_geometry_type(FWA_wetlandsP)
#types_df.wetP <- data.frame(types) #POLYGON

FWA_lakesP<-get_data_fn("WHSE_BASEMAPPING.FWA_LAKES_POLY",'FWA_lakesP') %>%
  mutate(water=1) %>%
  mutate(WaterType='lake') %>%
  mutate(lake_id=as.numeric(rownames(.))) %>%
  select(water,WaterType)
write_sf(FWA_lakesP, file.path(spatialOutDirP,"FWA_lakesP.gpkg"))

FWA_riversP<-get_data_fn('WHSE_BASEMAPPING.FWA_RIVERS_POLY','FWA_riversP') %>%
  mutate(water=1) %>%
  mutate(WaterType='river') %>%
  select(water,WaterType)
write_sf(FWA_riversP, file.path(spatialOutDirP,"FWA_riversP.gpkg"))

#Erase_waterP<-rbind(FWA_lakesP,FWA_riversP) %>%
#  dplyr::group_by(water) %>%
#  dplyr::summarize()  %>%
#  st_buffer(dist=0.5)
#write_sf(Erase_waterP, file.path(spatialOutDirP,"Erase_waterP.gpkg"),delete_layer=TRUE)

#communitiesP<-get_data_fn('WHSE_LEGAL_ADMIN_BOUNDARIES.ABMS_MUNICIPALITIES_SP','communitiesP')
karstP<-get_data_fn('WHSE_LAND_USE_PLANNING.RKPM_KARST_POTENTIAL_AREA_SP','karstP')
BGCprotectedP<-get_data_fn('WHSE_LAND_AND_NATURAL_RESOURCE.PASO_PCT_BGC_CODE_PRTCTD_SVW','BGCprotectedP')
GeoFP<-get_data_fn('WHSE_MINERAL_TENURE.GEOL_FAULT_LINE','GeoFP')
Fish_ObserveP<-get_data_fn('WHSE_FISH.FISS_FISH_OBSRVTN_PNT_SP','Fish_ObserveP')
Fish_StockedP<-get_data_fn('WHSE_FISH.FISH_RELEASES_PUBLIC_VW','Fish_StockedP')

Old_GrowthP<-get_data_fn('WWHSE_FOREST_VEGETATION.OGSR_TAP_SERAL_STAGE_SP','Old_GrowthP')#also in raster
Old_GrowthSSP<-get_data_fn('WHSE_FOREST_VEGETATION.OGSR_TAP_SERAL_STAGE_SP','Old_GrowthSSP')#also in raster
#Need species of concern specific data set - only available from CDC
#Species_of_ConcernP<-get_data_fn('WHSE_TERRESTRIAL_ECOLOGY.BIOT_OCCR_MASKED_SENS_AREA_SP','Species_of_ConcernP')
#Need permission to access this layer:
#Species_of_ConcernPrivP<-get_data_fn('WHSE_TERRESTRIAL_ECOLOGY.BIOT_OCCR_MASKED_SENS_PRIV_SP','Species_of_ConcernPrivP')
FWA_ASS_WSP<-get_data_fn('WHSE_BASEMAPPING.FWA_ASSESSMENT_WATERSHEDS_POLY','FWA_ASS_WSP')
MMWBP<-get_data_fn('WHSE_FISH.WDIC_WATERBODY_POLY_SVW','WSA_WB_PLY') %>%
  dplyr::filter(DESCRIPTION=='Man-made waterbody') %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001))
write_sf(MMWBP, file.path(spatialOutDirP,"MMWBP.gpkg"))

#Lands that Contribute to Conservation - download to SpatialDir
#https://github.com/bcgov/designatedlands/releases/download/v0.1.0/designatedlands.gpkg.zip
ConservationLandsP<-read_sf(file.path(SpatialDir,'designatedlands.gpkg'))
st_write(ConservationLandsP,file.path(spatialOutDirP,"ConservationLandsP.gpkg"))

#Fire
Fire_CurrentP<-get_data_fn('WHSE_LAND_AND_NATURAL_RESOURCE.PROT_CURRENT_FIRE_POLYS_SP','Fire_CurrentP')
Fire_HistoricP<-get_data_fn('WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP','Fire_HistoricP')

Fire_CurrentP<-read_sf(file.path(spatialOutDirP,'Fire_CurrentP.gpkg')) %>%
  dplyr::select(id,FIRE_NUMBER,FIRE_YEAR)
Fire_HistoricP<-read_sf(file.path(spatialOutDirP,'Fire_HistoricP.gpkg')) %>%
  dplyr::select(id,FIRE_NUMBER,FIRE_YEAR)

FireP<-dplyr::bind_rows(list(Fire_HistoricP,Fire_CurrentP)) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(FIRE_YEAR=as.numeric(FIRE_YEAR)) %>%
  mutate(fire= case_when((FIRE_YEAR > 2019) ~ 5,
                         (FIRE_YEAR<=2018 & FIRE_YEAR >= 1991) ~ 2,
                         (FIRE_YEAR<=1990) ~ 1,
                         TRUE ~ 0))

st_write(Fire,file.path(spatialOutDirP,"FireP.gpkg"))
Fire2010P<-Fire %>%
  dplyr::filter(FIRE_YEAR>2009)
st_write(Fire2010P,file.path(spatialOutDirP,"Fire2010P.gpkg"))

FireR<- fasterize(Fire,ProvRast,field='fire')
FireR[is.na(FireR)]<-0
writeRaster(FireR,filename=file.path(spatialOutDirP,"FireR.tif"), format="GTiff", overwrite=TRUE)

#CE Roads
road_file=(file.path(spatialOutDirP,'roadsP.gpkg'))
if (!file.exists(disturb_file)) {
  RoadZip <- 'BC_CE_Integrated_Roads_2024.zip'
  #Fails due to file size, so set timeout to be very large or download seperately
  options(timeout=10000)
  #Manually download file... below doesnt work with BC Data Catalogue
  #download.file(" https://coms.api.gov.bc.ca/api/v1/object/ecea4b04-055a-49d1-8910-60d726d2d1bf",
  #              destfile = file.path(SpatialDir, disturbZip))
  unzip(file.path(SpatialDir, RoadZip), exdir = file.path(SpatialDir, "RoadsP"))

  RoadsP_gdb <- list.files(file.path(SpatialDir, "RoadsP",'BC_CE_Integrated_Roads_2024'), pattern = ".gdb", full.names = TRUE)[1]
  fc_list <- st_layers(RoadsP_gdb)

  Roads_in <- read_sf(RoadsP_gdb, layer = "integrated_roads_2024")
  st_crs(Roads_in) <- 3005
  write_sf(Roads_in, file.path(spatialOutDirP,"roadsP.gpkg"))
} else {
  roadsP<-read_sf(file.path(spatialOutDirP,"roadsP.gpkg"))
}

#Roads
roads_file <- file.path(spatialOutDirP,"roadsSR.tif")
if (!file.exists(roads_file)) {
  roads_sf_in<-read_sf(file.path(spatialOutDirP,'roadsP.gpkg'))

  #Check the types
  unique(roads_sf_in$DRA_ROAD_CLASS)
  unique(roads_sf_in$DRA_ROAD_SURFACE)
  unique(roads_sf_in$OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE)

  ### Check Petro roads
  #Appears petro roads are typed with SURFACE and CLASSS
  table(roads_sf_in$DRA_ROAD_SURFACE,roads_sf_in$OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE)
  table(roads_sf_in$DRA_ROAD_CLASS,roads_sf_in$OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE)

  #Additional petro road checks
  #Check if all petro roads have a OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE
  PetroCheck<-roads_sf_in %>%
    st_drop_geometry() %>%
    dplyr::filter(is.na(DRA_ROAD_CLASS))

  Petro_Tbl <- st_set_geometry(roads_sf_in, NULL) %>%
    dplyr::count(OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE, LENGTH_METRES)

  roads_sf_petro <- roads_sf_in %>%
    mutate(DRA_ROAD_SURFACE=if_else(is.na(OG_DEV_PRE06_OG_PETRLM_DEV_RD_PRE06_PUB_ID),DRA_ROAD_SURFACE,'OGC')) %>%
    mutate(DRA_ROAD_CLASS=if_else(is.na(OG_DEV_PRE06_OG_PETRLM_DEV_RD_PRE06_PUB_ID),DRA_ROAD_CLASS,OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE))

  Petro_Tbl <- st_set_geometry(roads_sf_petro, NULL) %>%
    dplyr::count(DRA_ROAD_SURFACE, DRA_ROAD_CLASS)
  #### End Petro road check

  #Eliminate non-summer roads
  notRoadsCls <- c("ferry", "water", "Road Proposed","WINT")
  notRoadsSurf<-c("boat")
  notWinter<-c("WINT")

  roads_sf_1<-roads_sf_in %>%
    filter(!DRA_ROAD_CLASS %in% notRoadsCls,
           !DRA_ROAD_SURFACE %in% notRoadsSurf,
           !OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE %in% notWinter)

  HighUseCls<-c("Road arterial major","Road highway major", "Road arterial minor","Road highway minor",
                "Road collector major","Road collector minor","Road ramp","Road freeway",
                "Road yield lane")

  ModUseCls<-c("Road local","Road recreation","Road alleyway","Road restricted",
               "Road service","Road resource","Road driveway","Road strata",
               "Road resource demographic", "Road strata","Road recreation demographic", "Trail Recreation",
               "Road runway", "Road runway non-demographic", "Road resource non-status","Road unclassified or unknown")

  LowUseCls<-c("Road lane","Road skid","Road trail","Road pedestrian","Road passenger",
               "Trail", "Trail demographic","Trail skid", "Road pedestrian mall")

  HighUseSurf<-c("paved")
  ModUseSurf<-c("loose","rough","unknown")
  LowUseSurf<-c("overgrown","decommissioned","seasonal")

  #Add new attribute that holds the use classificationr
  roads_sf <- roads_sf_1 %>%
    mutate(RoadUse = case_when((DRA_ROAD_CLASS %in% HighUseCls & DRA_ROAD_SURFACE %in% HighUseSurf) ~ 1, #high use
                               (DRA_ROAD_CLASS %in% LowUseCls | DRA_ROAD_SURFACE %in% LowUseSurf |
                                  #(DRA_ROAD_SURFACE %in% ModUseSurf & is.na(DRA_ROAD_NAME_FULL)) |
                                  (is.na(DRA_ROAD_CLASS) & is.na(DRA_ROAD_SURFACE))) ~ 3,#low use
                               TRUE ~ 2)) # all the rest are medium use

  #Check the assignment
  Rd_Tbl <- st_set_geometry(roads_sf, NULL) %>%
    dplyr::count(DRA_ROAD_SURFACE, DRA_ROAD_CLASS, is.na(DRA_ROAD_NAME_FULL), RoadUse)

  #Data check
  nrow(roads_sf)-nrow(roads_sf_1)
  table(roads_sf$RoadUse)

  # save as geopackage format for use in GIS and for buffer anlaysis below
  write_sf(roads_sf, file.path(spatialOutDirP,"roads_sfP.gpkg"))

  #Use Stars to rasterize according to RoadUse and save as a tif
  #first st_rasterize needs a template to 'burn' the lines onto
  BCr_S <- read_stars(file.path(spatialOutDirP,'BCr_S.tif'), proxy=FALSE)
  template = BCr_S
  template[[1]][] = NA
  roadsSR<-stars::st_rasterize(roads_sf[,"RoadUse"], template)
  write_stars(roadsSR,dsn=file.path(spatialOutDirP,'roadsSRP.tif'))

#Create a distance to road raster
  roadsDist.1<-rast(file.path(spatialOutDirP,'roadsSRP.tif')) %>%
    #convert low use roads to 0 and NaN to 0
    terra::subst(c(2,3), 0) %>%
    terra::subst(NaN, 0)
  #use distance function on raster values=1 - high use roads
  roadsDist<- terra::distance(roadsDist.1,target=1)
  #plot(roadsDist)
  writeRaster(roadsDist, file.path(spatialOutDirP,'roadsDistP.tif'), overwrite=TRUE)

} else {
  #Read in raster roads with values 0-none, 1-high use, 2-moderate use, 3-low use)
  roadsSR<-raster(file.path(spatialOutDirP,'roadsSRP.tif'))
  roads_sf<-st_read(file.path(spatialOutDirP,"roads_sfP.gpkg"))
  roadsDist<-rast(file.path(spatialOutDirP,'roadsDistP.tif'))
}

#Provincial Human Disturbance Layers - compiled for CE
disturb_file=(file.path(spatialOutDirP,'DisturbanceP.gpkg'))
if (!file.exists(disturb_file)) {
  disturbZip <- 'BC_CEF_Human_Disturbance_2023.zip'
  #Fails due to file size, so set timeout to be very large or download seperately
  #options(timeout=10000)
  #Changed download - now manual
  #download.file("https://coms.api.gov.bc.ca/api/v1/object/ecea4b04-055a-49d1-8910-60d726d2d1bf",
  #              destfile = file.path(SpatialDir, disturbZip))
  unzip(file.path(SpatialDir, disturbZip), exdir = file.path(SpatialDir, "DisturbanceP"))

  disturb_gdb <- list.files(file.path(SpatialDir, "DisturbanceP",'BC_CEF_Human_Disturbance_2023'), pattern = ".gdb", full.names = TRUE)
  fc_list <- st_layers(disturb_gdb)

  DisturbanceP_in <- read_sf(disturb_gdb, layer = "BC_CEF_Human_Disturb_BTM_2023")
  st_crs(DisturbanceP_in) <- 3005
  write_sf(DisturbanceP_in, file.path(spatialOutDirP,"DisturbanceP.gpkg"))
} else {
  DisturbanceP<-read_sf(file.path(spatialOutDirP,"DisturbanceP.gpkg"))
}

#Needs refinement to differentiate rural/urban and old vs young cutblocks, rangeland, etc.
dist_file<-file.path(spatialOutDirP,'disturbance_sfR.tif')
if (!file.exists(dist_file)) {
  DisturbanceP<-read_sf(file.path(spatialOutDirP,'DisturbanceP.gpkg'))
  disturbance_sf <- DisturbanceP

  #Fasterize disturbance subgroup
  disturbance_Tbl <- st_set_geometry(disturbance_sf, NULL) %>%
    dplyr::count(.,CEF_DISTURB_SUB_GROUP, CEF_DISTURB_GROUP)

  #Fix non-unique sub group codes
  disturbance_sf <- disturbance_sf %>%
    mutate(disturb = case_when(!(CEF_DISTURB_SUB_GROUP %in% c('Baseline Thematic Mapping', 'Historic BTM', 'Historic FAIB', 'Current FAIB')) ~ CEF_DISTURB_GROUP,
                               (CEF_DISTURB_GROUP == 'Agriculture_and_Clearing' & CEF_DISTURB_SUB_GROUP == 'Baseline Thematic Mapping') ~ 'Agriculture_and_Clearing',
                               (CEF_DISTURB_GROUP == 'Mining_and_Extraction' & CEF_DISTURB_SUB_GROUP == 'Baseline Thematic Mapping') ~ 'Mining_and_Extraction',
                               (CEF_DISTURB_GROUP == 'Urban' & CEF_DISTURB_SUB_GROUP == 'Baseline Thematic Mapping') ~ 'Urban',
                               (CEF_DISTURB_GROUP == 'Cutblocks' & CEF_DISTURB_SUB_GROUP == 'Current FAIB') ~ 'Cutblocks_Current',
                               (CEF_DISTURB_GROUP == 'Cutblocks' & CEF_DISTURB_SUB_GROUP == 'Historic FAIB') ~ 'Cutblocks_Historic',
                               (CEF_DISTURB_GROUP == 'Cutblocks' & CEF_DISTURB_SUB_GROUP == 'Historic BTM') ~ 'Cutblocks_Historic',
                               TRUE ~ 'Unkown'))

  disturbance_Tbl <- st_set_geometry(disturbance_sf, NULL) %>%
    dplyr::count(.,CEF_DISTURB_SUB_GROUP, CEF_DISTURB_GROUP, disturb)
  WriteXLS(disturbance_Tbl,file.path(DataDir,'disturbance_Tbl.xlsx'))

  Unique_disturb<-unique(disturbance_sf$disturb)
  AreaDisturbance_LUT<-data.frame(disturb_Code=1:length(Unique_disturb),disturb=Unique_disturb)

  #Write out LUT
  WriteXLS(AreaDisturbance_LUT,file.path(DataDir,'AreaDisturbance_LUT.xlsx'))

  AreaDisturbance_LUT<-data.frame(read_excel(file.path(DataDir,'AreaDisturbance_LUT.xlsx'))) %>%
    dplyr::select(disturb,disturb_code=disturb_Code,ID=disturbRank)

  disturbance_sfR1a <- disturbance_sf %>%
    left_join(AreaDisturbance_LUT)

  #contains a multisurface type, only extract P and M
  disturbance_sfR1b <- disturbance_sfR1a %>%
    mutate(types=st_geometry_type(.)) %>%
    dplyr::filter(types %in% c("POLYGON","MULTIPOLYGON")) %>%
    st_cast("POLYGON")
  #write_sf(disturbance_sfR1b, file.path(spatialOutDirP,"disturbance_sfR1b.gpkg"))
  disturbance_sfR1b<-st_read(file.path(spatialOutDirP,"disturbance_sfR1b.gpkg"))
  unique(st_is_valid(disturbance_sfR1b)) #TRUE FALSE
  disturbance_sf<-st_make_valid(disturbance_sfR1b) %>%
    st_set_crs(3005)
  unique(st_is_valid(disturbance_sf)) #TRUE
  write_sf(disturbance_sf, file.path(spatialOutDirP,"disturbance_sfP.gpkg"))

  #clgeo_Clean approach
  #disturbance_sf.s<-as(disturbance_sfR1b,'Spatial')
  #clgeo_IsValid(disturbance_sf.s, verbose = FALSE) #FALSE
  #report <- clgeo_CollectionReport(disturbance_sf.s)
  #clgeo_SummaryReport(report)
  #This takes a very long time...
  #disturbance_sf.1<-clgeo_Clean(disturbance_sf.s, errors.only = NULL, verbose = FALSE)
  #alternative
  #disturbance_sf.1 <- clgeo_Clean(sp = disturbance_sf.s, strategy = "BUFFER")
  #disturbance_sf<-disturbance_sf.1 %>%
  #  st_as_sf() %>%
  #  st_set_crs(3005)
  #clgeo_IsValid(as(disturbance_sf,'Spatial'), verbose = FALSE) #

  disturbance_sf<-st_read(file.path(spatialOutDirP,"disturbance_sfP.gpkg"))

  disturbance_sfRP<- fasterize(disturbance_sf, BCr, field="ID")
  disturbance_sfRP[is.na(disturbance_sfRP)]<-0

  #write_sf(disturbance_sf, file.path(spatialOutDirP,"disturbance_sf.gpkg"))
  writeRaster(disturbance_sfRP, filename=file.path(spatialOutDirP,'disturbance_sfRP'), format="GTiff", overwrite=TRUE)

} else {
  disturbance_sfRP<-raster(file.path(spatialOutDirP,'disturbance_sfRP.tif'))
  disturbance_sf<- st_read(file.path(spatialOutDirP,"disturbance_sfP.gpkg"))
}

#Dwelling Density
dwell_file<-file.path(spatialOutDirP,'dwellP_R.tif')
if (!file.exists(dwell_file)) {
#stats Canada provides population numbers for census dissemination areas
# data for finer resolution dissemination blocks is not available due to privacy concerns
# census boundaries are available here, download to local 'data/spatial' directory:
#https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
#dissemination blocks: censusBlkB - ldb_000b21f_e
#dissemination areas: censusBlkA - lda_000b21f_e
out_name<-'censusBlkA'
censusBlk_gdb<-list.files(file.path(SpatialDir,'HumanDensity/lda_000b21f_e'), pattern = ".gdb", full.names = TRUE)[1]
st_layers(censusBlk_gdb)
layer_nm<-readline(prompt='Enter layer_name: ')
censusBlkA.1<-gdbFn(censusBlk_gdb,layer_nm,out_name) %>%
  dplyr::filter(PRUID == "59") #select only BC
#Re project to BC Albers and write geometry to disk
#Projection is from https://www150.statcan.gc.ca/n1/pub/92-160-g/92-160-g2016002-eng.htm
st_crs(censusBlkA.1)<-3347
censusBlkA <-st_transform(censusBlkA.1, 3005) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001))
st_write(censusBlkA,file.path(spatialOutDirP,"censusBlkA.gpkg"), append = FALSE)
censusBlkA<-st_read(file.path(spatialOutDirP,"censusBlkA.gpkg"))
#Download population data to local 'data' directory and read
#Data - https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/hlt-fst/pd-pl/comprehensive.cfm
# a number of versions were investigated but none found to be at dissemination block scale
#Humans_DB2 <-
#  data.frame(read.csv(header=TRUE, file=file.path(DataDir,'2021_92-151_X.csv'))) %>%
#  dplyr::select(one_of(c('DGUID','Population.and.dwelling.counts..5...Population..2021..1.','Population.and.dwelling.counts..5...Land.area.in.square.kilometres..2021..4.','Population.and.dwelling.counts..5...Population.density.per.square.kilometre..2021..5.')))
#colnames(Humans_DB)<-c('DGUID','TotalHumans','AreaOnFile_KM2','HumanDensity')
#Humans_DB3in <-
#  data.frame(read.csv(header=TRUE, file=
#  file.path(DataDir,'98-401-X2021006_BC_CB_eng_CSV/998-401-X2021006_BC_CB_eng_CSV_data_BritishColumbia.csv')))
Humans_DBin <-
  data.frame(read.csv(header=TRUE, file=file.path(DataDir,'98100015-eng/98100015.csv')))
Humans_DB <- Humans_DBin %>%
  data.frame(read.csv(header=TRUE, file=file.path(DataDir,'98100015-eng/98100015.csv'))) %>%
  dplyr::select(one_of(c('DGUID','Population.and.dwelling.counts..5...Population..2021..1.','Population.and.dwelling.counts..5...Total.private.dwellings..2021..2.','Population.and.dwelling.counts..5...Land.area.in.square.kilometres..2021..4.','Population.and.dwelling.counts..5...Population.density.per.square.kilometre..2021..5.')))
colnames(Humans_DB)<-c('DGUID','TotalHumans','TotalDwellings','AreaOnFile_KM2','HumanDensity')
#Attach data to geometry
censusP<-
  censusBlkA %>%
  left_join(Humans_DB, by='DGUID') %>%
  dplyr::filter(PRUID==59) %>%
  #calculate dwelling density for indicator
  mutate(DwellingDensity=round(TotalDwellings/AreaOnFile_KM2,0))
st_write(censusP,file.path(spatialOutDirP,"censusP.gpkg"), append = FALSE)
#select census areas where dwelling density is greater than 5 dwellings/km2 and convert to a spatial vector
residenceP.1 <- censusP %>%
  mutate(dwell=if_else(DwellingDensity>5,1,0)) %>%
  mutate(residence=if_else(HumanDensity>5,1,0)) %>%
  #dplyr::select(dwell) %>%
  dplyr::select(residence) %>%
  #dplyr::filter(DwellingDensity>5) %>%
  vect()
mapview(residenceP.1)

#Flip to terra rast so distance of NA cells to dwelling cells can be calculated
residenceP.2<-rasterize(residenceP.1,rast(BCr),field='residence') %>%
  terra::subst(0, NA)
plot(residenceP.2)
residenceP_R<-terra::distance(residenceP.2)
plot(residenceP_R)
writeRaster(residenceP_R, file.path(spatialOutDirP,'residenceP_R.tif'), overwrite=TRUE)

} else {
  dwellP_R.tif<-rast(file.path(spatialOutDirP,'dwellP_R.tif'))
  residenceP_R.tif<-rast(file.path(spatialOutDirP,'residenceP_R.tif'))
}

#Geology
#GeologyP<-st_read(file.path(SpatialDir,'Geology/BC_digital_geology_gpkg/BC_digital_geology.gpkg'))
#Download zip shape file from Data Catalogue
# https://catalogue.data.gov.bc.ca/dataset/ef8476ed-b02d-4f5c-b778-0d44c9126144/resource/aa3a15f8-02fe-49c6-836c-6866c326203d/download/bedrockgeology2018.zip
GeologyP<-st_read(file.path(SpatialDir,'Geology/bedrockgeology2018/BC_bedrock_ll83.shp'))
BedrockL<-c( 'diorite' , 'diorite to granodiorite' , 'diorite to porphyry' ,
             'diorite to quartz monzonite' , 'diorite to quartz-feldspar±hornblende±biotite porphyry' ,
             'diorite, foliated' , 'diorite, gabbro' , 'diorite, gabbro, quartz diorite, granodiorite' ,
             'diorite, gabbro, tonalite' , 'diorite, granodiorite, tonalite, gabbro' ,
             'diorite, granodiorite, tonalite, metagabbro' , 'diorite, microdiorite, gabbro' ,
             'dioritic intrusive rocks' , 'dioritic to syenitic intrusive rocks' ,
             'feldspar porphyritic intrusive rocks' , 'foliated granite, alkali feldspar granite intrusive rocks' ,
             'gabbro' , 'gabbro to granodiorite' , 'gabbro to quartz diorite' , 'gabbro, pyroxenite, diorite' ,
             'gabbroic intrusive rocks' , 'gabbroic to dioritic intrusive rocks' , 'gabbroic, diorite' ,
             'gneiss' , 'gneissic diorite' , 'granite' , 'granite to quartz diorite' ,
             'granite, alkali feldspar granite intrusive rocks' , 'granite, alkali feldspar phyric' ,
             'granite, granodiorite' , 'granite, granodiorite, diorite' , 'granite, quartz monzonite,
             granodiorite, rhyolite' , 'granitoid, gabbro and porphyry' , 'granodiorite' ,
             'granodiorite and plagioclase±hornblende porphyry' , 'granodiorite dikes' ,
             'granodiorite to feldspar±hornblende±biotite porphyry' , 'granodiorite to granite' ,
             'granodiorite to quartz-feldspar±hornblende±biotite porphyry' , 'granodiorite to tonalite' ,
             'granodiorite, granite' , 'granodiorite, tonalite, granite' , 'granodioritic intrusive rocks' ,
             'granodioritic orthogneiss' , 'high level quartz phyric, felsitic intrusive rocks' ,
             'metaquartz diorite' , 'monzodioritic to gabbroic intrusive rocks' , 'monzonite' ,
             'quartz diorite' , 'quartz diorite and granodiorite' , 'quartz diorite to feldspar porphyry' ,
             'quartz diorite to granite' , 'quartz diorite to granodiorite' ,
             'quartz diorite to quartz-feldspar±hornblende±biotite porphyry' ,
             'quartz diorite to tonalite' , 'quartz diorite, feldspar-hornblende dacite porphyry' ,
             'quartz dioritic intrusive rocks' , 'quartz feldspar porphyry' ,
             'quartz monzodiorite to granodiorite' , 'quartz monzodiorite to plagioclase-hornblende porphyry' ,
             'quartz monzonite' , 'quartz monzonitic intrusive rocks' ,
             'quartz monzonitic to monzogranitic intrusive rocks' , 'quartz porphyry intrusive' ,
             'quartz-biotite schist' , 'quartz-feldspar±hornblende±biotite porphyry' , 'quartz-sericite schist' ,
             'quartzite' ,  'syenitic intrusive rocks' , 'syenitic to monzodioritic intrusive rocks' ,
             'syenitic to monzonitic intrusive rocks' , 'tonalite' , 'tonalite intrusive rocks' , 'tonalite, diorite' ,
             'tonalite, quartz diorite' )
BedrockP <- GeologyP %>%
  dplyr::filter(rock_type %in% BedrockL) %>%
  st_transform(3005) %>%
  mutate(rastV=as.numeric(rownames(.)))
write_sf(BedrockP, file.path(spatialOutDirP,"BedrockP.gpkg"))

BedrockrP <- fasterize(st_cast(BedrockP,"POLYGON"),ProvRast,field='rastV')
writeRaster(BedrockrP, filename=file.path(spatialOutDirP,'BedrockrP'), format="GTiff", overwrite=TRUE)

#Download streams from BC Data Catalogue
Streams_F<-file.path(spatialOutDirP,"StreamsP.gpkg")
if (!file.exists(Streams_F)) {
  Streams_L<-st_layers(file.path(GISLibrary,'ProvWaterData/FWA_STREAM_NETWORKS_SP.gdb'))$name[1:246]
  Streams<-list()
  Streams<-lapply(Streams_L, function(x) read_sf(file.path(GISLibrary,'ProvWaterData/FWA_STREAM_NETWORKS_SP.gdb'), layer=x))
  names(Streams) <- Streams_L

  StreamsP <- do.call(rbind, Streams)

  write_sf(StreamsP, file.path(spatialOutDirP,"StreamsP.gpkg"))
} else {
  StreamsP <- read_sf(file.path(spatialOutDirP,"StreamsP.gpkg"))
}

