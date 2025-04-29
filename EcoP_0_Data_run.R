# Copyright 2018 Province of British Columbia
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

#Load packages, options and set up higher level directories
source("header.R")
#Provincial - run_Prov.R - must be run prior to EcoProv data prep - downloads data to local drive

#set up abbreviations and directory locations for EcoProvinces
WetlandAreaL<-list('SIM_Base',c('Taiga_Planes_Base','Boreal_Plains_Base'),
                   'Sub_Boreal','GD_Base','GD_Base_Est','Sub_Boreal_PEM','SI_Base','CI_Base')
WetlandAreaDirL<-c('SIM_Base','Taiga_Boreal_Plains',
                   'Sub_Boreal','GD_Base','GD_Base_Est','Sub_Boreal_PEM','SI_Base','CI_Base')
WetlandAreaShortL<-c('SIM','TBP',
                     'SB','GD','GD_Est','SB_PEM','SI','CI')
EcoPNL<-list("SOUTHERN INTERIOR MOUNTAINS",c("BOREAL PLAINS","TAIGA PLAINS"),
             "SUB-BOREAL INTERIOR","GEORGIA DEPRESSION","GEORGIA DEPRESSION","SUB-BOREAL INTERIOR",
             "SOUTHERN INTERIOR","CENTRAL INTERIOR")

#Select an EcoProvince(s)
#one of: 1-SIM, 2-TBP, 3-SB, 4-GD, 5-GD_Est, 6-SB_PEM, 7-SI, 8-CI

EcoP<-1
WetlandArea<-WetlandAreaL[EcoP]
WetlandAreaDir<-WetlandAreaDirL[EcoP]
WetlandAreaShort<-WetlandAreaShortL[EcoP]
EcoPN<-as.character(EcoPNL[EcoP])
#For Plains use:
# EcoPN<-c("BOREAL PLAINS","TAIGA PLAINS")

#Base load
spatialOutDir <- file.path('out','spatial',WetlandAreaDir)
spatialOutDirDesign <- file.path('../WESP_Sample_Design/out/spatial',WetlandAreaDir)
spatialOutDirDraw <- file.path('../WESP_Sample_Draw/data')
dataOutDir <- file.path(OutDir,'data',WetlandAreaDir)
dataOutDirDesign <- file.path('../WESP_Sample_Design/out/data',WetlandAreaDir)
dir.create(file.path(dataOutDir), showWarnings = FALSE)
dir.create(file.path(spatialOutDir), showWarnings = FALSE)
tempAOIDir<-paste0("tmp/",WetlandAreaDir)
dir.create(tempAOIDir, showWarnings = FALSE)

#Make set of AOIs
#AOI_EP - EcoProvince boundary
#AOI - AOI+intersecting watershed assessment units - get hydro unit for all wetlands within AOI_EP
#AOIbuff - with 2km buffer - for some Office questions in case wetland on boundary of AOI_EP

AOI_file <- file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOI.gpkg"))
if (!file.exists(AOI_file)) {
#first get the ASS_WS layer
FWA_ASS_WSP<-st_read(file.path(spatialOutDirP,'FWA_ASS_WSP.gpkg'))

#Get the EcoProvince boundary and buffer it by 2K - OF questions look up to 2K
AOI_EP <- bcmaps::ecoprovinces() %>%
    dplyr::filter(ECOPROVINCE_NAME %in% EcoPN) %>%
    st_union() %>%
    st_as_sf() %>%
    mutate(AOI=1) %>%
    dplyr::select(AOI)
st_geometry(AOI_EP) = "geom"
#Identify all the FWA assessment watersheds that are within or touch the EcoProvince boundary and disolve
# then remove hanging polygons
area_thresh <- units::set_units(1000, km^2)

AOI<-FWA_ASS_WSP %>%
  st_filter(AOI_EP, .predicates=st_intersects) %>%
  #st_filter(AOIbuff, .predicates=st_intersects) %>%
  mutate(AOI=1) %>%
  dplyr::group_by(AOI) %>%
  dplyr::summarize() %>%
  st_cast("POLYGON") %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  dplyr::filter(area_Ha>100000) %>%
  fill_holes(., threshold = area_thresh) %>%
  dplyr::select(AOI)

AOIbuff <- AOI %>%
  st_buffer(dist=2000) %>%
  dplyr::select(AOI) %>%
  fill_holes(., threshold = area_thresh)

#Check output and write
mapview(AOIbuff)+mapview(AOI_EP)+mapview(AOI)
write_sf(AOIbuff, file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOIbuff.gpkg")))
write_sf(AOI, file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOI.gpkg")))
write_sf(AOI_EP, file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOI_EP.gpkg")))
write_sf(AOI, file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOI.shp")))

#Generate raster clipping units
#AOIr25<-fasterize(AOI,raster(file.path(spatialOutDirP,'ProvRast25.tif')))
#  writeRaster(AOIr25, filename=file.path(spatialOutDir,'AOIr25'), format="GTiff", overwrite=TRUE)
AOIr<-fasterize(AOI,raster(file.path(spatialOutDirP,'ProvRast.tif'))) %>%
  raster::crop(AOI)
  writeRaster(AOIr, filename=file.path(spatialOutDir,'AOIr'), format="GTiff", overwrite=TRUE)
AOIbuffr<-fasterize(AOIbuff,raster(file.path(spatialOutDirP,'ProvRast.tif'))) %>%
  raster::crop(AOIbuff)
  writeRaster(AOIbuffr, filename=file.path(spatialOutDir,'AOIbuffr'), format="GTiff", overwrite=TRUE)
} else
AOI<- st_read(file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOI.gpkg")))
AOIbuff<- st_read(file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOIbuff.gpkg")))
AOI_EP<- st_read(file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOI_EP.gpkg")))
#AOIr25<-raster(file.path(spatialOutDir,'AOIr25.tif'))
AOIr<-raster(file.path(spatialOutDir,'AOIr.tif'))
AOIbuffr<-raster(file.path(spatialOutDir,'AOIbuffr.tif'))

#Load core wetlands if they exist - go to MkWetlands scripts to generate
wet_file <- file.path(spatialOutDir,'Wetlands.gpkg')
if (file.exists(wet_file)) {
Wetlands<-st_read(file.path(spatialOutDir,'Wetlands.gpkg'))
WetlandsB<-st_read(file.path(spatialOutDir,'WetlandsB.gpkg'))
#WetlandsAll<-st_read(file.path(spatialOutDir,'WetlandsAll.gpkg'))
wetland.pt<-st_read(file.path(spatialOutDir,'wetland.pt.gpkg'))
}
