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

source("header.R")
#Run Provincial scale scripts only once

#Select an EcoProvince(s)
#one of: 1-SIM, 2-TBP, 3-SB, 4-GD, 5-GD_Est, 6-SB_PEM, 7-SI
EcoP<-7
WetlandArea<-WetlandAreaL[EcoP]
WetlandAreaDir<-WetlandAreaDirL[EcoP]
WetlandAreaShort<-WetlandAreaShortL[EcoP]
EcoPN<-as.character(EcoPNL[EcoP])
#For Plains use:
# EcoPN<-c("BOREAL PLAINS","TAIGA PLAINS")

#Set up unique directories for EcoProvince output
spatialOutDir <- file.path('out','spatial',WetlandAreaDir)
spatialOutDirDesign <- file.path('../WESP_Sample_Design/out/spatial',WetlandAreaDir)
dataOutDir <- file.path(OutDir,'data',WetlandAreaDir)
dataOutDirDesign <- file.path('../WESP_Sample_Design/out/data',WetlandAreaDir)
dir.create(file.path(dataOutDir), showWarnings = FALSE)
dir.create(file.path(spatialOutDir), showWarnings = FALSE)
tempAOIDir<-paste0("tmp/",WetlandAreaDir)
dir.create(tempAOIDir, showWarnings = FALSE)

AOIin <- bcmaps::ecoprovinces() %>%
  dplyr::filter(ECOPROVINCE_NAME %in% EcoPN) %>%
  st_union() %>%
  st_buffer(dist=1000)#modified AOI to capture wetlands on boundaries of AOI
AOI<-st_as_sf(AOIin)
mapview(AOIin)
write_sf(AOI, file.path(spatialOutDir,"SI.shp"))

#Base load
source('01_base_load.R')
#source('01_load.R') - if needed

#Creating new EcoProvince base wetlands
#source('01_load_MkWetlands.R')
#source('02_clean_MkWetlands.R')

#Preparing existing EcoProvince base wetlands
#source('01_load_pre_made_wetlands.R')
#This step requires a clean SampleStrata file from WESP_Sample_Design
#Dropped for 2023 - see Archive
#source('02_clean_pre_made_wetlands.R') #strips off and saves design attributes
# with calls to '02_clean geometry.R' and '02_clean_BCWF_centroids' if needed

#PEM & ESI data - for sub-boreal
#source('02_clean_Sub-Boreal.R')
# with calls to '02_clean_SubB_ESI.R' and identifies if wetland was identified by PEM
#Additional clean for SB to reconcile old label with new wetlands
#source('02_clean_SB_WTLND_ID.R')
#Generates PEM wetlands not in base wetlands and saves to Sub_Boreal_PEM directory
# these can then be prepared for design by the 02_clean_pre_designe_wetlands.R script

#PEM data for SI
#source('02_clean_field_SI.R') #for SI since requires specific clean up - including adding OK PEM polygons

#Adding field data
#source('01_load_2022Field.R')
#source('02_clean_Field_SIM.R') #for SIM since requires specific clean up
#source('02_clean_Field_SB.R') #for SB since requires specific clean up
#source('02_clean_field_TBP.R') #for TBP since requires specific clean up
#source('02_clean_field_GD.R') #for GD since requires specific clean up
#source('02_clean_field_GD_Est.R') #for GD Estuary since requires specific clean up

#Get data sets prepped for design stage
#source('02_clean_pre_design_wetlands.R')







