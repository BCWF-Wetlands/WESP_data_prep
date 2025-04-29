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
                   'Sub_Boreal','GD_Base','GD_Base_Est','Sub_Boreal_PEM','SI_Base')
WetlandAreaDirL<-c('SIM_Base','Taiga_Boreal_Plains',
                   'Sub_Boreal','GD_Base','GD_Base_Est','Sub_Boreal_PEM','SI_Base')
WetlandAreaShortL<-c('SIM','TBP',
                     'SB','GD','GD_Est','SB_PEM','SI')
EcoPNL<-list("SOUTHERN INTERIOR MOUNTAINS",c("BOREAL PLAINS","TAIGA PLAINS"),
             "SUB-BOREAL INTERIOR","GEORGIA DEPRESSION","GEORGIA DEPRESSION","SUB-BOREAL INTERIOR","SOUTHERN INTERIOR")

#Select an EcoProvince(s)
#one of: 1-SIM, 2-TBP, 3-SB, 4-GD, 5-GD_Est, 6-SB_PEM, 7-SI

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
dataOutDir <- file.path(OutDir,'data',WetlandAreaDir)
dataOutDirDesign <- file.path('../WESP_Sample_Design/out/data',WetlandAreaDir)
dir.create(file.path(dataOutDir), showWarnings = FALSE)
dir.create(file.path(spatialOutDir), showWarnings = FALSE)
tempAOIDir<-paste0("tmp/",WetlandAreaDir)
dir.create(tempAOIDir, showWarnings = FALSE)

#Load AOI
AOI<- st_read(file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOI.gpkg")))
AOIbuff<- st_read(file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOIbuff.gpkg")))
AOI_EP<- st_read(file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOI_EP.gpkg")))
#AOIr25<-raster(file.path(spatialOutDir,'AOIr25.tif'))
AOIr<-raster(file.path(spatialOutDir,'AOIr.tif'))
AOIbuffr<-raster(file.path(spatialOutDir,'AOIbuffr.tif'))

#Fetch Field Data - check that field data is stored in DataDir
FieldData<-file.path(DataDir,'2024FieldData')
out_name<-'2024Field'
field_gdb<-list.files(FieldData, pattern = ".gdb", full.names = TRUE)[2]
st_layers(field_gdb)
layer_nm<-readline(prompt='Enter layer_name: ')
source('Field_01_load.R')
#
source('Field_02_clean_EP.R')# EP=EcoProvince specific clean script

#Adding field data
#source('01_load_2022Field.R')
#source('02_clean_Field_SIM.R') #for SIM since requires specific clean up
#source('02_clean_Field_SB.R') #for SB since requires specific clean up
#source('02_clean_field_TBP.R') #for TBP since requires specific clean up
#source('02_clean_field_GD.R') #for GD since requires specific clean up
#source('02_clean_field_GD_Est.R') #for GD Estuary since requires specific clean up

#Get data sets prepped for design stage
#source('02_clean_pre_design_wetlands.R')


