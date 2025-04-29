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
source('01_Load_Fns.R')

#Provincial - Prov_0_Data_run.R - must be run prior to EcoProv data prep - downloads data to local drive

#Run Provincial scale scripts only once
#Load basic provincial files - will take a few hours
source('Prov_01_load_base.R')

#BCWF specific layers - landcover, landform
#Fetch from BCWF WESP/GIS_external/Data_Library
#https://bcwildlife-my.sharepoint.com/personal/wew_bcwf_bc_ca/_layouts/15/onedrive.aspx?ga=1&id=%2Fpersonal%2Fwew%5Fbcwf%5Fbc%5Fca%2FDocuments%2FWetlands%20Workforce%2FWESP%2FGIS%5Fexternal%2FData%5FLibrary
#Copy into local directory ./out/Spatial as 'landcoverP' and 'landformP'
landcoverP<-raster(file.path(WESPdata,'LandCover/BC_Land_Cover/BC_LandCover_20210512.tif'))
writeRaster(landcoverP,file.path(spatialOutDirP,'landcoverP.tif'),overwrite=TRUE)
#LandCover_LUT<-read_xlsx(file.path(DataDir,"LandCover_LUT.xlsx"))
#WriteXLS(LandCover_LUT,file.path(dataOutDir,'LandCover_LUT.xlsx'))

landformP<-raster(file.path(WESPdata,'BC_Landform/bc_landform9cl.tif'))
writeRaster(landformP,file.path(spatialOutDirP,'landformP.tif'),overwrite=TRUE)
Landforms_LUT<-read.csv(file.path(WESPdata,'BC_Landform/Landforms.csv'))
WriteXLS(Landforms_LUT,file.path(dataOutDir,'Landforms_LUT.xlsx'))

#Load files that require specialized input that changes year to year, first source zip download and sf functions - could take many hours/days
#VRI - go to VRI link and get the zip location
browseURL("https://catalogue.data.gov.bc.ca/dataset/vri-2023-forest-vegetation-composite-rank-1-layer-r1-")
#Set location of VRI file
VRI_DC<-"https://pub.data.gov.bc.ca/datasets/02dba161-fdb7-48ae-a4bb-bd6ef017c36d/current/VEG_COMP_LYR_R1_POLY_2023.gdb.zip"
# https://catalogue.data.gov.bc.ca/dataset/vri-2023-forest-vegetation-composite-rank-1-layer-r1-
#### Warning R fails to read in downloaded file.
### Read downloaded gdb into QGIS - use the MMQGIS plugin to Modify/Geometry Convert to POLYGON to clean
### Then filter on ORG_UNIT_CODE for the units within the EcoProvince and save FEATURE_ID, OBJECTID, and SITE_INDEX as gpkg
### Read in using the EcoP_01_load.R script
out_name<-'VRIP'
#run function to download and unzip file
#ZipO<-ZipFn(VRI_DC,out_name)
ZipO<-list.files(file.path(DataDir), pattern = ".gdb", full.names = TRUE)[1]
st_layers(ZipO)
#enter the layer name desired at the prompt
layer_nm<-readline(prompt='Enter layer_name: ')
#run function to convert convert to sf and save to disk
VRIP.1<-gdbFn(ZipO,layer_nm,out_name)

#Streams
Streams_DC<-'ftp://ftp.gdbc.gov.bc.ca/sections/outgoing/bmgs/FWA_Public/FWA_STREAM_NETWORKS_SP.zip'
out_name<-'StreamsP'
ZipO<-ZipFn(Streams_DC,out_name)
st_layers(ZipO)
#enter the layer name desired at the prompt
layer_nm<-readline(prompt='Enter layer_name: ')
#run function to convert convert to sf and save to disk
StreamsP<-gdbFn(ZipO,layer_nm,out_name)

