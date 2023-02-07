library(sf)
library(dplyr)
library(readr)
library(raster)
library(bcmaps)
library(rgdal)
library(fasterize)
library(readxl)
library(mapview)
library(WriteXLS)
library(foreign)
library(ggplot2)
library(ggnewscale)
library(viridis)
library(stars)
library(rgrass7)
library(exactextractr)
library(expss)
library(openxlsx)
library(cleangeo)
library(geos)
library(tidyr)
library(plyr)
library(bcdata)
library(tmap)
library(smoothr)
library(terra)
library(rmapshaper)
library(tibble)
library(stringr)

options(scipen=999)
options(warn = 1)
options(timeout=180)

OutDir <- 'out'
OutDirWESP <-'../WESP_Sample_Design/out'
dataOutDir <- file.path(OutDir,'data')
#dataOutDirWESP <- file.path(OutDirWESP,'data')
#tileOutDir <- file.path(dataOutDir,'tile')
#figsOutDir <- file.path(OutDir,'figures')
spatialOutDirP <- file.path(OutDir,'spatial')
#spatialOutDirPWESP <- file.path(OutDirWESP,'spatial')
DataDir <- 'data'
DataDirWESP <- '../WESP_Sample_Design/data'
SpatialDir <- file.path(DataDir,'spatial')
GISLibrary<- file.path('/Users/darkbabine/ProjectLibrary/Library/GISFiles/BC')
WESPDir <- file.path('../WESP_Sample_Design/data')
BioDDir <- file.path('../../Biodiversity/data')
DrawDir <- file.path('../WESP_Sample_Draw/data')
PEMspatialOutDir <-file.path(DataDir,'spatial/Sub_Boreal_PEM')

dir.create(file.path(OutDir), showWarnings = FALSE)
dir.create(file.path(dataOutDir), showWarnings = FALSE)
#dir.create(file.path(tileOutDir), showWarnings = FALSE)
#dir.create(file.path(figsOutDir), showWarnings = FALSE)
dir.create(DataDir, showWarnings = FALSE)
dir.create(SpatialDir, showWarnings = FALSE)
dir.create(PEMspatialOutDir, showWarnings = FALSE)
dir.create("tmp", showWarnings = FALSE)
dir.create(file.path(spatialOutDirP), showWarnings = FALSE)

WetlandAreaL<-list('SIM_Base',c('Taiga_Planes_Base','Boreal_Plains_Base'),
                'Sub_Boreal','GD_Base','GD_Base_Est','Sub_Boreal_PEM','SI_Base')
WetlandAreaDirL<-c('SIM_Base','Taiga_Boreal_Plains',
                   'Sub_Boreal','GD_Base','GD_Base_Est','Sub_Boreal_PEM','SI_Base')
WetlandAreaShortL<-c('SIM','TBP',
                   'SB','GD','GD_Est','SB_PEM','SI')
EcoPNL<-list("SOUTHERN INTERIOR MOUNTAINS",c("BOREAL PLAINS","TAIGA PLAINS"),
             "SUB-BOREAL INTERIOR","GEORGIA DEPRESSION","GEORGIA DEPRESSION","SUB-BOREAL INTERIOR","SOUTHERN INTERIOR")




