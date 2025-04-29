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

#Script read new field data and updates SampleStrata
# Typically requires manual over site to get data worked up and consistent
# checks if there are WTLND_ID that were sampled but not present in SampleStrata
# checks if it is a name change or if it is a new set of wetlands that needs to be added
# may require extensive modifications to field data depending on its condition

# Read in disturbance LUT for interpreting field observed disturbance
RdDisturb_LUT<-readRDS(file='tmp/RdDisturb_LUT')
Disturb_LUT<-readRDS(file='tmp/Disturb_LUT')

#Read in most recent SampleStrata file - written from Design to draw, draw only writes to time stamped output files
# Add ObsDist and ObsLandcover so can add field data and their are comparable attributes in the non-sampled wetlands
timeStamp<-'02_November_2023_15_05'
#Note SampleStrataGeo_2024_SI.gpkg name changed to SampleStrataGeo_2024_SI_A.gpkg
# since drawing a second time based on 2024 data
SampleStrata2024.in<-read_sf(file.path(file.path(spatialOutDirDraw,'SampleStrataGeo_2024_SI_A.gpkg'))) %>%
  #Assign Primary, Secondary and Tertiary from DisturbType and LandsCoverType in old data - so can match new data
  mutate(Primary_DstrbTyp=DisturbType) %>%
  mutate(Primary_LndCvr=LandCoverType) %>%
  mutate(Secondary_DstrbTyp=DisturbType) %>%
  mutate(Secondary_LndCvr=LandCoverType) %>%
  mutate(Tertiary_Dstrbtyp=DisturbType) %>%
  mutate(Tertiary_LndCvr=LandCoverType) %>%
  #Some disturbance types are NA, set to 'not disturbed'
  mutate(Primary_DstrbTyp=ifelse(is.na(Primary_DstrbTyp),'not disturbed',Primary_DstrbTyp)) %>%
  dplyr::select(-c(ObsDist1,ObsDist2,ObsLandcover1,ObsLandcover2))

#Data checking
unique(SampleStrata2024.in$LandCoverType)
unique(SampleStrata2024.in$Primary_DstrbTyp)
unique(SampleStrata2024.in$BEC)
Check_SampleStrata<-SampleStrata2024.in %>%
  dplyr::filter(is.na(BEC))

#RdDisturbType can be combined with disturbance in field data
table(SampleStrata2024.in$DisturbType,SampleStrata2024.in$RdDisturbType)
table(SampleStrata2024.in$YearSampled)

#Read in EcoProvince Field Data and check data
Field2024.in<-st_read(file.path(spatialOutDir,paste0(WetlandAreaDir,"_2024Field.gpkg"))) %>%
  mutate(Primary_DstrbTyp=ifelse(is.na(Primary_DstrbTyp),'not_disturbed',Primary_DstrbTyp))
table(Field2024.in$Sampled)
table(Field2024.in$YearSampled)
Check_Field<-Field2024.in %>%
  dplyr::filter(is.na(Primary_LndCvr))

unique(Field2024.in$Primary_LndCvr)
Field2024.1<-Field2024.in %>%
  mutate(YearSampled=ifelse((is.na(YearSampled) | YearSampled==0),2022,YearSampled)) %>%
  mutate(Primary_LndCvr=ifelse(WTLND_ID=='SI_19936','Grassland',Primary_LndCvr))
unique(Field2024.1$Primary_LndCvr)

LndC<-c('Primary_LndCvr', 'Secondary_LndCvr', 'Tertiary_LndCvr')
DisT<-c('Primary_DstrbTyp', 'Secondary_DstrbTyp', 'Tertiary_Dstrbtyp')

table(Field2024.1$Primary_LndCvr)
table(Field2024.1$Primary_DstrbTyp)
table(Field2024.1$Secondary_LndCvr)
table(Field2024.1$Secondary_DstrbTyp)
table(Field2024.1$Tertiary_LndCvr)
table(Field2024.1$Tertiary_Dstrbtyp)
table(SampleStrata2024.in$Primary_LndCvr)
table(SampleStrata2024.in$Primary_LndCvr)

#May need to clean certain sites...
#Field2024 WTLND_ID=='SI_19936' has no Land Cover
Check<-SampleStrata2024.in %>%
  dplyr::filter(WTLND_ID=='SI_19936')
#Check to see if Field data has proper attributes for disturbance and land cover
#run through fuzzy match - first for Land cover
LndCovMatch<-lapply(1:3, function (i) {
  df1<-data.frame(Primary_LndCvr=unique(Field2024.1[[LndC[i]]]))
  df2<-data.frame(DisturbType=unique(SampleStrata2024.in[['Primary_LndCvr']]))
  matched_data <- stringdist_join(
    df1, df2,
    by = "Primary_LndCvr",  # Column to match on
    mode = "full",  # Type of join: left, inner, or full
    method = "jw",  # Jaro-Winkler distance metric
    max_dist = .175,  # Maximum allowable distance for a match
    ignore_case=FALSE
  )
  print("Matched Data:")
  print(matched_data)
  return(matched_data)
})
DistMatch<-lapply(1:3, function (i) {
  df1<-data.frame(DstrbTyp=unique(Field2024.1[[DisT[i]]]))
  df2<-data.frame(DisturbType=unique(SampleStrata2024.in[['DisturbType']]))
  matched_data <- stringdist_join(
    df1, df2,
    by = c('DstrbTyp'='DisturbType'),  # Column to match on
    mode = "full",  # Type of join: left, inner, or full
    method = "jw",  # Jaro-Winkler distance metric
    max_dist = .175,  # Maximum allowable distance for a match
    ignore_case=FALSE
  )
  print("Matched Data:")
  print(matched_data)
  write.csv(matched_data, file=file.path(dataOutDir,paste0('DistMatch_',DisT[i],'.csv')), row.names = FALSE)
  return(matched_data)
})

#Clean up field data so that it has proper attribute names
#write out and manually match up fields for those not caught by fuzzy match
matched_dataUp.1<-read.csv(file=file.path(dataOutDir,paste0('DistMatch_',DisT[1],'U.csv'))) %>%
  dplyr::rename(Primary_DstrbTyp=DstrbTyp)
matched_dataUp.2<-read.csv(file=file.path(dataOutDir,paste0('DistMatch_',DisT[2],'U.csv'))) %>%
  dplyr::rename(Secondary_DstrbTyp=DstrbTyp)
matched_dataUp.3<-read.csv(file=file.path(dataOutDir,paste0('DistMatch_',DisT[3],'U.csv'))) %>%
  dplyr::rename(Tertiary_Dstrbtyp=DstrbTyp)

#Clean up field data with known problems and results of fuzzy match disturbance values
Field2024.2<-Field2024.1  %>%
  left_join(matched_dataUp.1) %>%
  dplyr::select(!Primary_DstrbTyp) %>%
  dplyr::rename(Primary_DstrbTyp=DisturbType) %>%
  left_join(matched_dataUp.2) %>%
  dplyr::select(!Secondary_DstrbTyp) %>%
  dplyr::rename(Secondary_DstrbTyp=DisturbType) %>%
  left_join(matched_dataUp.3) %>%
  dplyr::select(!Tertiary_Dstrbtyp) %>%
  dplyr::rename(Tertiary_Dstrbtyp=DisturbType)

unique(Field2024.2$Primary_LndCvr)
#move roads to their own category
rdCode<-c('low use roads','mod use roads','high use roads')

Field2024<-Field2024.2 %>%
  mutate(RdDisturbTypeP=ifelse(Primary_DstrbTyp %in% rdCode, (Primary_DstrbTyp), NA)) %>%
  mutate(RdDisturbTypeS=ifelse(Secondary_DstrbTyp %in% rdCode, (Secondary_DstrbTyp), NA)) %>%
  mutate(RdDisturbTypeT=ifelse(Tertiary_Dstrbtyp %in% rdCode, (Tertiary_Dstrbtyp), NA)) %>%
  #concatenate across 3 disturbance types to make 1 rd type
  unite(RdDisturbType, c(RdDisturbTypeP,RdDisturbTypeS,RdDisturbTypeT),sep=',',na.rm=TRUE,remove=TRUE) %>%
  #one case where roads show up in multiple disturbance categories
  mutate(RdDisturbType=ifelse(WTLND_ID=='SI_13773','low use roads',RdDisturbType)) %>%
  #drop out road categoreis from Primary, Secondary and Tertiary
  mutate(Primary_DstrbTyp=replace(Primary_DstrbTyp,Primary_DstrbTyp %in% rdCode, NA)) %>%
  mutate(Secondary_DstrbTyp=replace(Secondary_DstrbTyp,Secondary_DstrbTyp %in% rdCode, NA)) %>%
  mutate(Tertiary_Dstrbtyp=replace(Tertiary_Dstrbtyp,Tertiary_Dstrbtyp %in% rdCode, NA)) %>%
  #move Tertiary to Secondary is Secondary is empty
  mutate(Secondary_DstrbTyp=if_na(Secondary_DstrbTyp,Tertiary_Dstrbtyp)) %>%
  mutate(Primary_DstrbTyp=ifelse(is.na(Primary_DstrbTyp),'not disturbed',Primary_DstrbTyp)) %>%
  mutate(Secondary_DstrbTyp=ifelse(is.na(Secondary_DstrbTyp),'not disturbed',Secondary_DstrbTyp)) %>%
  mutate(Tertiary_Dstrbtyp=ifelse(is.na(Tertiary_Dstrbtyp),'not disturbed',Tertiary_Dstrbtyp))

unique(Field2024$Primary_LndCvr)
unique(Field2024$Primary_DstrbTyp)

#Make sure Wetlands' and SampleStrata files Sampled fields are populated with Field data and save
Field.sampled<-Field2024 %>%
    st_drop_geometry() %>%
    dplyr::select(WTLND_ID,Sampled,YearSampled)
SampleStrata2024.1 <- SampleStrata2024.in %>%
  dplyr::select(!c(Sampled,YearSampled)) %>%
  left_join(Field.sampled,by=c('WTLND_ID')) %>%
  mutate(across(where(is.numeric), tidyr::replace_na, 0))
  #replace(is.na(.), 0)

#Check number joined
nrow(SampleStrata2024.1 %>% filter(Sampled==1)) #84, should be 86 - 2 missing see next code block
#Check what did not join -setdiff(Wetlands$WTLND_ID, SampleStrata2023.in$WTLND_ID)
LostWetlands<-Field.sampled %>%
  anti_join(SampleStrata2024.1,by=c('WTLND_ID'))
#LostWetlands$WTLND_ID #"SI_99906","SI_99907"
LostWetlandsF<-SampleStrata2024 %>%
  dplyr::filter(WTLND_ID %in% c("SI_99906","SI_99907"))

#non-wetland, and small wetland fragment have been dropped in SampleStrata - drop in wetlands file
# c("SI_1166","SI_3146")
#tt<-SampleStrata2024 %>%
#  dplyr::filter(WTLND_ID %in% c("SI_1166","SI_3146"))
#DropWetlands<-c("SI_1166","SI_3146")
#SampleStrata2023<-SampleStrata2023 %>%
#  dplyr::filter(!WTLND_ID %in% DropWetlands)

#Add unjoined to Wetlands file - so complete, but check if overlap
#Gather all the attributes from the SampleDesign file, save and remove from strata file

SDatts<-colnames(SampleStrata2024.1)
SampleStrata2024.atts<-SampleStrata2024.1 %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID,!intersect(names(.),names(Field2024)))# %>%

#Compare sizes of Field polygons with original
WTLND_Area_Check.1<-SampleStrata2024.1 %>%
  dplyr::filter(WTLND_ID %in% Field2024$WTLND_ID) %>%
  mutate(SSarea_Ha=as.numeric(st_area(.)*0.0001)) %>%
  st_drop_geometry()
WTLND_Area_Check<-Field2024 %>%
  mutate(Farea_Ha=as.numeric(st_area(.)*0.0001)) %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID, Farea_Ha,Shape_Area,area_Ha) %>%
  left_join(WTLND_Area_Check.1)
#If original wetlands have been modified based on field conditions - drop all and use field wetlands instead

#27 March 2025 - no area changes, just 2 lost wetlands
#Drop field wetlands from SampleStrata2023 and only keep columns that are
# in Field data and add dummy columns for those that arent in
#Disturb and Land Type are Adusted above on input
SampleStrata2024.2 <- SampleStrata2024.1 %>%
  dplyr::filter(!WTLND_ID %in% Field2024$WTLND_ID) %>%
  dplyr::select(intersect(names(.),names(Field2024)))

#Set up data to bind - but only with common columns - most others are office questions and will be exported for the
# office step later
Field2024.toBind<-Field2024 %>%
  dplyr::select(intersect(names(.),names(SampleStrata2024.2)))

#Bind the 2 data sets together
SampleStrata2024.3 <- rbind(SampleStrata2024.2,Field2024.toBind)

#Add back in all the sample design attributes - except for the 2 missing and recalculate the variables
SampleStrata2024 <-SampleStrata2024.3 %>%
  left_join(SampleStrata2024.atts) %>%
  mutate(wet_id=as.numeric(rownames(.))) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001))

#Fix Large_Wetlands - result from added wetlands
Large_Wetlands<- SampleStrata2024 %>%
  dplyr::filter(area_Ha>=1000)
#Make Wetlands all the rest
SampleStrata2024 <- SampleStrata2024 %>%
  mutate(LargeWetland=if_else(WTLND_ID %in% Large_Wetlands$WTLND_ID, ">=1000", "<1000"))
#Wetlands to Fix
WetsToFix<-SampleStrata2024 %>%
  dplyr::filter(WTLND_ID %in% c("SI_20698.1","SI_99901","SI_99902","SI_99904","SI_99905","SI_99906","SI_99907"))

#Fix BEC
Check_SampleStrata<-SampleStrata2024 %>%
  dplyr::filter(is.na(BEC))
Check_SampleStrata$WTLND_ID
SampleStrata2024 <- SampleStrata2024 %>%
  mutate(BEC=ifelse(WTLND_ID=="SI_99901",'Low-Dry',BEC)) %>%
  mutate(BEC=ifelse(WTLND_ID=="SI_20698.1",'Low-Dry',BEC)) %>%
  mutate(BEC=ifelse(WTLND_ID=="SI_99905",'Low-Dry',BEC)) %>%
  mutate(BEC=ifelse(WTLND_ID=="SI_99902",'High',BEC)) %>%
  mutate(BEC=ifelse(WTLND_ID=="SI_99904",'Low-Wet',BEC)) %>%
  mutate(BEC=ifelse(WTLND_ID=="SI_99906",'Low-Dry',BEC)) %>%
  mutate(BEC=ifelse(WTLND_ID=="SI_99907",'Low-Dry',BEC))
#Fix Flow Code
unique(SampleStrata2024$FlowCode)
SampleStrata2024 <- SampleStrata2024 %>%
  mutate(FlowCode=ifelse(WTLND_ID=="SI_20698.1",3,FlowCode)) %>%
  mutate(FlowCode=ifelse(WTLND_ID=="SI_99901",1,FlowCode)) %>%
  mutate(FlowCode=ifelse(WTLND_ID=="SI_99902",1,FlowCode)) %>%
  mutate(FlowCode=ifelse(WTLND_ID=="SI_99904",1,FlowCode)) %>%
  mutate(FlowCode=ifelse(WTLND_ID=="SI_99905",1,FlowCode)) %>%
  mutate(FlowCode=ifelse(WTLND_ID=="SI_99906",3,FlowCode)) %>%
  mutate(FlowCode=ifelse(WTLND_ID=="SI_99907",3,FlowCode))
#Modify - by inspection - Disturbance and Land cover so if more rare in Secondary or Tertiary they become Primary
# Do an initial run through the draw routine to determine which landcover and disturbance are the most rare
# modify primary if the wetlane has a rare secondary or tertiary
Rare_LC<-c("Cropland","Grassland" )
Rare_disturb<-c("historic fire","mine, OGC infrastructure","ROW, Power, Rail, OGC geophysical","recent fire")
Check_rare<-Field2024 %>%
  dplyr::select(WTLND_ID, Primary_DstrbTyp,Secondary_DstrbTyp,Tertiary_Dstrbtyp,Primary_LndCvr,Secondary_LndCvr,Tertiary_LndCvr)
SampleStrata2024 <- SampleStrata2024 %>%
  mutate(Primary_DstrbTyp=ifelse(WTLND_ID=="SI_5280",Secondary_DstrbTyp,Primary_DstrbTyp)) %>%
  mutate(Primary_DstrbTyp=ifelse(WTLND_ID=="SI_7552",Tertiary_Dstrbtyp,Primary_DstrbTyp)) %>%
  mutate(Primary_DstrbTyp=ifelse(WTLND_ID=="SI_17878",Secondary_DstrbTyp,Primary_DstrbTyp)) %>%
  mutate(Primary_DstrbTyp=ifelse(WTLND_ID=="SI_18864",Secondary_DstrbTyp,Primary_DstrbTyp)) %>%
  mutate(Primary_DstrbTyp=ifelse(WTLND_ID=="SI_10256",Tertiary_Dstrbtyp,Primary_DstrbTyp)) %>%
  mutate(Primary_DstrbTyp=ifelse(WTLND_ID=="SI_17771",Tertiary_Dstrbtyp,Primary_DstrbTyp)) %>%
  mutate(Primary_LndCvr=ifelse(WTLND_ID=="SI_5280",Secondary_LndCvr,Primary_LndCvr)) %>%
  mutate(Primary_LndCvr=ifelse(WTLND_ID=="SI_19591",Secondary_LndCvr,Primary_LndCvr)) %>%
  mutate(Primary_LndCvr=ifelse(WTLND_ID=="SI_19041",Secondary_LndCvr,Primary_LndCvr)) %>%
  mutate(Primary_LndCvr=ifelse(WTLND_ID=="SI_16997",Secondary_LndCvr,Primary_LndCvr)) %>%
  mutate(Primary_LndCvr=ifelse(WTLND_ID=="SI_99902",Secondary_LndCvr,Primary_LndCvr))

#Modify unknown
Check_unknow<-SampleStrata2024 %>%
  dplyr::filter(Primary_DstrbTyp=='unknown') %>%
  dplyr::select(WTLND_ID, Primary_DstrbTyp,Secondary_DstrbTyp,Tertiary_Dstrbtyp,Primary_LndCvr,Secondary_LndCvr,Tertiary_LndCvr)

#Modify RdDisturbType
Check_unknow<-SampleStrata2024 %>%
  dplyr::filter(RdDisturbType=='No') %>%
  dplyr::select(WTLND_ID, Primary_DstrbTyp,Secondary_DstrbTyp,Tertiary_Dstrbtyp,Primary_LndCvr,Secondary_LndCvr,Tertiary_LndCvr)
SampleStrata2024<-SampleStrata2024 %>%
  mutate(RdDisturbType=ifelse(RdDisturbType=="","No",RdDisturbType))
unique(SampleStrata2024$RdDisturbType)

#Write out to Draw directory
SampleStrata2024.data<-SampleStrata2024 %>%
  st_drop_geometry()
outFileN<-paste0('SampleStrata_2024_',WetlandAreaShort,'.csv')
write.csv(SampleStrata2024.data, file=file.path(spatialOutDirDraw,outFileN), row.names = FALSE)

SampleStrata2024.geo<-SampleStrata2024 %>%
  dplyr::distinct(.,WTLND_ID,.keep_all=TRUE)
outFileNGeo<-paste0('SampleStrataGeo_2024_',WetlandAreaShort,'.gpkg')
write_sf(SampleStrata2024.geo, file.path(spatialOutDirDraw,outFileNGeo), delete_dsn = T)

table(SampleStrata2024.geo$Sampled)
table(SampleStrata2024.geo$YearSampled)
#0        2022 2023  2024
#20367    21    17    34

Check<-SampleStrata2024.geo %>%
  dplyr::filter(Sampled==1) %>%
  dplyr::select(WTLND_ID,YearSampled,geom)
table(st_is_valid(SampleStrata2024.geo))

SampleStrata2024.data.check<-SampleStrata2024.data %>%
  dplyr::filter(Sampled==1)

message('Breaking')
break

