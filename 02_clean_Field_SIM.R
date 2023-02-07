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
# checks if there are WTLND_ID that were sampled but not present in SampleStrata
# checks if it is a name change or if it is a new set of wetlands that needs to be added

#Read in EcoProvince Field Data - only the 2022 data
Field2022<-st_read(file.path(dataOutDir,paste0(WetlandAreaDir,"_2022Field.gpkg")))
table(Field2022$Sampled)#25
table(Field2022$YearSampled)
table(Wetlands$YearSampled)

#Clean up field data
#Rename SIM_10296 to something different since a new wetland -see Jan 1,2023 Kyla Rushton email
#first find a new WTLND_ID
#SampleStrata2022.in.ng<-SampleStrata2022.in %>% st_drop_geometry() %>% dplyr::select(WTLND_ID)
#WTLND_ID.find<-SampleStrata2022.in.ng %>% dplyr::filter(WTLND_ID=='SIM_25987') #0
Field2022<-Field2022 %>%
  mutate(WTLND_ID=ifelse(WTLND_ID=='SIM_10296','SIM_25987',WTLND_ID)) %>%
  #clean up WTLND_ID with ?
  mutate(WTLND_ID=ifelse(WTLND_ID=='SIM_10581?','SIM_10581',WTLND_ID)) %>%
  mutate(YearSampled=2022)

#Read in SampleStrata2022 data and clean up, so has only 2021
SampleStrata2022.in<-read_sf(file.path(spatialOutDirDesign,"SampleStrata2022.gpkg"))
table(SampleStrata2022.in$YearSampled)

#clean up the 2022 data so only pre 2022 samples - rename to 2021
SampleStrata2022.in.1<-SampleStrata2022.in %>%
  #dplyr::filter(YearSampled==1)
  dplyr::mutate(YearSampled=ifelse(YearSampled==1,0,YearSampled)) %>%
  dplyr::mutate(YearSampled=ifelse(YearSampled==2022,0,YearSampled)) %>%
  dplyr::mutate(Sampled=ifelse(YearSampled==0,0,1)) %>%
  dplyr::mutate(SampleType=ifelse(Sampled==0,0,SampleType))
table(SampleStrata2022.in.1$Sampled)
table(SampleStrata2022.in.1$YearSampled)
write_sf(SampleStrata2022.in.1, file.path(spatialOutDir,"SampleStrata2022.in.1.gpkg"))

#Save the pre-made flow attributes for the sample design
FlowAttributesL<-c("stream_intersect" , "river_intersect", "mmwb_intersect", "lake_intersect",
                   "split_by_stream",  "stream_start", "stream_end", "Verticalflow", "Bidirectional",
                   "Throughflow", "Outflow", "Inflow", "granitic_bedrock","max_stream_order")
#BVWFattributesL<-c("dist_to_road","parcelmap_private","partner_site")
BVWFattributesL<-c("dist_to_road","parcelmap_private","Nation","pct_private_ovlp")

FlowAttributes<-SampleStrata2022.in.1 %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID,FlowAttributesL) %>%
  mutate(Inflow=ifelse(Inflow=="`","No",Inflow))
WriteXLS(FlowAttributes,file.path(dataOutDirDesign,paste('FlowAttributes.xlsx',sep='')))

BVWFattributes<-SampleStrata2022.in.1 %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID,BVWFattributesL)
WriteXLS(BVWFattributes,file.path(dataOutDirDesign,paste('BVWFattributes.xlsx',sep='')))

dropDesignAttributes<-c("dist_to_road","stream_intersect","river_intersect","mmwb_intersect","lake_intersect",
                        "split_by_stream","stream_start","stream_end","max_stream_order","granitic_bedrock",
                        "BEC_BCWF","BEC","Verticalflow","Bidirectional","Throughflow",
                        "Outflow","Inflow","FlowCode","Water","nRiver",
                        "nLake","LakeSize","LargeWetland","DisturbCode","DisturbType",
                        "LandCoverType","LandCCode","Land_Disturbance","fire_history","Land_Cover",
                        "parcelmap_private",
                        "pcentIn500Buf","win500",
                        "Nation","pct_private_ovlp")
SampleCols <- c(Sampled = NA, YearSampled = NA, SampleType = NA,
                Observed_Landcover = NA, Observed_Disturbance = NA)

SampleStrata2022 <- SampleStrata2022.in.1 %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  dplyr::select(-any_of(dropDesignAttributes)) %>%
  add_column(!!!!!!SampleCols[!names(SampleCols) %in% names(.)]) %>%
  mutate_at(c("Sampled","SampleType","YearSampled"), as.numeric)
#dplyr::filter(area_Ha<0.25) %>%
table(SampleStrata2022$Sampled, SampleStrata2022$YearSampled) #51, 16

#Check if wetlands sampled in 2022 are in SampleStrata
WTLND_Check<-SampleStrata2021 %>%
  dplyr::filter(WTLND_ID %in% Field2022$WTLND_ID)
nrow(WTLND_Check)#25 new sites
#WTLND_ID.find<-SampleStrata2021 %>% dplyr::filter(WTLND_ID=='SIM_19693') #0

WTLND_Check<-Field2022 %>%
  dplyr::filter(!WTLND_ID %in% SampleStrata2021$WTLND_ID)
nrow(WTLND_Check)# 1 labels not in Sample Strata
# What ids not in strata?
WTLND_Check$WTLND_ID #"SIM_25987" - new label added - see above

WTLND_Check<-SampleStrata2021 %>%
  dplyr::filter(Sampled>0)
nrow(WTLND_Check)#46 original

#Drop field wetlands from SampleStrata2021 & select sub-set of attributes
SampleStrata2021.2 <- SampleStrata2021 %>%
  dplyr::filter(!WTLND_ID %in% Field2022$WTLND_ID)
write_sf(SampleStrata2021.2, file.path(spatialOutDir,"SampleStrata2021.2.gpkg"))

SampleStrata2021.2.tojoin<-SampleStrata2021.2 %>%
  st_drop_geometry() %>%
  dplyr::select(-c(Sampled,YearSampled,SampleType))

SampleStrata2021.3 <- SampleStrata2021.2 %>%
  dplyr::select(WTLND_ID,Sampled,YearSampled,SampleType)

#rbind field data to SampleStrata2021 data set
#Bind the 2 data sets together and recalculate the variables
WetlandsAll <- rbind(SampleStrata2021.3,Field2022) %>%
  mutate(wet_id=as.numeric(rownames(.))) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  left_join(SampleStrata2021.2.tojoin)

table(WetlandsAll$Sampled)#34
table(WetlandsAll$YearSampled)
#    0  2021  2022
#25902    46    26

write_sf(WetlandsAll, file.path(spatialOutDir,"WetlandsAll.gpkg"))
