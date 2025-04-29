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

#Read in EcoProvince Field Data
Field2022<-st_read(file.path(dataOutDir,paste0(WetlandAreaDir,"_2022Field.gpkg"))) %>%
  dplyr::select(WTLND_ID,Sampled,YearSampled,SampleType)
table(Field2022$Sampled)
table(Field2022$YearSampled)

##GD stuff...

  #mutate(wet_id=seq.int(nrow(.))) %>%
  #mutate(WTLND_ID=paste0(WetlandAreaShort,'_',wet_id)) %>%
  #Filter out bad ids - should be fixed in source file
  dplyr::filter(str_detect(Wtlnd_C, 'GD')) %>%
  dplyr::filter(nchar(Wtlnd_C)<9) %>%
  # mutate(wet_id=seq.int(nrow(.))) %>%
  mutate(wet_id=1:nrow(.)) %>%
  dplyr::select(wet_id,WTLND_ID=Wtlnd_C,area_Ha,Sampled,YearSampled,
                Internal_Flow_dist, Stream_Intersect,GlacialInfluence,Flood_Infastructure,
                ConservationInvestment,Sustained_Sci_Use,
                Primary_LndCvr,Secondary_LndCvr,Tertiary_LndCvr,
                Primary_DstrbTyp,Secondary_DstrbTyp,Tertiary_DstrbTyp,
                Percent_of_catchament,
                LandCoverType=Primary_LndCvr,DisturbType=Primary_DstrbTyp)
#Make sure geometry column is called 'geom'
#Check 2023 data against previous
#Wetlands<-st_read(file.path(spatialOutDir,'Wetlands.gpkg'))
#Field2022Data<-Wetlands %>%
#  dplyr::filter(Sampled==1)
#check sampled prior to 2023
Field2023DataCheck<-Field2023Data %>%
  dplyr::filter(Sampled==1 & YearSampled<2023)
#Check if wetlands are in wetland data base
Field2023DataCheck2<-Wetlands %>%
  dplyr::filter(WTLND_ID %in% Field2023Data$WTLND_ID)
Field.pt<-Field2023Data %>%
  dplyr::filter(!WTLND_ID %in% Wetlands$WTLND_ID)
write_sf(Field.pt, file.path(spatialOutDir,"Field.pt.gpkg"))

#Clean up Wetlands data
# add missing wetlands and update wetland values with new info from sampling
# by dropping then re-adding field wetlands
Missing <- setdiff(names(Wetlands), names(Field2023Data))
Field2023Data[Missing]<-0
FieldsToBind<-Field2023Data %>%
  dplyr::select(names(Wetlands))
Wetlands.F<-Wetlands %>%
  dplyr::filter(!WTLND_ID %in% Field2023Data$WTLND_ID) %>%
  rbind(FieldsToBind)

source('BufferNpoints.R')

#write out field data
write_sf(Field2023Data, file.path(spatialOutDir,"Field2023Data.gpkg"))


#Taiga-Boreal clean up...
#Read in EcoProvince Sample Strata Wetlands from 02_clean_pre_made_wetlands or make
SampleStrata2021<-st_read(file.path(spatialOutDir,"SampleStrata2021.gpkg"))

#issues with pre-built polygons being multi-polygon so cast as polygon and fix

#Check if unique Wetland ids
n_occur <- data.frame(table(SampleStrata2021$WTLND_ID))
n_occur[n_occur$Freq > 1,]
#Make a sf/data frame of non uniques
NonUnique<-SampleStrata2021[SampleStrata2021$WTLND_ID %in% n_occur$Var1[n_occur$Freq > 1],]
#write_sf(NonUnique, file.path(spatialOutDir,"NonUnique.gpkg"))
#Make a data.frame of non unique and check
NonUniqueData<-NonUnique %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  st_drop_geometry()

#drop duplicate records
SampleStrata2021_1 <- SampleStrata2021 %>% distinct()

#Check if multiple WTLDN_IDs
n_occur <- data.frame(table(SampleStrata2021_1$WTLND_ID))
n_occur[n_occur$Freq > 1,]
#change WTLND_ID for unique
NonUnique2<-SampleStrata2021_1[SampleStrata2021_1$WTLND_ID %in% n_occur$Var1[n_occur$Freq > 1],]
NonUniqueData2<-NonUnique2 %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  st_drop_geometry()
saveRDS(SampleStrata2021_1,file='tmp/SampleStrata2021_1')
table(SampleStrata2021_1$Sampled, SampleStrata2021_1$YearSampled) #51

SampleStrata2021_fix <- SampleStrata2021_1 %>%
  dplyr::filter(WTLND_ID %in% NonUniqueData2$WTLND_ID) %>%
  group_by(WTLND_ID) %>%
  mutate(Wet_num=sequence(n())) %>%
  ungroup() %>%
  mutate(WTLND_ID=paste0(WTLND_ID,'_',Wet_num)) %>%
  dplyr::select(-c('Wet_num'))

SampleStrata2021_2<-SampleStrata2021_1 %>%
  dplyr::filter(!WTLND_ID %in% NonUniqueData2$WTLND_ID)

SampleStrata2021.1<- rbind(SampleStrata2021_2,SampleStrata2021_fix)

tt<-SampleStrata2021.1 %>%
  st_drop_geometry() %>%
  dplyr::filter(YearSampled==2021)
# GD_28685 has limited data

#Check if wetalnds sampled in 2022 are in SampleStrata
WTLND_Check<-SampleStrata2021.1 %>%
  dplyr::filter(WTLND_ID %in% Field2022$WTLND_ID)
nrow(WTLND_Check)#6
WTLND_Check<-Field2022 %>%
  dplyr::filter(!WTLND_ID %in% SampleStrata2021.1$WTLND_ID)
nrow(WTLND_Check)#1
WTLND_Check$WTLND_ID #"GD_99999"
WTLND_Check<-SampleStrata2021.1 %>%
  dplyr::filter(Sampled>0)
nrow(WTLND_Check)#63

tt<-SampleStrata2021.1 %>%
  st_drop_geometry() %>%
  dplyr::filter(Sampled==1) %>%
  dplyr::select(WTLND_ID)

ttt<-Field2022 %>%
  st_drop_geometry() %>%
  dplyr::filter(YearSampled==2021) %>%
  dplyr::select(WTLND_ID)


#Save the pre-made flow attributes for the sample design
FlowAttributesL<-c("stream_intersect" , "river_intersect", "mmwb_intersect", "lake_intersect",
                   "split_by_stream",  "stream_start", "stream_end", "Verticalflow", "Bidirectional",
                   "Throughflow", "Outflow", "Inflow", "granitic_bedrock","max_stream_order")
#BVWFattributesL<-c("dist_to_road","parcelmap_private","partner_site")
BVWFattributesL<-c("dist_to_road","parcelmap_private","Nation","pct_private_ovlp")

FlowAttributes<-SampleStrata2021.1 %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID,FlowAttributesL) %>%
  mutate(Inflow=ifelse(Inflow=="`","No",Inflow))
WriteXLS(FlowAttributes,file.path(dataOutDirDesign,paste('FlowAttributes.xlsx',sep='')))

BVWFattributes<-SampleStrata2021.1 %>%
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

SampleStrata2021 <- SampleStrata2021.1 %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  dplyr::select(-any_of(dropDesignAttributes)) %>%
  add_column(!!!!!!SampleCols[!names(SampleCols) %in% names(.)]) %>%
  mutate_at(c("Sampled","SampleType","YearSampled"), as.numeric)
#dplyr::filter(area_Ha<0.25) %>%
table(SampleStrata2021$Sampled, SampleStrata2021$YearSampled) #51, 16

#if not in SampleStrata then will need to add
#Drop field wetlands from SampleStrata2021 & select sub-set of attributes
SampleStrata2021.2 <- SampleStrata2021 %>%
  dplyr::filter(!WTLND_ID %in% Field2022$WTLND_ID)
#write_sf(SampleStrata20201.2, file.path(spatialOutDir,"SampleStrata20201.2.gpkg"))

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

table(WetlandsAll$Sampled)#70
table(WetlandsAll$YearSampled)
#    2021 2022
#     63    7

write_sf(WetlandsAll, file.path(spatialOutDir,"WetlandsAll.gpkg"))


flowCheck<-WetlandsAll %>%
  st_drop_geometry() %>%
  dplyr::filter(WTLND_ID %in% c("GD_28685", "GD_26201", "GD_26246", "GD_27213", "GD_27576", "GD_27905", "GD_27932", "GD_99999"))

