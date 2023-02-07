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
Field2022<-st_read(file.path(dataOutDir,paste0(WetlandAreaDir,"_2022Field.gpkg")))
table(Field2022$Sampled)
table(Field2022$YearSampled)

#Taiga-Boreal clean up...
#Read in EcoProvince Sample Strata Wetlands
#issues with pre-built polygons being multi-polygon so cast as polygon and fix
SampleStrata2021in<-st_read(file.path(DataDir,'2022WetlandData',paste0("SampleStrata_",WetlandAreaShort,".gpkg"))) %>%
  st_cast("POLYGON")
#st_drop_geometry()

#Check if unique Wetland ids
n_occur <- data.frame(table(SampleStrata2021in$WTLND_ID))
n_occur[n_occur$Freq > 1,]
#Make a sf/data frame of non uniques
NonUnique<-SampleStrata2021in[SampleStrata2021in$WTLND_ID %in% n_occur$Var1[n_occur$Freq > 1],]
#write_sf(NonUnique, file.path(spatialOutDir,"NonUnique.gpkg"))
#Make a data.frame of non unique and check
NonUniqueData<-NonUnique %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  st_drop_geometry()

#drop duplicate records
SampleStrata2021_1 <- SampleStrata2021in %>% distinct()

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

SampleStrata2021.1<-rbind(SampleStrata2021_2,SampleStrata2021_fix)

tt<-SampleStrata2021.1 %>%
  st_drop_geometry() %>%
  dplyr::filter(WTLND_ID %in% c('TP_1_2','TP_1_1'))

#Check if wetalnds sampled in 2022 are in SampleStrata
WTLND_Check<-SampleStrata2021.1 %>%
  dplyr::filter(WTLND_ID %in% Field2022$WTLND_ID)
nrow(WTLND_Check)#65
WTLND_Check<-Field2022 %>%
  dplyr::filter(!WTLND_ID %in% SampleStrata2021.1$WTLND_ID)
nrow(WTLND_Check)#2
WTLND_Check$WTLND_ID #"BP_1552AA" "BP_19_2"
WTLND_Check<-SampleStrata2021.1 %>%
  dplyr::filter(Sampled>0)
nrow(WTLND_Check)#51

tt<-SampleStrata2021.1 %>%
  st_drop_geometry() %>%
  dplyr::filter(Sampled==1) %>%
  dplyr::select(WTLND_ID)

ttt<-Field2022 %>%
  st_drop_geometry() %>%
  dplyr::filter(YearSampled==2021) %>%
  dplyr::select(WTLND_ID)

#if not in SampleStrata then will need to add
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






#Update Sample Strata with 2022 field data and new 'Observed' fields
Field2022_Observed<-Field2022 %>%
  st_drop_geometry() %>%
  dplyr::filter(WTLND_ID %in% SampleStrata2021$WTLND_ID) %>%
  dplyr::select(WTLND_ID, Sampled, YearSampled, SampleType, Observed_Landcover, Observed_Disturbance)

SampleStrata2022_1 <- SampleStrata2021 %>%
  dplyr::select(-c("Sampled","SampleType","YearSampled")) %>%
  left_join(Field2022_Observed, by=c("WTLND_ID")) %>%
  relocate(geom, .after = last_col())
table(SampleStrata2022_1$Sampled, SampleStrata2022_1$YearSampled) #47, 18=65

#Check for number of samples in 2021 data
WTLND_Check<-SampleStrata2021 %>%
  dplyr::filter(Sampled==1)
nrow(WTLND_Check)#51
#Check for number of samples in 2022 data
WTLND_Check<-SampleStrata2022_1 %>%
  st_drop_geometry() %>%
  dplyr::filter(Sampled==1)
nrow(WTLND_Check)#65

#Identify wetlands not in SampleStrata Data
AddWetlands<-Field2022 %>%
  dplyr::filter(!WTLND_ID %in% SampleStrata2022_1$WTLND_ID) #2

#Check if wetlands are in SampleStrata but with different names
NameDrop<-c('Sampled', 'YearSampled', 'SampleType', 'Observed_Landcover', 'Observed_Disturbance')
#set up the right order in data frame so cand bind.
colsToAdd1<-names(SampleStrata2022_1[1:4])
colsToAdd2<-colsToAdd1[1:(length(colsToAdd1)-1)]
colsToAdd3<-names(SampleStrata2022_1[5:(ncol(SampleStrata2022_1))])

SampleStrata2022_newID<-SampleStrata2022_1 %>%
  st_intersection(AddWetlands) %>%
  #st_drop_geometry() %>%
  dplyr::select(-(any_of(NameDrop))) %>%
  dplyr::rename(Sampled=Sampled.1) %>%
  dplyr::rename(YearSampled=YearSampled.1) %>%
  dplyr::rename(SampleType=SampleType.1) %>%
  dplyr::rename(Observed_Landcover=Observed_Landcover.1) %>%
  dplyr::rename(Observed_Disturbance=Observed_Disturbance.1) %>%
  dplyr::rename(WTLND_ID_orig=WTLND_ID) %>%
  dplyr::rename(WTLND_ID=WTLND_ID.1) %>%
  dplyr::select(colsToAdd1,colsToAdd3,WTLND_ID_orig)

#### These next steps are manual
#Erase sampled wetland from main set and then add back in next step
#this will reset wetland coverage to not have sampled sites, but will preserve larger
# pieces associated with wetland
SampleStrata2022_E <- SampleStrata2022_1 %>%
  dplyr::filter(WTLND_ID %in% SampleStrata2022_newID$WTLND_ID_orig) %>%
  rmapshaper::ms_erase(AddWetlands, remove_slivers=TRUE) %>%
  st_cast("POLYGON")
#routine changes geometry column name, need to change it back
st_geometry(SampleStrata2022_E) <- "geom"

#Remove small residual polygons from erase and make new unique WTLND_IDs
SampleStrata2022_E1 <- SampleStrata2022_E %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  dplyr::filter(area_Ha>1.25) %>%
  group_by(WTLND_ID) %>%
  mutate(Wet_num=sequence(n())) %>%
  ungroup() %>%
  mutate(WTLND_ID=paste0(WTLND_ID,'__',Wet_num)) %>%
  dplyr::select(-c('area_Ha', 'Wet_num'))

#Remove wetlands from SampleStrata then add back newly created Wetland(with sampled portion removed)
#then add sampled portion -AddWetlands
SampleStrata2022_2<-SampleStrata2022_1 %>%
  dplyr::filter(!WTLND_ID %in% SampleStrata2022_newID$WTLND_ID_orig) %>%
  rbind(SampleStrata2022_E1)

SampleStrata2022 <- SampleStrata2022_newID %>%
  dplyr::select(-c('WTLND_ID_orig')) %>%
  rbind(SampleStrata2022_2)
length(unique(SampleStrata2022$WTLND_ID))
#Write out file and send to 02_clean_pre_design_wetlands to make ready for WESP_Sample_Design
# or WESP_Sample_Draw
write_sf(SampleStrata2022, file.path(spatialOutDir,"SampleStrata2022.gpkg"))
write_sf(SampleStrata2022, file.path(spatialOutDir,"WetlandsAll.gpkg"))


