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

#Read in base wetlands with PEM attributes and drop WTLND_ID since it will be
# re-generated using old WTLND_ID suplemented with new labels where required.
New_wetlands.1<-st_read(file.path(spatialOutDir,"WetlandsAll_wPEM.gpkg")) %>%
  mutate(wet_idN = as.numeric(rownames(.)))

SampleStrata_backup<-st_read(file.path(spatialOutDir,"SampleStrata_2022.gpkg")) %>%
  mutate(wet_idO = as.numeric(rownames(.))) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001))

#write out a backup since being replaced
#write_sf(Old_wetlands.1, file.path(spatialOutDirDesign,"SampleStrata_backup.gpkg"))
#Also write to local
#write_sf(SampleStrata_backup, file.path(spatialOutDir,"SampleStrata_2022.gpkg"))

Old_wetlands<- Old_wetlands.1 %>%
  dplyr::select(WTLND_ID, wet_idO)
#Check the number of wetlands that have been histrionically sampled
# includes some of the ESI wetlands - may only be ESI wetlands collected 2019 and 1 in 2021
OldSampled<-Old_wetlands.1 %>%
  st_drop_geometry() %>%
  dplyr::select(c(WTLND_ID,wet_idO,Sampled,YearSampled,SampleType)) %>%
  dplyr::filter(YearSampled>0)#dplyr::filter(Sampled>0)
nrow(OldSampled) #12

# all of the ESI sampled wetlands are in the new set
ESISampled<-New_wetlands.1 %>%
  st_drop_geometry() %>%
  dplyr::filter(YearSampled>0)#dplyr::filter(Sampled>0)
nrow(ESISampled) #16 with 4 more from new base wetlands

#pull out sampled wetlands
IntersectOld<-as.data.frame(st_intersects(New_wetlands.1,Old_wetlands))
colnames(IntersectOld)<-c('wet_idN', 'wet_idO')

New_wetlands.2 <- New_wetlands.1 %>%
  full_join(IntersectOld) %>% #all New_wetlands + unmatched Old
  #now link back to Old_wetlands and get old WTLND_ID
  left_join(st_drop_geometry(Old_wetlands), by="wet_idO")
New_wetlands.2.check<-New_wetlands.2 %>%
  st_drop_geometry() %>%
  dplyr::filter(Sampled>0)
nrow(New_wetlands.2.check) #17 - some double counting that should resolve by group_by
#write_sf(New_wetlands.2, file.path(spatialOutDir,"New_wetlands.2.gpkg"))

#Make a one 1 many - list of old wetlands for each new wetland
#capture wetlands not in new
nrow(New_wetlands.1)
nrow(New_wetlands.2)
tt<-New_wetlands.2 %>%
  dplyr::filter(is.na(wet_idO))

#collapse by new wetland land id and get unique WTLND_ID for each new wetland
wetlands_wOldID<-New_wetlands.2 %>%
  group_by(wet_idN) %>%
  dplyr::summarize(num_old=n(),WTLND_ID=paste(sort(unique(WTLND_ID)),collapse=", "))

New_wetlands.3<-wetlands_wOldID %>%
  left_join(st_drop_geometry(New_wetlands.1))

#Generate new WTLND_ID label using old
New_wetlands.4<-New_wetlands.3 %>%
  #st_drop_geometry() %>%
  #save original WTLND_ID(s) -some are now one poly and have multiple old ids
  dplyr::rename(WTLND_ID_old=WTLND_ID) %>%
  mutate(WTLND_ID=paste0(WetlandAreaShort,'_',wet_id)) %>%
  #assign new id, if multiple old ids use first one
 # mutate(WTLND_ID=ifelse(WTLND_ID_old=='',
 #                        paste0(WetlandAreaShort,'_',(10000+wet_id)),
 #                        gsub(",.*$", "", WTLND_ID_old))) %>%
  dplyr::select(wet_id,WTLND_ID,WTLND_ID_old,area_Ha, Sampled,YearSampled,SampleType,PEMwetland) %>%
  #dplyr::select(wet_id,WTLND_ID,WTLND_ID_old,area_Ha, Sampled,YearSampled,SampleType) %>%
  #Set numeric NA to 0
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

#Data Check
nrow(New_wetlands.4)
length(unique(New_wetlands.4$WTLND_ID))
length(unique(New_wetlands.4$wet_id))

n_occur <- data.frame(table(New_wetlands.4$WTLND_ID))
#n_occur[n_occur$Freq > 1,]
NonUnique<-New_wetlands.4[New_wetlands.4$WTLND_ID %in% n_occur$Var1[n_occur$Freq > 1],] %>%
  st_drop_geometry()
write_sf(New_wetlands.4, file.path(spatialOutDir,"New_wetlands.4.gpkg"))

saveRDS(New_wetlands.4, file='tmp/WetlandsAll')
write_sf(New_wetlands.4, file.path(spatialOutDir,"WetlandsAll.gpkg"))

#Peel out the unique SB attributes - may get stripped off by generic routines
# but will want to re-attribute for the final field data sets
SB_supplemental_attributes<-New_wetlands.4 %>%
  st_drop_geometry() %>%
  dplyr::select(wet_id,WTLND_ID,WTLND_ID_old,PEMwetland)
saveRDS(SB_supplemental_attributes, file='tmp/SB_supplemental_attributes')
#Save to Design directory - accessed by draw routines
WriteXLS(SB_supplemental_attributes,file.path(dataOutDirDesign,paste('SB_supplemental_attributes.xlsx',sep='')))

New_wetlands.4.check<-New_wetlands.4 %>%
  st_drop_geometry() %>%
  dplyr::filter(Sampled>0)





