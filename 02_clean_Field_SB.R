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

#Read original 2021 data for refrence
SampleStrata_Original<-st_read(file.path(spatialOutDir,"SampleStrata_2022.gpkg"))

#Read in EcoProvince Field Data - only the 2022 data
Field2022<-st_read(file.path(dataOutDir,paste0(WetlandAreaDir,"_2022Field.gpkg"))) %>%
  dplyr::filter(YearSampled==2022)

table(Field2022$Sampled)#18
table(Field2022$YearSampled)

#Read in 2021 data with all historic samples
SampleStrata2021in<-st_read(file.path(spatialOutDir,"WetlandsAll.gpkg"))
table(SampleStrata2021in$Sampled)
table(SampleStrata2021in$YearSampled)

SampleStrata2021<-SampleStrata2021in

#Check if wetalnds sampled in 2022 are in SampleStrata
WTLND_Check<-SampleStrata2021 %>%
  dplyr::filter(WTLND_ID %in% Field2022$WTLND_ID)
nrow(WTLND_Check)#14 new sites
WTLND_Check<-Field2022 %>%
  dplyr::filter(!WTLND_ID %in% SampleStrata2021$WTLND_ID)
nrow(WTLND_Check)# 4 labels not in Sample Strata
# What ids not in strata?
WTLND_Check$WTLND_ID #"SB_22351AA" "SB_25174AA" "SB_18690AA" "SB_9490AA"
WTLND_Check<-SampleStrata2021 %>%
  dplyr::filter(Sampled>0)
nrow(WTLND_Check)#16 original

#Update Sample Strata with 2022 field data and new 'Observed' fields
#Find corresponding Wetlands in Sample Strata and remove from set - so can replace with field versions
#Loop through with each Field2022$WTLND_ID
FieldSamples<-Field2022$WTLND_ID
ParseWetFn <- function(wetN){
  SampleStrata2021.1 %>%
    st_drop_geometry() %>%
    #use grep to search WTLND_ID_old string since contains multiple WTLND_IDs if a wetland has been aggregated in new make wetland process
    dplyr::filter(str_detect(WTLND_ID_old,FieldSamples[wetN]))
  #return(SampleStrata2021.drop)
}
#Finds 13, 1 is missing and 4 have characters after them and have been picked up
# since their non-character part has been identified
SampleStrata2021.drop<-
  do.call(rbind,(lapply(c(1:length(FieldSamples)), function(i) ParseWetFn(i))))

#Look for the 1 missing Wetlands in original data
wetCheck<-SampleStrata_Original %>%
  filter(WTLND_ID=='SB_36643') #not there - checked spatial as well -
 #looks like added in the new data set it is now SB_34080
#Add missing to selection by adding its label to SB_34080 record's old field - then go back and re-run
SampleStrata2021.1<-SampleStrata2021 %>%
  mutate(WTLND_ID_old=ifelse(wet_id==34080,'SB_36643',WTLND_ID_old))

#Drop selected wetlands from SampleStrata2021
SampleStrata20201.2 <- SampleStrata2021.1 %>%
  dplyr::filter(!wet_id %in% SampleStrata2021.drop$wet_id) %>%
  dplyr::select(-c(wet_id,WTLND_ID,area_Ha))
colnames(SampleStrata20201.2)

write_sf(SampleStrata20201.2, file.path(spatialOutDir,"SampleStrata20201.2.gpkg"))

#Check is any were PEM wetlands
PEMwetsInField<-SampleStrata2021.drop %>%
  dplyr::filter(PEMwetland==1)

#rbind field data to SampleStrata2021 data set
#first get the data.frame structured the same as the sample strata
colnames(Field2022)
Field2022.1<-Field2022 %>%
  mutate(PEMwetland=ifelse(WTLND_ID %in% PEMwetsInField$WTLND_ID_old,1,0)) %>%
  dplyr::select(WTLND_ID_old=WTLND_ID,Sampled,YearSampled,SampleType,PEMwetland)
write_sf(Field2022.1, file.path(spatialOutDir,"Field2022.12.gpkg"))

#Bind the 2 data sets together and recalculate the variables
WetlandsAll <- rbind(SampleStrata20201.2,Field2022.1) %>%
  mutate(wet_id=as.numeric(rownames(.))) %>%
  mutate(WTLND_ID=paste0(WetlandAreaShort,'_',wet_id)) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001))

table(WetlandsAll$Sampled)#34
table(WetlandsAll$YearSampled)
#0      2019  2021  2022
#66611    13     3    18

write_sf(WetlandsAll, file.path(spatialOutDir,"WetlandsAll.gpkg"))
