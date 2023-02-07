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

#Scan the directory with the 2022 Field data and read in all the shape files into a list
#remove any non target directorys into the '2022Field DataOther'2022FieldDataOther' folder
Field2022DataFiles<-list.files(path=file.path(DataDir,'2022FieldData'),
                             recursive=TRUE, pattern='shp$')
Field2022Data <- lapply(file.path(DataDir,'2022FieldData',
                                                   Field2022DataFiles), st_read)
#Get the EcoProvince identifier from the file name
Field2022DataFileNames<-sub("\\_2022.*", "", Field2022DataFiles)
#Assign EcoProvince name to each element of the shapefile list
names(Field2022Data)<-Field2022DataFileNames

#Transform each EcoProvince to BC Albers and save file.
Taiga_Boreal_Plains_2022Field<-Field2022Data[[1]]
#Need to fix column names
colnames(Taiga_Boreal_Plains_2022Field)
st_crs(Taiga_Boreal_Plains_2022Field) #EPSG:3857
Taiga_Boreal_Plains_2022Field1<-Taiga_Boreal_Plains_2022Field %>%
  st_transform(crs=Prov_crs) %>%
  mutate(SampleType=1) %>% # add since not in Taiga data
  #mutate(YearSample=if_else(YearSample==0,2022,YearSample)) %>% #some have sampled==1 but no YearSampled
  dplyr::select(WTLND_ID=WTLND_I,YearSampled=YearSample, Sampled, SampleType,
                Observed_Landcover=Obs_Landco,Observed_Disturbance=Obs_Distur) %>% #SampleType missing in field data
  dplyr::filter(Sampled==1 & YearSampled>0) %>%
  dplyr::filter(WTLND_ID!='BP_19')

st_geometry(Taiga_Boreal_Plains_2022Field1) <- "geom"

BP_19_2<-st_read(file.path(DataDir,paste0("2022FieldDataOther/BP_19_2.gpkg"))) %>%
  st_zm() %>%
  mutate(WTLND_ID='BP_19_2') %>%
  mutate(YearSampled=2021) %>%
  mutate(Sampled=1) %>%
  mutate(SampleType=1) %>%
  mutate(Observed_Landcover=NA) %>%
  mutate(Observed_Disturbance=NA) %>%
  dplyr::select(WTLND_ID,YearSampled,Sampled, SampleType,
                Observed_Landcover,Observed_Disturbance)
Taiga_Boreal_Plains_2022Field<-rbind(Taiga_Boreal_Plains_2022Field1,BP_19_2)

table(Taiga_Boreal_Plains_2022Field$YearSampled)

write_sf(Taiga_Boreal_Plains_2022Field, file.path(dataOutDir,"Taiga_Boreal_Plains_2022Field.gpkg"))

GD_Base_2022Field<-Field2022Data[[2]]
colnames(GD_Base_2022Field)
st_crs(GD_Base_2022Field) #EPSG:3857
GD_Base_2022Field<-GD_Base_2022Field %>%
  st_transform(crs=Prov_crs) %>%
  dplyr::select(WTLND_ID,YearSampled=YearSample, Sampled,SampleType) %>%
  dplyr::filter(Sampled>0)
write_sf(GD_Base_2022Field, file.path(dataOutDir,"GD_Base_2022Field.gpkg"))
table(GD_Base_2022Field$YearSampled)

Sub_Boreal_2022Field<-Field2022Data[[3]]
colnames(Sub_Boreal_2022Field)
st_crs(Sub_Boreal_2022Field) #EPSG:3857
Sub_Boreal_2022Field<-Sub_Boreal_2022Field %>%
  st_transform(crs=Prov_crs) %>%
  dplyr::select(WTLND_ID=WTLND_I,Sampled, YearSampled=YrSmpld, SampleType=SmplTyp) %>%
  dplyr::filter(YearSampled>0)
write_sf(Sub_Boreal_2022Field, file.path(dataOutDir,"Sub_Boreal_2022Field.gpkg"))
#table(Sub_Boreal_2022Field$YrSmpld)

SIM_2022Field<-Field2022Data[[4]]
colnames(SIM_2022Field)
st_crs(SIM_2022Field) #EPSG:3857
SIM_2022Field<- SIM_2022Field %>%
  st_transform(crs=Prov_crs) %>%
  dplyr::select(WTLND_ID=WTLND_ID,Sampled, YearSampled=YearSample, SampleType=SampleType) %>%
  dplyr::filter(Sampled>0) #%>%
  #Data had a YearSampled set to 0
  #dplyr::mutate(YearSampled=2022)
write_sf(SIM_2022Field, file.path(dataOutDir,paste0(WetlandAreaDir,"_2022Field.gpkg")))
table(SIM_2022Field$YearSampled)

table(GD_Base_2022Field$YearSampled)
table(Sub_Boreal_2022Field$YearSampled)
