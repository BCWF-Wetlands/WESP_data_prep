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

#WetlandsAll<-st_read(file.path(spatialOutDirDesign,paste0("SampleStrata_",WetlandAreaShort,".gpkg"))) #%>%

#wetlands can come in 3 types:
#1. Wetlands newly made and have no design attributes
#2. pre-made wetlands with no sampling info
#3. old wetlands with sampling info

WetlandsAllin<-st_read(file.path(spatialOutDir,"WetlandsAll.gpkg")) %>%
  mutate(wet_id=seq.int(nrow(.))) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001))
#if new calculate WTLND_ID as well
  #mutate(WTLND_ID=paste0(WetlandAreaShort,'_',wet_id))
st_crs(WetlandsAllin) <- 3005

WetlandsAllin<-st_make_valid(WetlandsAllin)
clgeo_IsValid(as(WetlandsAllin,'Spatial'), verbose = FALSE)
#sp.clean <- clgeo_Clean(as(WetlandsAllin,'Spatial'))
#clgeo_IsValid(sp.clean, verbose = FALSE)
#WetlandsAllin<-st_as_sf(sp.clean)

#Check for missing hydro values
#hydroCheck<-WetlandsAll.1 %>%
#  dplyr::filter(is.na(stream_intersect))

#Pull out wetlands >1000 ha as a special case - eg Tagia
#Hard to type them with disturbance, land cover or even BEC due to size
#Suggest randomly sampling large wetlands - 15 in the Taiga - sample 5 randomly
Large_Wetlands<- WetlandsAllin %>%
  dplyr::filter(area_Ha>=1000)
#Make Wetlands all the rest
Wetlands1 <- WetlandsAllin %>%
  mutate(LargeWetland=if_else(WTLND_ID %in% Large_Wetlands$WTLND_ID, ">=1000", "<1000"))

source('BufferNpoints.R')

#Write out data
write_sf(Wetlands, file.path(spatialOutDir,"Wetlands.gpkg"))
write_sf(wetland.pt, file.path(spatialOutDir,"wetland.pt.gpkg"))
write_sf(WetlandsB, file.path(spatialOutDir,"WetlandsB.gpkg"))

#Write files for WESP design routines
write_sf(Wetlands, file.path(spatialOutDirDesign,"Wetlands.gpkg"))
write_sf(wetland.pt, file.path(spatialOutDirDesign,"wetland.pt.gpkg"))
write_sf(WetlandsB, file.path(spatialOutDirDesign,"WetlandsB.gpkg"))

table(Wetlands$Sampled, Wetlands$YearSampled) #46 26

##############Data Check
#tt<-Wetlands %>%
#  dplyr::filter(WTLND_ID %in% c('TP_1_1','TP_1_2','TP_1_3','TP_1_4','TP_1_5')


#flowCheck<-WetlandsAll %>%
#  st_drop_geometry() %>%
#  dplyr::filter(WTLND_ID %in% c("GD_28685", "GD_26201", "GD_26246", "GD_27213", "GD_27576", "GD_27905", "GD_27932", "GD_99999"))
  #dplyr::filter(stream_intersect=='unknown')
