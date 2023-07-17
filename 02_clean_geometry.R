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

#Read in data for fixing
WetlandsAll<-readRDS(file='tmp/WetlandsAll')

#First pull out the valid geometry to speed up repairs, double check it with
#clgeo_IsValid and save
WetlandsIn_Valid<-WetlandsIn %>%
  dplyr::filter(st_is_valid(WetlandsIn)==TRUE)
clgeo_IsValid(as(WetlandsIn_Valid,'Spatial'), verbose = FALSE) #TRUE
write_sf(WetlandsIn_Valid, file.path(spatialOutDir,"WetlandsIn_Valid.gpkg"))

#Geometry Repair
# Pull out dirty geometry to see what needs to be fixed
WetlandsIn_Invalid<-WetlandsIn %>%
  dplyr::filter(st_is_valid(WetlandsIn)==FALSE)
#check attributes
Wetlandsa_Invalid_no_geo <- Wetlandsa_Invalid %>%
  st_drop_geometry()
#Repair geometry, check with clgeo_IsValid and save
Wetlandsa_Invalid_fix <- Wetlandsa_Invalid %>%
  st_make_valid()
clgeo_IsValid(as(Wetlandsa_Invalid_fix,'Spatial'), verbose = FALSE) #TRUE
write_sf(Wetlandsa_Invalid_fix, file.path(spatialOutDir,"Wetlandsa_Invalid_fix.gpkg"))

#combine fixed geometry and test for validitiy one last time
Wetlandsa<- rbind(Wetlandsa_Valid,Wetlandsa_Invalid_fix) %>%
  mutate(CalibSite=0) %>%
  dplyr::select(WTLND_ID, Sampled, YearSampled,area_Ha,
                dist_to_road, Rd_Insct,
                stream_intersect,river_intersect,mmwb_intersect,lake_intersect,
                split_by_stream,stream_start,stream_end,max_stream_order, granitic_bedrock,
                BEC_BCWF=BEC,
                Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                Land_Cover, Land_Disturbance,
                fire_history, fire_year,
                parcelmap_private,
                DUC_Class, Wet_Numb,OF18,CalibSite,X50m_Buff)
clgeo_IsValid(as(Wetlandsa,'Spatial'), verbose = FALSE) #TRUE

#Pull out Boreal Plains Eco Province
Wetlandsb<-EcoP_L[[WetlandArea[2]]] %>%
  dplyr::select(WTLND_ID, Sampled, YearSampled, area_Ha,
              dist_to_road, Rd_Insct,
              stream_intersect,river_intersect,mmwb_intersect,lake_intersect,
              split_by_stream,stream_start,stream_end,max_stream_order, granitic_bedrock,
              BEC_BCWF=BEC,
              Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
              Land_Cover, Land_Disturbance,
              fire_history, fire_year,
              parcelmap_private,
              DUC_Class, Wet_Numb,OF18,CalibSite,X50m_Buff)
clgeo_IsValid(as(Wetlandsb,'Spatial'), verbose = FALSE) #TRUE

#Make Final wetlands data set after repairs or skip to next step
WetlandsAll<- rbind(Wetlandsa,Wetlandsb) %>%
  mutate(area_Ha=as.numeric(area_Ha))
clgeo_IsValid(as(WetlandsAll,'Spatial'), verbose = FALSE) #TRUE

saveRDS(WetlandsAll, file='tmp/WetlandsAll')
