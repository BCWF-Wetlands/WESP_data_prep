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

#load in layers
VRI_raw<-read_sf(file.path(spatialOutDir,"VRI_raw.gpkg"))
FWA_wetlands<-st_read(file.path(spatialOutDir,"FWA_wetlands.gpkg"))
Erase_water<-st_read(file.path(spatialOutDir,"Erase_water.gpkg"))

#Pull out VRI wetlands - code originally from Jesse Fraser for Skeena Wetlands,
# adopted by Evin at BCWF - translated into R by D Morgan
VRI_wetlands_1a <- VRI_raw %>%
  dplyr::select(VRI_id,FEATURE_ID,NON_PRODUCTIVE_DESCRIPTOR_CD,NON_PRODUCTIVE_CD,BCLCS_LEVEL_1,BCLCS_LEVEL_2,BCLCS_LEVEL_3,BCLCS_LEVEL_4,
                BCLCS_LEVEL_5,SHRUB_HEIGHT,SHRUB_CROWN_CLOSURE,SHRUB_COVER_PATTERN,HERB_COVER_TYPE,HERB_COVER_PATTERN,HERB_COVER_PCT,
                BRYOID_COVER_PCT,NON_VEG_COVER_PATTERN_1,NON_VEG_COVER_PCT_1,NON_VEG_COVER_TYPE_1,NON_VEG_COVER_PATTERN_2,NON_VEG_COVER_PCT_2,
                NON_VEG_COVER_TYPE_2,NON_VEG_COVER_PATTERN_3,NON_VEG_COVER_PCT_3,NON_VEG_COVER_TYPE_3,LAND_COVER_CLASS_CD_1,EST_COVERAGE_PCT_1,
                SOIL_MOISTURE_REGIME_1,LAND_COVER_CLASS_CD_2,EST_COVERAGE_PCT_2,SOIL_MOISTURE_REGIME_2,LAND_COVER_CLASS_CD_3,EST_COVERAGE_PCT_3,
                SOIL_MOISTURE_REGIME_3,INVENTORY_STANDARD_CD,NON_PRODUCTIVE_DESCRIPTOR_CD
                ) %>%
  mutate(wetlandType=
            case_when(BCLCS_LEVEL_1=='V' & BCLCS_LEVEL_2=='N' & BCLCS_LEVEL_3=='W' & BCLCS_LEVEL_4=='BM' & BCLCS_LEVEL_5=='CL' ~ 3,
            BCLCS_LEVEL_1=='V' & BCLCS_LEVEL_2=='N' & BCLCS_LEVEL_3=='W' & BCLCS_LEVEL_4=='BY' & BCLCS_LEVEL_5 %in% c('CL','OP') ~ 3,
            BCLCS_LEVEL_1=='V' & BCLCS_LEVEL_2=='N' & BCLCS_LEVEL_3=='W' & BCLCS_LEVEL_4 %in% c('HE', 'HF', 'HG') & BCLCS_LEVEL_5 %in% c('DE', 'OP', 'SP') ~ 3,
            BCLCS_LEVEL_1=='V' & BCLCS_LEVEL_2=='N' & BCLCS_LEVEL_3=='W' & BCLCS_LEVEL_4 %in% c('SL', 'ST') & BCLCS_LEVEL_5 %in% c('DE', 'OP', 'SP') ~ 4,
            BCLCS_LEVEL_1=='V' & BCLCS_LEVEL_2=='T' & BCLCS_LEVEL_3=='W' & BCLCS_LEVEL_4 %in% c('TB', 'TC') & BCLCS_LEVEL_5 %in% c('DE', 'OP', 'SP') ~ 4,
            BCLCS_LEVEL_1=='V' & BCLCS_LEVEL_2=='T' & BCLCS_LEVEL_3=='W' & BCLCS_LEVEL_4=='TM' & BCLCS_LEVEL_5 %in% c('SP', 'OP') ~ 4,
            BCLCS_LEVEL_1=='N' & BCLCS_LEVEL_2=='L' & BCLCS_LEVEL_3=='W' & BCLCS_LEVEL_4=='EL' & BCLCS_LEVEL_5 %in% c('MU','ES') ~ 9,
            INVENTORY_STANDARD_CD=='F' & NON_PRODUCTIVE_DESCRIPTOR_CD=='S' ~ 5,
            SOIL_MOISTURE_REGIME_1== '6' & (
              NON_VEG_COVER_TYPE_1 %in% c('MU', 'BE', 'LS', 'RM', 'LA', 'RE', 'RS', 'RI') |
              NON_VEG_COVER_TYPE_2 %in% c('MU', 'BE', 'LS', 'RM', 'LA', 'RE', 'RS', 'RI') |
              NON_VEG_COVER_TYPE_3 %in% c('MU', 'BE', 'LS', 'RM', 'LA', 'RE', 'RS', 'RI')) ~ 6,
            #This case is covered above so not used
            #BCLCS_LEVEL_3=='W' ~ 8,
            SOIL_MOISTURE_REGIME_1 %in% c('6','7','8') ~ 7,
            TRUE ~ 0))

#Data Checking
write_sf(VRI_wetlands_1a, file.path(spatialOutDir,"VRI_wetlands_1a.gpkg"))
#VRI_wetlands_1<-st_read(file.path(spatialOutDir,"VRI_wetlands_1.gpkg"))

table(VRI_wetlands_1$wetlandType)
tt<-VRI_wetlands_1 %>%
  filter(wetlandType==6 & NON_VEG_COVER_TYPE_1 =='RI')
table(tt$BCLCS_LEVEL_5)

#Filter out wetlands from VRI and check
VRI_wetlands_2 <- VRI_wetlands_1 %>%
  dplyr::filter(wetlandType>0) %>%
  mutate(wetland=1) %>%
  dplyr::select(wetland,wetlandType)
#write_sf(VRI_wetlands_2, file.path(spatialOutDir,"VRI_wetlands_2.gpkg"))
table(VRI_wetlands_2$wetlandType)

#Erase water (lakes and rivers, not streams) from wetlands
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
#st_erase(x,y) will erase y from x.

VRI_wetlands_3 <- VRI_wetlands_2 %>%
  st_erase(Erase_water) %>%
  st_cast("POLYGON")
#  rmapshaper::ms_erase(Erase_water, remove_slivers=TRUE) %>%
VRI_wetlands_3<-VRI_wetlands_3 %>%
  st_cast("POLYGON")
st_geometry(VRI_wetlands_3) <- "geom"

#Breaks geometry so need to fix
Wetlands_Invalid<-VRI_wetlands_3 %>%
  dplyr::filter(st_is_valid(VRI_wetlands_3)==FALSE)
Wetlands_Invalid_fix <- Wetlands_Invalid %>%
  st_make_valid()
clgeo_IsValid(as(Wetlands_Invalid_fix,'Spatial'), verbose = FALSE) #TRUE

Wetlands_Valid<-VRI_wetlands_3 %>%
  dplyr::filter(st_is_valid(VRI_wetlands_3)==TRUE)
clgeo_IsValid(as(Wetlands_Valid,'Spatial'), verbose = FALSE) #TRUE

VRI_wetlands<- rbind(Wetlands_Valid,Wetlands_Invalid_fix)
clgeo_IsValid(as(VRI_wetlands,'Spatial'), verbose = TRUE)
write_sf(VRI_wetlands, file.path(spatialOutDir,"VRI_wetlands.gpkg"))
#VRI_wetlands_4<-st_read(file.path(spatialOutDir,"VRI_wetlands_4.gpkg"))
types <- st_geometry_type(VRI_wetlands)
types_df.4 <- data.frame(types) #POLYGON

#load FWA wetlands
FWA_wetlandsIn<-FWA_wetlands %>%
  mutate(wetland=1) %>%
  mutate(wetlandType=10) %>%
  dplyr::select(wetland,wetlandType)

cast_all <- function(xg) {
    lapply(c("MULTIPOLYGON", "MULTILINESTRING", "MULTIPOINT", "POLYGON", "LINESTRING", "POINT"),
           function(x) st_cast(xg, x))
  }
FWA_wetlandsIn.1<-st_sfc(cast_all(FWA_wetlandsIn))

types <- st_geometry_type(FWA_wetlandsIn)
types_df.FWA <- data.frame(types) #POLYGON

#Combine VRI and FWI wetlands
VRI_FWA_wetlands_1 <- rbind(VRI_wetlands,FWA_wetlandsIn)

types <- st_geometry_type(VRI_FWA_wetlands_1)
types_df <- data.frame(types)
clgeo_IsValid(as(VRI_FWA_wetlands_1,'Spatial'), verbose = TRUE)

write_sf(VRI_FWA_wetlands_1, file.path(spatialOutDir,"VRI_FWA_wetlands_1.gpkg"))
#VRI_FWA_wetlands_1<-st_read(file.path(spatialOutDir,"VRI_FWA_wetlands_1.gpkg"))

#aggregate overlapping wetlands into single polygon
VRI_FWA_wetlands_2<- VRI_FWA_wetlands_1 %>%
  dplyr::group_by(wetland) %>%
  #dplyr::summarize(geometry = st_combine(geom)) %>%
  dplyr::summarize(n=n()) %>%
  st_cast('POLYGON') %>%
  dplyr::ungroup() %>%
  st_buffer(dist=0)

VRI_FWA_wetlands_3<-st_make_valid(VRI_FWA_wetlands_2)
clgeo_IsValid(as(VRI_FWA_wetlands_3,'Spatial'), verbose = TRUE)

#Remove slivers
#check for empty geometry
Wet_Check_1<-VRI_FWA_wetlands_3 %>%
  dplyr::filter(st_is_empty(.))
nrow(Wet_Check_1)
#check that there are no duplicate geometries
Wet_Check_2<-VRI_FWA_wetlands_3 %>%
  dplyr::distinct(.)
nrow(Wet_Check_2)
#Remove small 'sliver' polygons less than 1 ha
#Smallest FWA wetland
FWA_wetlandsCheck<-FWA_wetlands %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001))
min(FWA_wetlandsCheck$area_Ha)
#Smallest VRI wetland
VRI_wetlandsCheck<-VRI_wetlands_3 %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001))
min(VRI_wetlandsCheck$area_Ha)
#Smallest complex wetland
wetlandsCheck<-VRI_FWA_wetlands_3 %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001))
min(wetlandsCheck$area_Ha)

#remove polygons less than 100m2 (0.01ha) tenth of a hectare
VRI_FWA_wetlands_4 <- VRI_FWA_wetlands_3 %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  dplyr::filter(area_Ha>0.01)

#Remove all wetlands not within the EcoProvince
#Intersect (clip) the wetlands with the EP boundary
#Then remove wetlands that dont have their entire area within the EP
VRI_FWA_wetlands_5<-VRI_FWA_wetlands_4 %>%
 st_intersection(AOI_EP) %>%
 mutate(area_HaIntersect=as.numeric(st_area(.)*0.0001)) %>%
  #round the areas to deal with minor area differences
 dplyr::filter(round(area_Ha,digits=4)==round(area_HaIntersect,digits=4))

mapview(AOI_EP)+mapview(VRI_FWA_wetlands_5)
#write_sf(VRI_FWA_wetlands_5, file.path(spatialOutDir,"VRI_FWA_wetlands_5.gpkg"))

#Clean up and save
SampleCols <- c(Sampled = NA, YearSampled = NA, SampleType = NA)

WetlandsAll<-VRI_FWA_wetlands_5 %>%
  #st_filter(AOI_EP, .predicates = st_contains) %>%
  mutate(wet_id = row_number()) %>%
  mutate(WTLND_ID=paste0(WetlandAreaShort,'_',wet_id)) %>%
  add_column(!!!SampleCols[!names(SampleCols) %in% names(.)]) %>%
  dplyr::select(-c(area_HaIntersect,AOI)) %>%
  replace(is.na(.), 0)

write_sf(WetlandsAll, file.path(spatialOutDir,"WetlandsAll.gpkg"))
