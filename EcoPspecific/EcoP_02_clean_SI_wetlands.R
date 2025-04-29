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

#Read in supplied OK data
OK_gdb<-file.path(SpatialDir,'SI_wetlands/Okanagan_Wetlands.gdb')
OK_list <- st_layers(OK_gdb)
OK_in <- read_sf(OK_gdb, layer = "Okanagan_Wetlands")

#Use QGIS to reproject OK_in - st_transform was losing polygons and to clean
# topology, use Vector/Geometry/CheckValidity then Processing/Toolbox/Fix geometries
# then export to gpkg
write_sf(OK_in, file.path(spatialOutDir,"OK_in.gpkg"))
OK_in.1<-st_read(file.path(spatialOutDir,"OK_in_reproj_fix.gpkg"))
#OK_in_reproj.1<-st_make_valid(OK_in_reproj)
#Double check geometry validity
clgeo_IsValid(as(OK_in.1,'Spatial'), verbose = FALSE)

#intersect with AOI, select Unique_ID to link back to full data if needed
# and cast as polygon and add a unique id
OK_raw <- st_intersection(AOI, OK_in.1)
st_geometry(OK_raw) <- "geom"

OK_raw.1 <- OK_raw %>%
  dplyr::select(Unique_ID) %>%
  st_cast("POLYGON") %>%
  mutate(OK_id=seq.int(nrow(.)))

OK_raw.data<-OK_raw.1 %>%
  st_drop_geometry()

#Visualize data
mapview(OK_raw.1) + mapview(AOI)
write_sf(OK_raw.1, file.path(spatialOutDir,"OK_raw.1.gpkg"))
#OK_raw.1<-st_read(file.path(spatialOutDir,"OK_raw.1.gpkg"))

Wetlands.1 <- st_read(file.path(spatialOutDir,"WetlandsAll_in.gpkg")) %>%
  #When re-read wet_id can. change
  mutate(wet_id=seq.int(nrow(.)))
 write_sf(Wetlands.1, file.path(spatialOutDir,"Wetlands.1.gpkg"))

#Intersection of wetlands
OK_intersect2 <-OK_raw.1  %>%
  st_intersects(Wetlands.1) %>%
  as.data.frame()
OK_intersect <-OK_raw.1 %>%
  st_intersects(Wetlands.1) %>%
  as.data.frame() %>%
  dplyr::rename(wet_id=col.id) %>%
  dplyr::rename(OK_id=row.id)
#Make new layer combining base wetlands and OK wetlands that intersect
#populate with OK_id so can reference back to full OK data
#First pull out the OK wetlands that overlap the base wetlands
OK_Overlap<-OK_raw.1 %>%
  dplyr::select(OK_id) %>%
  dplyr::filter((OK_id %in% OK_intersect$OK_id))

#Second pull out the base wetlands that overlap OK wetlands
Base_Overlap <- Wetlands.1 %>%
  dplyr::select(wet_id) %>%
  dplyr::filter((wet_id %in% OK_intersect$wet_id))
write_sf(Base_Overlap, file.path(spatialOutDir,"Base_Overlap1.gpkg"))

#Check geometry
Wetlands_Invalid<-OK_Overlap %>%
  dplyr::filter(st_is_valid(OK_Overlap)==FALSE)
Wetlands_Invalid<-Base_Overlap %>%
  dplyr::filter(st_is_valid(Base_Overlap)==FALSE)
#ALso clgeo
clgeo_IsValid(as(Base_Overlap,'Spatial'), verbose = FALSE)# FALSE
clgeo_IsValid(as(OK_Overlap,'Spatial'), verbose = FALSE)# FALSE
#OK_Overlap.clean <- clgeo_Clean(as(OK_Overlap,'Spatial'))
#clgeo_IsValid(OK_Overlap.clean, verbose = FALSE)
#OK_Overlap<-st_as_sf(sp.clean)

#3) Union the base and OK wetlands that overlap
#union keeps crashing in R despite geos/st cleaning
#OK_base<-OK_Overlap %>%
#  st_union(Base_Overlap)
#export Base and OK_Overlap then use GRASS to clean, union and dissolve
# uses GRASS function v.clean,
# then add a 'wetland' w v.db.addcolumn, then set to 1 w v.db.update
# dissolve v.dissolve on both layers on 'wetland'
# v.overlay ('or') to union
# v.dissolve, after adding 'wetland' column to dissolve on.
write_sf(Base_Overlap, file.path(spatialOutDir,"Base_Overlap.gpkg"))
write_sf(OK_Overlap, file.path(spatialOutDir,"OK_Overlap.gpkg"))
#Read back in from GRASS and set wetland type to 11 - ie PEM overlaps with Base wetlands
OK_Base.1<-st_read(file.path(spatialOutDir,"Base_OK_C_D_U_D.gpkg")) %>%
  mutate(wet_id=99999) %>%
  mutate(wetlandType=11) %>%
  mutate(OK_Base_id = row_number())

#Check wetlands spatial integrity, clean and save
clgeo_IsValid(as(OK_Base.1,'Spatial'), verbose = FALSE)# FALSE
OK_Base.1.clean <- clgeo_Clean(as(OK_Base.1,'Spatial'))
clgeo_IsValid(OK_Base.1.clean, verbose = FALSE)
OK_Base.1<-st_as_sf(OK_Base.1.clean)
write_sf(OK_Base.1, file.path(spatialOutDir,"OK_Base.1.gpkg"))

#Use st_intersects to pull in the OK id to the unioned OK-Base wetlands
OK_Base.id <- OK_Base.1 %>%
  st_intersects(OK_Overlap) %>%
  as.data.frame() %>%
  dplyr::rename(OK_Base_id=row.id) %>%
  dplyr::rename(OK_id=col.id) %>%
  group_by(OK_Base_id) %>%
  #Take first OK id - since some have multiple ids because they were unioned and dissolved
  dplyr::summarize(OK_id=first(OK_id), n=n())

#Join to OK_Base.id to pull in OK_id, OK_Base_id, wetlandType, dummy wet_id
OK_Base<-OK_Base.1 %>%
left_join(OK_Base.id,by=c('OK_Base_id')) %>%
  dplyr::select(wet_id,OK_id, wetlandType)
#not sure about these steps....
  #dplyr::filter((OK_id %in% OK_intersect$OK_id)) %>% #This would pull the 'first' OK_id in the list from the intersect above
#Standardize the geometry column name
st_geometry(OK_Base) <- "geom"

#Check the data in QGIS
write_sf(OK_Base, file.path(spatialOutDir,"OK_Base.gpkg"))
#create data only for manual data checking
OK_Base.data<-OK_Base %>%
  st_drop_geometry()

#Not sure what I was thinking here....maybe to
# identify wetlands that are unique given that OK_id has been reduced
# or ...
#OK_Base.2<-OK_Base %>%
# dplyr::filter((OK_id %in% OK_intersect$OK_id))
#  dplyr::select(wet_id,OK_id, wetlandType)
#write_sf(OK_Base.2, file.path(spatialOutDir,"OK_Base.2.gpkg"))

#drop the base wetlands that intersect from the original base wetlands
Wetlands.1.drop<-Wetlands.1 %>%
  dplyr::filter(!wet_id %in% Base_Overlap$wet_id) %>%
  mutate(OK_id=99999) %>%
  mutate(wetlandType=1) %>%
  dplyr::select(wet_id,OK_id, wetlandType)
write_sf(Wetlands.1.drop, file.path(spatialOutDir,"Wetlands.1.drop.gpkg"))

#add the overlapping wetlands in
Wetlands.2<-rbind(Wetlands.1.drop, OK_Base)
write_sf(Wetlands.2, file.path(spatialOutDir,"Wetlands.2.gpkg"))

#add the alone OK wetlands in and generate an new id
OK_intersect.1 <-OK_intersect %>%
  group_by(OK_id) %>%
  dplyr::summarize(n=n())

OK_alone<-OK_raw.1 %>%
  dplyr::filter(!OK_id %in% OK_intersect.1$OK_id) %>%
  mutate(wet_id=99999) %>%
  mutate(wetlandType=12) %>%
  dplyr::select(wet_id,OK_id,wetlandType)
  write_sf(OK_alone, file.path(spatialOutDir,"OK_alone.gpkg"))

#add the OK wetlands that do not overlap
Wetlands.3<-rbind(Wetlands.2,OK_alone) %>%
  mutate(wet_id2=seq.int(nrow(.)))

write_sf(Wetlands.3, file.path(spatialOutDir,"Wetlands.3.gpkg"))

#Add in a link to original OK data
#Use st_intersects to pull in the OK id to the unioned OK-Base wetlands
Wetlands_OKdat <- Wetlands.3 %>%
  st_intersects(OK_raw.1) %>%
  as.data.frame() %>%
  dplyr::rename(wet3_id=row.id) %>%
  dplyr::rename(OK_raw_id=col.id) %>%
  group_by(wet3_id) %>%
  #Take first OK id - since some have multiple ids because they were unioned
  dplyr::summarize(OK_raw_id=first(OK_raw_id), n=n())

#Join to OK_Base.id to pull in OK_id, OK_Base_id, wetlandType, dummy wet_id
Wetlands.4<-Wetlands.3 %>%
  left_join(Wetlands_OKdat,by=c('wet_id2'='wet3_id')) %>%
  mutate(wet_id=seq.int(nrow(.))) %>%
  #not sure about these steps....
  #dplyr::filter((OK_id %in% OK_intersect$OK_id)) %>% #This would pull the 'first' OK_id in the list from the intersect above
  dplyr::select(wet_id,OK_raw_id, wetlandType,nOKwets=n) %>%
  mutate(PEMwetland=ifelse(wetlandType>10,1,0))

SampleCols <- c(Sampled = NA, YearSampled = NA, SampleType = NA)

WetlandsAll <- Wetlands.4 %>%
  #When re-read wet_id can. change
  #mutate(wet_id = row_number()) %>%
  mutate(wet_id=seq.int(nrow(.))) %>%
  mutate(WTLND_ID=paste0(WetlandAreaShort,'_',wet_id)) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  add_column(!!!SampleCols[!names(SampleCols) %in% names(.)]) %>%
  dplyr::filter(area_Ha>0.01) #drops 308 wetlands




