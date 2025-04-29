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

#Provincial Human Disturbance Layers - compiled for CE
#Needs refinement to differentiate rural/urban and old vs young cutblocks, rangeland, etc.

Wets.i<-st_read(file.path(spatialOutDir,"WetlandsESI.1.gpkg")) %>%
  mutate(wet_id=seq.int(nrow(.)))

#FWCP - PEM wetlands
# Ends with 2 data sets
# 1. WetlandsAll_wPEM - base wetlands w attribure identifying if it is also a PEM wetland
# 2. PEM_wetlands - wetlands that are not base wetlands but are PEM

#Clean and add PEM data
#Terra read in
  FWCP_in1<-rast(file.path(DataDir,"WESPdata/FWCP_Wetlands/WetlandPEM/3CategoryPrediction/20190926-103025_map_recl.tif"))
  ProvRast25<-rast(file.path(spatialOutDirP,'ProvRast25.tif'))

#Terra crop and mask
  FWCP_in<-FWCP_in1 %>%
    terra::resample(ProvRast25) %>%
    terra::crop(AOI) %>%
    terra::mask(vect(AOI))

#Reclassify PEM to pull out wetlands
  m<-c(0,1,NA,1,2,1,2,3,NA)
  m<-matrix(m,ncol=3,byrow=TRUE)
    FWCP.r<-classify(FWCP_in,m,right=TRUE)
writeRaster(FWCP.r, file.path(spatialOutDir,"FWCP.r.tif"), overwrite=TRUE)
FWCP.r<-rast(file.path(spatialOutDir,"FWCP.r.tif"))

#Identify unique wetlands in raster, consider any cells that touch to be part of the same wetland
FWCP_Patch8<-terra::patches(FWCP.r, directions=8)
writeRaster(FWCP_Patch8, file.path(spatialOutDir,"FWCP_Patch8.tif"), overwrite=TRUE)

#Only wetlands that touch on a face
#FWCP_Patch4<-terra::patches(FWCP.r, directions=4)
#writeRaster(FWCP_Patch4, file.path(spatialOutDir,"FWCP_Patch4.tif"), overwrite=TRUE)

#Identify raster where disturbance >0, likely openings that will be deleted
# cases where wetlands adjacent to disturbance patches that are being lumped
# in with adjacent disturbance patch
LandDisturb.t<-rast(file.path(spatialOutDirP,'LandDisturbP.tif')) %>%
  terra::project("EPSG:3005") %>%
  terra::disagg(fact=4, method="near") %>%
  terra::crop(AOI) %>%
  terra::mask(vect(AOI))
writeRaster(LandDisturb.t, file.path(spatialOutDir,"LandDisturb.t.tif"), overwrite=TRUE)

#Max in FWCP_Patch4 - 426873 - for assigning new patch ids
# Make a binary disturbance layer - where the 500000 will be added to the patch id
# thereby identifying  those units that have been disturbed as separate patches
m<-c(0,1,NA,2,11,500000)
m<-matrix(m,ncol=3,byrow=TRUE)
LandDisturb.tr<-classify(LandDisturb.t,m,right=TRUE)
writeRaster(LandDisturb.tr, file.path(spatialOutDir,"LandDisturb.tr.tif"), overwrite=TRUE)

#FWCP_Patch4.d<-FWCP_Patch4+LandDisturb.tr
#writeRaster(FWCP_Patch4.d, file.path(spatialOutDir,"FWCP_Patch4.d.tif"), overwrite=TRUE)

FWCP_Patch8.d<-FWCP_Patch8 + LandDisturb.tr
writeRaster(FWCP_Patch8.d, file.path(spatialOutDir,"FWCP_Patch8.d.tif"), overwrite=TRUE)

  ##### cellSize cant allocate enough memory so crashes
  #FWCP_Zonal<-terra::zonal(cellSize(FWCP_Patch, unit="ha"), FWCP_Patch, sum, as.raster=TRUE)
  #FWCP_large<-ifel(FWCP_Zonal<0.26, NA, FWCP_Patch)
  #writeRaster(FWCP_large, file.path(spatialOutDir,"FWCP_large.tif"), overwrite=TRUE)

#convert to polygon
# creates multipolygons, such that same geometry over 2 polygons
#FWCP.tp8dF<-as.polygons(FWCP_Patch8.d, dissolve=FALSE, values=TRUE, na.rm=TRUE)
#writeVector(FWCP.tp8d, file.path(spatialOutDir,"FWCP.tp8d"), overwrite=TRUE)

#Stars vectorization
FWCP_Patch8.ds <- read_stars(file.path(spatialOutDir,'FWCP_Patch8.d.tif'))
FWCP.tp8d.stars = st_as_sf(FWCP_Patch8.ds, as_points = FALSE, merge = TRUE)
write_sf(FWCP.tp8d.stars, file.path(spatialOutDir,"FWCP.tp8d.stars.gpkg"))

  FWCP.p8 <-FWCP.tp8d.stars %>%
    mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
    #Remove single cell polygons
    dplyr::filter(area_Ha>0.0625) %>%
    #mutate(wet_id_FWCP=seq.int(nrow(.))) %>%
    mutate(wet_id_FWCP=FWCP_Patch8.d.tif)

  write_sf(FWCP.p8, file.path(spatialOutDir,"FWCP.p8.gpkg"))
  #write_sf(FWCP.p4, file.path(spatialOutDir,"FWCP.p4.gpkg"))

#Clean up PEM polygon - select 4 or 8 vectors and polys
# first join with existing inventory and identify those that are not in the inventory,
# of those remove ones that are clearly not wetlands - e.g. cutblocks and other openings
#Combine PEM Wetlands with inventory Wetlands
#Keep the only PEM wetlands seperate for now
  FWCP_large<-rast(file.path(spatialOutDir,"FWCP_Patch8.d.tif"))

  Extract_combined <- dplyr::bind_rows(exact_extract(FWCP_large, Wets.i), .id = "wet_id_ext") %>%
    as_tibble()

  ExtractFull<-Extract_combined %>%
    mutate(wet_id_FWCP=value) %>%
    mutate(wet_id=as.numeric(wet_id_ext)) %>%
    dplyr::group_by(wet_id, wet_id_FWCP) %>%
    dplyr::summarise(overlap_Ha = round(sum(coverage_fraction))*0.0625) %>%
    right_join(Wets.i) %>%
    mutate(pcOverlap=round(overlap_Ha/area_Ha*100)) %>%
    dplyr::select(wet_id,wet_id_FWCP,overlap_Ha,area_Ha, pcOverlap)

  Extract_wet <- ExtractFull %>% group_by(wet_id) %>%
    dplyr::summarize(PEM_wets = paste(sort(unique(wet_id_FWCP)),collapse=", "), nPEMs=n()-1, pcOverlap=sum(pcOverlap)) %>%
    dplyr::filter(nPEMs>0)

  PEMs_in_wet<-as.numeric(unlist(list(strsplit(Extract_wet$PEM_wets, ","))))

# pull out those PEM wetlands not in current inventory
  FWCP.p8<-st_read(file.path(spatialOutDir,"FWCP.p8.gpkg"))

  FWCP_alone8<-FWCP.p8 %>%
    dplyr::filter(!wet_id_FWCP %in% PEMs_in_wet) %>%
    #remove small polys - 2 cells - seems to be noise for the most part
    mutate(area_Ha2=as.numeric(st_area(.)*0.0001)) %>%
    dplyr::filter(area_Ha>0.125) %>%
    #renumber polys for extact below
    mutate(wet_id_FWCP=seq.int(nrow(.)))

  write_sf(FWCP_alone8, file.path(spatialOutDir,"FWCP_alone8.gpkg"))

#Evaluate against disturbance and land cover to see what features PEM wetlands
# are occuring in
  LandCover<-raster(file.path(spatialOutDirP,'LandCoverPS.tif'))
  LandCoverType_LUT<-readRDS(file='tmp/LandCoverType_LUT')

  Wetlands_E <- data.frame(LandCCode=exact_extract(LandCover, FWCP_alone8, 'mode'))
  Wetlands_E$wet_id_FWCP <-seq.int(nrow(Wetlands_E))

  Wetlands_LC_PEM8 <- FWCP_alone8 %>%
    # mutate(wet_id=as.numeric(rownames(wetlandsB))) %>%
    left_join(Wetlands_E) %>%
    left_join(LandCoverType_LUT)

  write_sf(Wetlands_LC_PEM8, file.path(spatialOutDir,"Wetlands_LC_PEM8.gpkg"))

  #LandDisturbance
  LandDisturb<-raster(file.path(spatialOutDirP,'LandDisturbP.tif'))
  Disturb_LUT<-readRDS(file='tmp/Disturb_LUT')

  Wetlands_DE <- data.frame(DisturbCode=exact_extract(LandDisturb, FWCP_alone8, 'mode'))
  Wetlands_DE$wet_id_FWCP <-seq.int(nrow(Wetlands_DE))

  Wetlands_PEM18 <- Wetlands_LC_PEM8 %>%
    # mutate(wet_id=as.numeric(rownames(wetlandsB))) %>%
    left_join(Wetlands_DE) %>%
    left_join(Disturb_LUT)

  write_sf(Wetlands_PEM18, file.path(spatialOutDir,"Wetlands_PEM18.gpkg"))
  Wetlands_PEM18<-st_read(file.path(spatialOutDir,"Wetlands_PEM18.gpkg"))

#Drop PEM wetlands that are cutblocks, urban, mine sites, cropland and snow-ice
  table(Wetlands_PEM18$DisturbType)
  table(Wetlands_PEM18$LandCoverType)

#find wetlands to filter out
  Wetlands_PEM_odd8 <- Wetlands_PEM18 %>%
    #disturbance-historic, current cut blocks, urban, mine sites
    #landcover - urban, cropland, snow ice
    dplyr::filter(DisturbCode %in% c(3,7,12,13) |
                    LandCCode %in% c(1,7,8))
  write_sf(Wetlands_PEM_odd8, file.path(spatialOutDir,"Wetlands_PEM_odd8.gpkg"))
  Wetlands_PEM_odd8<-st_read(file.path(spatialOutDir,"Wetlands_PEM_odd8.gpkg"))

#now filter out the PEM wetlands that are not wetlands based on disturbance and land cover type
  PEM_wetlands1<-Wetlands_PEM18 %>%
    dplyr::filter(!wet_id_FWCP %in% Wetlands_PEM_odd8$wet_id_FWCP) %>%
    mutate(PEMwetland = 1) %>%
    mutate(wet_id_FWCP=seq.int(nrow(.)))

  #Generate centroid version of data
  wetlandsXY <- st_centroid(PEM_wetlands1)
  wetpt <- st_coordinates(wetlandsXY)
  wetpt <- wetlandsXY %>%
    cbind(wetpt) %>%
    st_drop_geometry()

  PEM_wetland.pt <- st_as_sf(wetpt, coords= c("X","Y"), crs = 3005)

  PEM_wetland.pt <- PEM_wetland.pt %>%
    mutate(wet_id_FWCP=as.numeric(rownames(PEM_wetland.pt)))
  st_crs(wetland.pt)<-3005

#clip points to AOI to make sure wetland are in AOI
  PEM_wetland.pt<-PEM_wetland.pt %>%
    st_intersection(AOI)

#identify large wetlands
  Large_Wetlands<- PEM_wetlands1 %>%
    dplyr::filter(area_Ha>=1000)

#Make Wetlands all the rest
  PEM_wetlands2 <- PEM_wetlands1 %>%
    mutate(LargeWetland=if_else(WTLND_ID %in% Large_Wetlands$WTLND_ID, ">=1000", "<1000"))

#Select PEM wetlands in AOI
PEM_wetlands<-PEM_wetlands2 %>%
    dplyr::filter((wet_id_FWCP %in% PEM_wetland.pt$wet_id_FWCP)) %>%
    mutate(wet_id_FWCP=seq.int(nrow(.))) %>%
    mutate(WTLND_ID=paste0("FWCP_",wet_id_FWCP))

#Write out data to PEM only directory
  write_sf(PEM_wetland.pt, file.path(PEMspatialOutDir,"PEM_wetland.pt.gpkg"))
  write_sf(PEM_wetlands, file.path(PEMspatialOutDir,"PEM_wetlands.gpkg"))
  write_sf(PEM_wetlands, file.path(PEMspatialOutDir,"WetlandsAll.gpkg"))

#Base wetlands with attribute identifying if it is also a PEM wetlands
  WetlandsAll_wPEM <- Wets.i %>%
    mutate(PEMwetland = ifelse((wet_id %in% Extract_wet$wet_id),1,0))
write_sf(WetlandsAll_wPEM, file.path(spatialOutDir,"WetlandsAll_wPEM.gpkg"))

#PEM wetlands that are not in Base wetlands
PEM_WetlandsAll <- PEM_wetlands %>%
  mutate(PEMwetland = ifelse(!(wet_id %in% Extract_wet$wet_id),1,0))
write_sf(PEM_WetlandsAll, file.path(spatialOutDir,"PEM_WetlandsAll.gpkg"))

#pull out base wetlands that are also PEM for inspection
Wetlands_wPEM<-WetlandsAll_wPEM %>%
  dplyr::filter(PEMwetland==1)
write_sf(Wetlands_wPEM, file.path(spatialOutDir,"Wetlands_wPEM.gpkg"))

#Ends with 2 data sets
# 1. WetlandsAll_wPEM - base wetlands w attribure identifying if it is also a PEM wetland
# 2. PEM_wetlands - wetlands that are not base wetlands but are PEM

