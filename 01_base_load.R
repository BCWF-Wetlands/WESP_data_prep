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

#Rasterize the Province for subsequent masking
# bring in BC boundary
bc <- bcmaps::bc_bound()
Prov_crs<-crs(bc)
#Prov_crs<-"+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

BCr_file <- file.path(spatialOutDirP,"BCr.tif")
if (!file.exists(BCr_file)) {
  BC<-bcmaps::bc_bound_hres(class='sf')
  saveRDS(BC,file='tmp/BC')
  ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5,
                   ymn=173787.5, ymx=1748187.5,
                   crs=Prov_crs,
                   res = c(100,100), vals = 1)
  ProvRast25<-raster(nrows=62976, ncols=68864, xmn=159587.5, xmx=1881187.5,
                     ymn=173787.5, ymx=1748187.5,
                     crs=Prov_crs,
                     res = c(25,25), vals = 1)
  ProvRast_S<-st_as_stars(ProvRast)
  write_stars(ProvRast_S,dsn=file.path(spatialOutDirP,'ProvRast_S.tif'))
  BCr <- fasterize(BC,ProvRast)
  BCr25 <- fasterize(BC,ProvRast25)
  #Linear rasterization of roads works better using the stars package
  BCr_S <-st_as_stars(BCr)
  write_stars(BCr_S,dsn=file.path(spatialOutDirP,'BCr_S.tif'))
  writeRaster(BCr, filename=BCr_file, format="GTiff", overwrite=TRUE)
  writeRaster(BCr25, filename=file.path(spatialOutDirP,"BCr25.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(ProvRast, filename=file.path(spatialOutDirP,'ProvRast'), format="GTiff", overwrite=TRUE)
  writeRaster(ProvRast25, filename=file.path(spatialOutDirP,'ProvRast25'), format="GTiff", overwrite=TRUE)
} else {
  BCr <- raster(BCr_file)
  BCr25 <- raster(file.path(spatialOutDirP,"BCr25.tif"))
  ProvRast<-raster(file.path(spatialOutDirP,'ProvRast.tif'))
  ProvRast25<-raster(file.path(spatialOutDirP,'ProvRast25.tif'))
  BCr_S <- read_stars(file.path(spatialOutDirP,'BCr_S.tif'))
  BC <-readRDS('tmp/BC')
}

crs(ProvRast)<-crs(bcmaps::bc_bound())
saveRDS(ProvRast,file='tmp/ProvRast')

ws <- get_layer("wsc_drainages", class = "sf") #%>%
