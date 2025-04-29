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

##Estuary Section
#Pacific Birds Estuary
#https://pacificbirds.org/2021/02/an-updated-ranking-of-british-columbias-estuaries/
PECP_Estuary<-read_sf(file.path(DataDir,'EstuaryData/PECP_Estuary_Shapefiles_PUBLIC/PECP_estuary_polys_ranked_2019_PUBLIC.shp'))
st_crs(PECP_Estuary) <- 3005
write_sf(PECP_Estuary, file.path(spatialOutDirP,"PECP_Estuary.gpkg"))

#Provincial Shorelines SHZN_SHORE_UNIT_CLASS_POLYS_SV
Shoreline<-read_sf(file.path(DataDir,'PROVdata/Shoreline/SHZN_SHORE_UNIT_CLASS_POLYS_SV/SU_CL_PY_S_polygon.shp'))
st_crs(Shoreline) <- 3005
write_sf(Shoreline, file.path(spatialOutDirP,"Shoreline.gpkg"))












