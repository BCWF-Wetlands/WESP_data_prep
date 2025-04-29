# Copyright 2020 Province of British Columbia
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

#Generates FREP and CGL and Nation amendments to the SampleStrata file
#SampleType gets set, some cases where 1 (Sampled) gets changed to 2 (FREP) or 3 (CGL), 4 is 'Other'
#Sampled is 66 at the start, bumps to 74 with the 8 Nation specific wetlands that will definitely be sampled

#Read in files for sending to draw
SampleStrata<-st_read(file.path(spatialOutDirDesign,"SampleStrata2022.gpkg")) %>%
  st_drop_geometry()

#Check number sampled
table(SampleStrata$Sampled)

#Some data checking
table(SampleStrata$SampleType)
table(SampleStrata$DisturbType)

SampleFileName<-'SampleStrata_PEM.csv'

#Save file for input to the sample draw program
write.csv(SampleStrata, file=file.path(DrawDir,SampleFileName), row.names = FALSE)
