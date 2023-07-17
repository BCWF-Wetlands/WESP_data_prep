<!-- 
Add a project state badge

See <https://github.com/BCDevExchange/Our-Project-Docs/blob/master/discussion/projectstates.md> 
If you have bcgovr installed and you use RStudio, click the 'Insert BCDevex Badge' Addin.
-->

WESP_data_prep
============================
The B.C. Wildlife Federation’s Wetlands Workforce project is a collaboration with conservation organizations and First Nations working to maintain and monitor wetlands across British Columbia.   
https://bcwf.bc.ca/initiatives/wetlands-workforce/.  

WESP - Wetland Ecosystem Services Protocol   

There are three sets of WESP R scripts to identify wetlands for monitoring within a study area.  
1) WESP_data_prep - This repository, presents a set of scripts used to generate a new, or process existing, wetlands for a study area;  
2) WESP_Sample_Design - attributes wetlands with local human and natural landscape characteristics - https://github.com/BCWF-Wetlands/WESP_Sample_Design; and    
3) WESP_Sample_Draw - Generates a report card of how samples are meeting sampling criteria and performs a draw to select wetlands to meet criteria - https://github.com/BCWF-Wetlands/WESP_Sample_Draw.


### Usage

There are a set of scripts that help prepare data for WESP sampling, there are four basic sets of scripts:    
Control scripts - set up the analysis environment;   
Load scripts - loads base data and field data;    
Clean scripts - cleans generic wetlands and ecoprovince/study area specific wetlands; and    
Specialty scripts - for ecoprovince wetlands that require additional processing.    

#Control Scripts:   
run_all.R	Sets local variables and directories used by scripts, presents script order.  
header.R	loads R packages, sets global directories, and attributes.

#Load Scripts:	
01_base_load.R	Loads core spatial layers used by routines.  
01_load.R	load script sourcing all the various pre-processed layers required - typically only required for first run.  
01_load_2022Field.R	Reads and prepares 2022 field data - shapefiles of wetlands sampled.  
01_load_MkWetlands.R	Loads layers needed to generate new base wetlands.  
01_load_pre_made_wetlands.R	Loads pre 2023 wetlands.    

#Clean Scripts:   
02_clean_BCWF_centroids.R	Cleans pre 2022 BCWF sampled wetland centroids, used to idenitfy wetlands sampled.   
02_clean_Field_SIM.R	Cleans 2022 Southern Interior Mountain field data.  
02_clean_Field_TBP.R	Cleans 2022 Taiga Boreal Plains field data.  
02_clean_Field_GD.R	Cleans 2022 Georgia Depression field data.  
02_clean_Field_GD_Est.R	Cleans 2022 Georgia Depression Estuary field data.  
02_clean_geometry.R	Cleans pre 2023 base wetland geometry where required.  
02_clean_MkWetlands.R	Takes loaded make wetland layers and constructs base wetlands.      
02_clean_pre_design_wetlands.R	Prepares wetlands for sample design routines.  
02_clean_pre_made_wetlands.R	Cleans pre-made wetlands and standradizes their content and structure.  
	
#Speciality Scripts:   
#Sub-Boreal:      
Sub-boreal had new wetlands generated in 2023, cross-references ESI and BCWF PEM wetlands:   
02_clean_SB_WTLND_ID.R	adds an atrtribute identifiying the pre-2023 WTLND_IDs for cross-referencing.  
02_clean_Sub-Boreal.R	Calls ESI and PEM routines and integrates.  
02_clean_SubB_ESI.R	Identifies wetlands sampled by ESI in the Sub-Boreal and cross-references base wetlands.   
02_clean_SubB_PEM.R	identifies PEM wetlands  (from Fish and Wildlife Compensation Program (FWCP)) and if a base wetland is also PEM, generates a separate set of PEM wetlands when they are not in the base wetland set.  
#Southern Interior:   
02_clean_field_SI.R'	for SI since requires specific clean up - including adding OK PEM polygons.  

### Project Status

The set of R WESP scripts are continually being modified and improved, including adding new study areas as sampling is initiated.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/BCWF-Wetlands/WESP_data_prep/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2022 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
