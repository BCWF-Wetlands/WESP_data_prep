### Wetland Ecosystem Services Protocol (WESP) data prep

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

There are five sets of WESP R scripts to assess wetlands:  
1) WESP_data_prep - This repository, presents a set of scripts used to generate a new, or process existing, wetlands for a study area and the associated spatial data - https://github.com/BCWF-Wetlands/WESP_data_prep;  
2) WESP_Sample_Design - attributes wetlands with  human and natural landscape characteristics - https://github.com/BCWF-Wetlands/WESP_Sample_Design;   
3) WESP_Sample_Draw - Generates a report card of how samples are meeting sampling criteria and performs a draw to select wetlands to fill sampling gaps - https://github.com/BCWF-Wetlands/WESP_Sample_Draw;  
4) WESP_OF - this repository, automates the majority of the office WESP questions. Questions OF6, OF8, OF9, OF10, OF11, OF13, OF14, OF24 must be answered manually - https://github.com/BCWF-Wetlands/WESP_OF; and  
5) WESP_Calculator - reads the office and field questions and runs wespr which calculate the Ecosystem Services for set of reference sites or a single comparison site - https://github.com/BCWF-Wetlands/WESP_Calculator.  


### Usage

There are a set of four basic scripts that help prepare data for WESP sampling and ecosystem service assessment, they include:    
Control scripts - set up the analysis environment;  
Load and Clean scripts - loads and cleans Provincial, Study Area and field data;    
Wetland scripts - generates or updates a study area wetland data set;    
Specialty scripts - task specific scripts for generic functions, cleaning geometry or attributing wetlands.    
  
The first step is to prepare Provincial data. Some layers, for example forest inventory (VRI) 
land disturbance and roads which should be updated annually. Once processed study area specific versions can be generated, 
typically at the EcoProvince scale. Provincial spatial data processing can take significant computational time, once the Provincial 
layers are compiled processing individual study area data sets is significantly faster. Some layers, such as DEM are
processed at the study area scale due to their high resolution. 
  
For a new project area a wetland inventory is generated.  These wetlands are attributed for conducting a sample design.
For existing projects the field scripts are used to identify sampled wetlands which are then integrated into the wetland
inventory layer. In the field, wetlands that were not in the inventory or are large wetlands that were subdivided
for sampling require manual processing. 

Once the sampled wetlands constitute a complete reference set (100 or more wetlands sampled 
across a study area) the next step is to run the automated office scripts and ecosystem service analysis (WESP_OF and WESP_Calculator). 
If there is further wetland sampling required the sample design and sample draw scripts are run in order to 
identify additional wetlands to sample (WESP_Sample_Design and WESP_Sample_Draw).

#Control Scripts:   
Prov_0_Data_run.R, EcoP_0_Data_run.r and Field_0_Data_run.R - Sets local variables and directories used by scripts, 
presents script order.  
header.R loads R packages, sets global directories, and attributes.

#Load Scripts:	
Prov_01_load.R - Loads Provincial spatial layers used by routines, most of these are available from the 
Province's data catalogue (https://catalogue.data.gov.bc.ca). Land cover is the exception and can be sources from the BCWF.  
EcoP_01_load.R - Loads and processes study area spatial data using the pre-processed Provincial data and any study area specific data.  
Field_01_load.R - Loads study area wetland field data, requires manual oversite due to the variability in sampling wetlands, and 
may include further processing.  

#MkWetlands Scripts:   
MkWetlands_01_load_new.R	- generates a new wetland inventory based on Fresh Water Atlas and VRI;  
MkWetlands_02_clean.R - cleans existing or new wetlands and standradizes their content and structure.  
Wetland attributes include:  
WTLND_ID - combination of study area abbreviation and an unique identifier set when a wetland inventory is first generated, WTLND_IDs can be added for new wetlands not in the original set;
Sampled - 0 or 1 denoting if a wetland has been sampled;  
YearSampled - year of sample;  
SampleType - legacy attribute used to track type of sample, such as PEM, etc;  
wet_id - a numeric wetland identifier, 1 to number of wetlands, if a wetland is dropped scripts will regenerate this attribute, where as WTLND_ID never changes;  
area_Ha - wetland area in hectares;  
Observed_Landcover* - land cover observed in the field for a wetland;  
Observed_Disturbance* - disturbane observed in the field;  
&LargeWetland - Is a wetland over 1,000ha Yes or No;  
*due to inconsistencies of observed types typically fuzzy matching is required to standardize - see Field_02_clean_SIM.R for an example of the processing required.  

#Speciality Scripts:   
If a study area's field wetlands or inventory includes extensive specialized processing, new scripts 
should be developed. Tasks can include: repairing wetland geometry, modifying field names associated with a 
wetland to follow the standard convention expected by the scripts, integrating novel spatial data sets, 
using wetlands generated by Predictive Ecosystem Mapping initiatives, etc

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
